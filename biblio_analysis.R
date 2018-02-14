library(tidyverse)
library(igraph)
library(gridExtra)

sta_pubs <- read_csv2("output/sta_pubs_with_groups.csv")

# removing people who started in 2017
sta_pubs <- sta_pubs %>% 
  filter(!(author %in% c("gebbie", "low", "mavuso", "msemburi", "ngwenya", "pienaar", "henry")))

sta_pubs$year <- factor(sta_pubs$year, levels = 2014:2017)

sta_pubs$rgroup <- factor(sta_pubs$rgroup, levels = c("b", "des", "e", "edu", "f", "geo", "msim",
                                                      "or", "oth"),
                          labels = c("biostats", "design", "ecostats", "education", "financial", "geostats", "mathmod", "OR", "other"))

sta_pubs$rgroup2 <- fct_collapse(sta_pubs$rgroup, 
                                 bio = "biostats", 
                                 eco = "ecostats",
                                 fin = "financial",
                                 OR = "OR",
                                 sim = "mathmod",
                                 other = c("design", "education", "geostats", "other"))
sta_pubs$rgroup2 <- factor(sta_pubs$rgroup2, levels = c("bio", "eco", "fin", "OR", "sim", "other"))

sta_pubs$role <- factor(sta_pubs$role, levels = c("p", "ap", "sl", "l", "pdoc", "con", "ep"),
                        labels = c("Prof", "A/Prof", "S.Lecturer", "Lecturer", "Postdoc", "Consultant", "Emer.Prof"))

# hack to reorder the staff names in order of level (prof)
ordered_staff <- sta_pubs %>% arrange(role, author) %>% 
  select(author) %>% unique() %>% unlist() %>% as.character()

sta_pubs$author <- factor(sta_pubs$author, levels = ordered_staff)

# summaries

# by year
all_by_year <- sta_pubs %>% 
  filter(!is.na(year)) %>%
  group_by(year) %>% 
  summarize(unique = n_distinct(title), total = n() - unique) %>%
  gather(unique, total, key = "papers", value = "n") %>%
  ggplot(aes(x = year, y = n, fill = papers)) + geom_bar(stat = "identity") +
  theme_bw(base_size=24) + 
  xlab("Year") + ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20),
        legend.title=element_blank())

# by subject
all_by_group <- sta_pubs %>% 
  filter(!is.na(year)) %>%
  group_by(rgroup2) %>% 
  summarize(unique = n_distinct(title), total = n() - unique) %>%
  gather(unique, total, key = "papers", value = "n") %>%
  ggplot(aes(x = rgroup2, y = n, fill = papers)) + geom_bar(stat = "identity") +
  theme_bw(base_size=24) + 
  xlab("Research group") + ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20),
        legend.title=element_blank())

ggsave("results/totalpapers.png", arrangeGrob(all_by_year, all_by_group, ncol = 2), 
       width = 14, height = 7, dpi = 300)

# by subject and year
group_by_year <- sta_pubs %>% 
  mutate(year = factor(sta_pubs$year, levels = 2017:2014)) %>%
  filter(!is.na(year)) %>%
  group_by(rgroup2, year) %>% 
  summarize(total = n()) %>%
  ggplot(aes(x = rgroup2, y = total, fill = year)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "BrBG", direction = -1) +
  theme_bw(base_size=24) + 
  xlab("Research group") + ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "bottom", legend.text=element_text(size=20),
        legend.title=element_blank())

ggsave("results/group_by_year.png", group_by_year, width = 9, height = 5, dpi = 300)

# by subject and role
group_by_role <- sta_pubs %>% 
  filter(!is.na(rgroup2)) %>%
  group_by(rgroup2, role) %>% 
  summarize(total = n()) %>%
  ungroup() %>%
  ggplot(aes(x = rgroup2, y = total, fill = role)) + geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size=24) + 
  xlab("Research group") + ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20)) +
  theme(legend.position = "right", legend.text=element_text(size=20),
        legend.title=element_blank())
ggsave("results/group_by_role.png", group_by_role, width = 9, height = 5, dpi = 300)

# per person, fill by role
person_by_role <- sta_pubs %>% 
  group_by(author, year, role) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  complete(nesting(author, role), year, fill = list(count = 0)) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = reorder(author, role), y = count, fill = role)) + geom_col() + coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size=24) + 
  ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank()) +
  theme(legend.position = "right", legend.text=element_text(size=20),
        legend.title=element_blank()) 
ggsave("results/person_by_role.png", person_by_role, width = 9, height = 8, dpi = 300)

# per person per year
person_by_year <- sta_pubs %>% 
  mutate(year = factor(sta_pubs$year, levels = 2017:2014)) %>%
  group_by(author, year, role) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  complete(nesting(author, role), year, fill = list(count = 0)) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = reorder(author, count), y = count, fill = year)) + geom_col() + coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  theme_bw(base_size=24) + 
  ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank()) +
  theme(legend.position = "right", legend.text=element_text(size=20),
        legend.title=element_blank())
ggsave("results/person_by_year.png", person_by_year, width = 9, height = 8, dpi = 300)

# per person per year
totals <- sta_pubs %>% 
  group_by(author) %>% 
  summarize(total = n()) 

person_by_group <- sta_pubs %>% 
  group_by(author, rgroup2) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  left_join(totals, by = "author") %>%
  ggplot(aes(x = reorder(author, total), y = count, fill = rgroup2)) + 
  geom_col() + coord_flip() +
  scale_fill_brewer(palette = "BrBG") +
  theme_bw(base_size=24) + 
  ylab("Number of papers") + 
  theme(axis.text=element_text(size=20), axis.title.y=element_blank()) +
  theme(legend.position = "right", legend.text=element_text(size=20),
        legend.title=element_blank())
ggsave("results/person_by_group.png", person_by_group, width = 9, height = 8, dpi = 300)

# publications per person per year
by_person <- sta_pubs %>% 
  group_by(rgroup2, author, year) %>% 
  summarize(count = n(), role = first(role)) %>% 
  arrange(role) %>% ungroup() %>% 
  complete(nesting(author,role), year, fill = list(count = 0)) %>%
  filter(!is.na(year))

# per person, coloured by role
by_person %>%
  arrange(role) %>%
  ggplot(aes(x = year, y = count, fill = role)) + 
  facet_wrap(~author) + geom_col() + theme_bw() + 
  theme(axis.title.x = element_blank(), legend.title=element_blank())

# collaboration graph
edgelist <- sta_pubs %>% 
  select(title, author) %>%
  filter(!is.na(title)) %>%
  mutate(title = str_replace_all(title, "[^[A-za-z]]", "")) 

g <- graph.edgelist(as.matrix(edgelist))
V(g)$type <- FALSE
V(g)$label.cex <- 2
V(g)$type[V(g)$name %in% edgelist$author] <- TRUE
author_projection <- bipartite.projection(g)$proj2
set.seed(100)
png("results/network.png", 800, 800)
plot(author_projection)
dev.off()
