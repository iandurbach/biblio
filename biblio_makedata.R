library(readxl)
library(tidyverse)
library(tidytext)
library(rvest)

x1 <- read_excel("pubs/pubs2014.xls")
x2 <- read_excel("pubs/pubs2015.xls")
x3 <- read_excel("pubs/pubs2016.xls")
x4 <- read_excel("pubs/pubs2017.xls")

x <- rbind.data.frame(x1,x2,x3,x4)
rm(x1,x2,x3,x4)

# a few authors had stars in front of them, remove
x <- x %>% mutate(authors = str_replace_all(authors, "\\*", ""))
  
# hacky way of extracting surnames
surname_reg <- "[^A-Za-z[\\-]*[a-z]$]"
tidyx <- unnest_tokens(x, author, authors, token = "regex", pattern = surname_reg)

# remove any rows where "author" is <3 or Sebnem
tidyx <- tidyx %>% filter((author == "er") | (nchar(author) > 3))

# read in the staff and postdocs from the webpage
staff <- read_html("http://www.stats.uct.ac.za/stats/people/academic")
elements <- html_nodes(x = staff, css = "#block-uct-domain-menu-tools-uct-sidebar-menu a")
staff <- html_text(elements, trim = TRUE) 

# remove some junk
manual_remove <- c("Overview", "Academic Staff", "Administrative Staff", "Postgraduate Students", "Postdoctoral Fellows")
staff <- staff[!(staff %in% manual_remove)]

# manually add staff not on the website
staff <- c(staff, "dunne", "ngwenya", "msemburi", "mavuso", "gebbie", "stray",
           "kassanjee", "hardy", "pienaar")

#### here you have to manually add some data on staff! 
staff # print out and look
role <- c("ap", "ap", "p", "p", "l", "sl", "ap", "sl", "l", "sl","sl", "ep", "sl",
           "l", "sl", "sl", "ap", "sl", "ep", "ap", "sl", "l", "ap", "l", "l", "l", "ap",
           "l", "con", "con", "l")
start <- 2013 + c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,3,3,3,3,0,0,0,3)
end <- 2017 + c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,0,-2,0,0,0,0,-1,0,0,0)

staff_list <- data.frame(name = staff, role = role, yrs_empl = end - start)

postdocs <- read_html("http://www.stats.uct.ac.za/stats/people/postdoc_fellows/")
elements <- html_nodes(x = postdocs, css = "#block-uct-domain-menu-tools-uct-sidebar-menu a")
postdocs <- html_text(elements, trim = TRUE) 
postdocs <- postdocs[!(postdocs %in% manual_remove)]

# manually add postdocs not on the website
postdocs <- c(postdocs, c("visser", "henry", "gridley"))
role <- rep("pdoc", length(postdocs))
yrs_empl <- c(2,2,2,2,2,2,2)

pdoc_list <- data.frame(name = postdocs, role = role, yrs_empl = yrs_empl)

staff_list <- rbind(staff_list, pdoc_list)

# assumes last word is the surname
staff_list <- staff_list %>% mutate(name = str_to_lower(word(name, -1)))

# merge 
sta_pubs <- right_join(tidyx, staff_list, by = c("author" = "name")) %>% arrange(year)

# remove duplicates (where title and author are same in two or more rows)
sta_pubs <- sta_pubs[!duplicated(cbind(sta_pubs$title, sta_pubs$author)), ]

# save results
write.csv2(sta_pubs, "output/sta_pubs.csv")

# I now go and manually classify each paper into a research area, and save that file as 
# "sta_pubs_with_groups"; that file gets read in at the start of "biblio_analysis.r"

