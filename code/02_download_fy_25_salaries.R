library(data.table)
library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
# library(httr2)
# library(jsonlite)
# Read in civil service titles 
csts <- fread(file.path(data_dir, "civ_serv_data_for_titles.csv"))
# look for titles - cleaned + standard CST 
titles <- c(unique(csts$descr), unique(csts$title_description))
titles <- unique(titles)
fy25 <- fread("https://data.cityofnewyork.us/resource/k397-673e.csv?$where=fiscal_year=2025&$limit=999999")
fy25 <- fy25[leave_status_as_of_june_30 == "ACTIVE", ]
fy25_sals <- fy25[title_description %in% titles, ]
# merge with civil service title info - min max sals, union, etc
sals <- merge(fy25_sals, csts, all.x = TRUE, allow.cartesian = TRUE)
# check 
unique(sals$title_description)
fwrite(sals, file.path(data_dir, "fy25_bargun_comp_titles_salaries.csv"))

