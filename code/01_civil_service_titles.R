# comparable city titles 
# civil service data - https://data.cityofnewyork.us/City-Government/NYC-Civil-Service-Titles/nzjr-3966/about_data
# we are using titles where job descriptions approximated our roles and through working relationships 
library(data.table)
library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
pol_codes <- c("1002A", "12626", "12627") # associate, admin, staff analysts
data_codes <- "21744" # city research 
couns_codes <- "30087" # agency, staff, counsel/attorney
web_codes <- "13632" # comp specialist 
# download civil service title ranges, barg unit, levels -------------------
csts <- fread("https://data.cityofnewyork.us/resource/nzjr-3966.csv?$limit=99999")
csts <- csts[!asg_lvl == '01']
csts[title %in% pol_codes, grp := "policy"]
csts[title %in% data_codes, grp := "data"]
csts[title %in% couns_codes, grp := "counsel"]
csts[title %in% web_codes, grp := "web"]
csts_comps <- csts[!is.na(grp), ]
csts_comps[, sys := "city"]
# council csts
csts_council <- csts[title %in% c("94381", "94435", "94451", "94453")]
csts_council[, sys := "council"]
csts_council[, grp := NULL]
csts_all <- rbind(csts_council, csts_comps, fill = TRUE)
csts_all[, title_description := trimws(gsub("\\(.*$", "", descr))]
# manually fix this 
csts_all[grep("senior legislative", descr, ignore.case = TRUE), title_description := "SENIOR LEGISLATIVE POLICY ANALYST"]
# remove management level in City Research Scientist
csts_all <- csts_all[!(title_description == "CITY RESEARCH SCIENTIST" & asg_lvl == "4B")]
View(csts_all)
fwrite(csts_all, file.path(data_dir, "civ_serv_data_for_titles.csv"))
