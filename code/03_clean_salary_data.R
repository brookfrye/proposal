library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
source(file.path(root_dir, "code/00_depends.R"))
sals <- fread(file.path(data_dir, "fy25_bargun_comp_titles_salaries.csv"))
sals[, full_name := paste(first_name, last_name)]
sals[, person_id := paste(full_name, agency_start_date, sep = "_")]
sals[, agency_start_date := as.IDate(agency_start_date)]
sals[, fy_start := as.IDate(sprintf("%d-07-01", fiscal_year - 1L))]
sals[, years_exp := as.numeric(fy_start - agency_start_date) / 365.25]
sals <- sals[years_exp <= 10, ]
# create grps from depends defs
sals[full_name %in% web_staff, grp := "web"]
sals[full_name %in% data_staff, grp := "data"]
sals[title_description %in% policy, grp := "policy"]
sals[title_description %in% counsel, grp := "counsel"]
sals[title_description == data, grp := "data"]
sals[title_description %in% web, grp := "web"]
sals[agency_name == "CITY COUNCIL", sys := "council"]
sals[is.na(sys), sys := "city"]
sals_wdt <- sals[, .(full_name, person_id, agency_name, agency_start_date, fiscal_year,
                  leave_status_as_of_june_30, title_description, years_exp,
                   grp, sys, title, asg_lvl,  base_salary, min_rate, max_rate, union_descr)]
sals_sub <- sals_wdt[base_salary > min_rate, ]
sals_sub <- sals_sub[base_salary <= max_rate, ]
sals_sub[, mid := (min_rate + max_rate) / 2]
sals_sub[, dist_to_mid := abs(base_salary - mid)]
setorder(sals_sub, person_id, dist_to_mid, asg_lvl)
sals_best <- sals_sub[, .SD[1L], by = .(person_id)]
sals_best[, log_salary := log(base_salary)]
sals_best[grep("senior legislative", title_description, ignore.case = TRUE), level := "senior"]
sals_best[asg_lvl == "02", level := "junior"]
sals_best[asg_lvl == "03", level := "senior"]
sals_best[asg_lvl == "01", level := "junior"]
sals_best[asg_lvl == "04", level := "senior"]
sals_best[asg_lvl == "4A", level := "senior"]
sals_best[asg_lvl == "00" & title_description == "ADMINISTRATIVE STAFF ANALYST", level := "senior"]
sals_best[asg_lvl == "00" & title_description == "ASSOCIATE STAFF ANALYST", level := "senior"]
sals_best[grp == "policy" & is.na(level), level := "junior"]
sals_best[grp == "data" & base_salary >= 97000 & sys == "council", level := "senior"]
sals_best[grp == "counsel" & base_salary >= 100000 & sys == "council", level := "senior"]
sals_best[grp == "web" & base_salary >= 100000 & sys == "council", level := "senior"]
sals_best[is.na(level) & sys == "council", level := "junior"]
sals_final <- sals_best[!(sys == "council" & base_salary >= 135000), ]
sals_final[, team_level := paste(level, grp, sep = " ")]
sals_final <- sals_final[!level == "", ]

fwrite(sals_final, file.path(data_dir, "fy25_sal_active.csv"))
