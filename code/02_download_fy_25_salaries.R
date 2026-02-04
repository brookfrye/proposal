library(data.table)
library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
# library(httr2)
# library(jsonlite)
csts <- fread(file.path(data_dir, "civ_serv_data_for_titles.csv"))
# look for titles - cleaned + standard CST 
titles <- c(unique(csts$descr), unique(csts$title_description))
titles <- unique(titles)
fy25 <- fread("https://data.cityofnewyork.us/resource/k397-673e.csv?$where=fiscal_year=2025&$limit=999999")
fy25 <- fy25[leave_status_as_of_june_30 == "ACTIVE", ]
fy25_sals <- fy25[title_description %in% titles, ]
sals <- merge(fy25_sals, csts, all.x = TRUE, allow.cartesian = TRUE)
fwrite(sals, file.path(data_dir, "fy25_bargun_comp_titles_salaries.csv"))

# # ---- Config ----
# BASE_URL <- "https://data.cityofnewyork.us/resource/k397-673e.json"
# APP_TOKEN <- Sys.getenv("socrata_token", unset = "")
# FY <- 2025L
# escape_soql_string <- function(x) gsub("'", "''", x, fixed = TRUE)
# titles_in <- paste0("'", escape_soql_string(titles), "'", collapse = ", ")
# where_clause <- sprintf(
#   "fiscal_year = %d AND title_description IN (%s)",
#   FY, titles_in
# )
# 
# select_clause <- paste(
#   c(
#     "fiscal_year",
#     "agency_name",
#     "title_description",
#     "last_name",
#     "first_name",
#     "mid_init",
#     "leave_status_as_of_june_30",
#     "base_salary"),
#   collapse = ","
# )
# # ---- Paged downloader ----
# soda_get_all <- function(base_url, where, select = NULL, page_size = 50000L, verbose = TRUE) {
#   out <- list()
#   offset <- 0L
#   i <- 1L
#   
#   repeat {
#     req <- request(base_url) %>% 
#       req_url_query(
#         "$where"  = where,
#         "$limit"  = page_size,
#         "$offset" = offset
#       )
#     
#     resp <- req %>% req_perform()
#     txt  <- resp %>% resp_body_string()
#     dat  <- fromJSON(txt, simplifyDataFrame = TRUE)
#     
#     # If nothing returned, stop
#     if (length(dat) == 0) break
#     
#     dt <- as.data.table(dat)
#     out[[i]] <- dt
#     
#     if (verbose) message(sprintf("page %d: n=%d (offset=%d)", i, nrow(dt), offset))
#     
#     # last page if fewer than requested
#     if (nrow(dt) < page_size) break
#     
#     offset <- offset + page_size
#     i <- i + 1L
#   }
#   
#   rbindlist(out, use.names = TRUE, fill = TRUE)
# }
# # run fun
# pay_2025_titles <- soda_get_all(
#   base_url = BASE_URL,
#   where    = where_clause,
#   select   = select_clause,
#   page_size = 50000L,
#   verbose  = TRUE
# )
# fy25sals <- merge(pay_2025_titles, csts, by = "title_description", allow.cartesian = TRUE)
# unique(fy25sals$title_description)
# fwrite(fy25sals, "data/fy25_salary_comp_titles.csv")


