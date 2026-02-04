library(data.table)
library(rprojroot)
library(jsonlite)

root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Config ----
DATASET_ID <- "k397-673e"
BASE_URL <- sprintf("https://data.cityofnewyork.us/resource/%s.csv", DATASET_ID)
COLUMNS_URL <- sprintf("https://data.cityofnewyork.us/api/views/%s/columns.json", DATASET_ID)
APP_TOKEN <- Sys.getenv("SOCRATA_APP_TOKEN", unset = "")

FY_START <- 2018L
FY_END <- 2025L
AGENCY_EXACT <- "CITY COUNCIL"
ACTIVE_LABEL <- "ACTIVE"

PAGE_SIZE <- 50000L
MAX_PAGES <- 200L

# ---- Helpers ----
get_columns <- function(url) {
  cols <- fromJSON(url, simplifyDataFrame = TRUE)
  as.data.table(cols)
}

field_from_name <- function(cols, patterns, required = TRUE) {
  hit <- rep(FALSE, nrow(cols))
  for (p in patterns) {
    hit <- hit | grepl(p, cols$name, ignore.case = TRUE)
  }
  idx <- which(hit)
  if (length(idx) == 0L) {
    if (required) stop(sprintf("No column matched patterns: %s", paste(patterns, collapse = ", ")))
    return(NA_character_)
  }
  cols$fieldName[idx[1]]
}

make_url <- function(base, select_cols, where_clause, limit, offset, app_token = "") {
  q <- list(
    "$select" = paste(select_cols, collapse = ","),
    "$where" = where_clause,
    "$limit" = limit,
    "$offset" = offset
  )
  if (nzchar(app_token)) q[["$$app_token"]] <- app_token
  # Manual query build to avoid dependencies
  query <- paste(
    vapply(names(q), function(k) {
      sprintf("%s=%s", k, utils::URLencode(as.character(q[[k]]), reserved = TRUE))
    }, character(1)),
    collapse = "&"
  )
  paste0(base, "?", query)
}

soda_get_all <- function(base, select_cols, where_clause, page_size = 50000L,
                         max_pages = 200L, app_token = "", verbose = TRUE) {
  out <- list()
  offset <- 0L
  for (i in seq_len(max_pages)) {
    url <- make_url(base, select_cols, where_clause, page_size, offset, app_token)
    dt <- tryCatch(
      fread(url, showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(dt) || nrow(dt) == 0L) break
    out[[i]] <- dt
    if (verbose) message(sprintf("page %d: n=%d (offset=%d)", i, nrow(dt), offset))
    if (nrow(dt) < page_size) break
    offset <- offset + page_size
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

# ---- Column discovery ----
cols <- get_columns(COLUMNS_URL)

col_fy <- field_from_name(cols, c("^fiscal year$", "fiscal year"))
col_agency <- field_from_name(cols, c("^agency name$", "agency"))
col_title <- field_from_name(cols, c("title description", "job title", "title"))
col_status <- field_from_name(cols, c("leave status", "status"), required = FALSE)
col_base <- field_from_name(cols, c("base salary", "base"))
col_bu <- field_from_name(cols, c("bargaining unit", "bargain"), required = FALSE)

select_cols <- unique(na.omit(c(col_fy, col_agency, col_title, col_status, col_base, col_bu)))

# ---- Download (prefer agency filter, fallback if empty) ----
where_base <- sprintf("%s >= %d AND %s <= %d", col_fy, FY_START, col_fy, FY_END)
where_with_agency <- sprintf("%s AND upper(%s) = '%s'", where_base, col_agency, toupper(AGENCY_EXACT))

pay <- soda_get_all(
  base = BASE_URL,
  select_cols = select_cols,
  where_clause = where_with_agency,
  page_size = PAGE_SIZE,
  max_pages = MAX_PAGES,
  app_token = APP_TOKEN,
  verbose = TRUE
)

if (nrow(pay) == 0L) {
  message("No rows with exact agency match; retrying without agency filter and filtering locally.")
  pay <- soda_get_all(
    base = BASE_URL,
    select_cols = select_cols,
    where_clause = where_base,
    page_size = PAGE_SIZE,
    max_pages = MAX_PAGES,
    app_token = APP_TOKEN,
    verbose = TRUE
  )
}

# ---- Standardize columns ----
pay[, fiscal_year := as.integer(get(col_fy))]
pay[, agency_name := as.character(get(col_agency))]
pay[, title_description := as.character(get(col_title))]
if (!is.na(col_status)) pay[, status := as.character(get(col_status))]
pay[, base_salary := as.numeric(get(col_base))]

# If agency filter was skipped or too broad, filter locally
if (nrow(pay) > 0L) {
  pay <- pay[grepl("^CITY COUNCIL$", toupper(agency_name))]
}

# Active employees only
if ("status" %in% names(pay)) {
  pay <- pay[toupper(status) == ACTIVE_LABEL]
} else {
  message("No status column found; skipping active-only filter.")
}

# Bargaining unit handling
if (!is.na(col_bu)) {
  pay[, barg_unit := as.character(get(col_bu))]
} else {
  csts_path <- file.path(data_dir, "civ_serv_data_for_titles.csv")
  if (file.exists(csts_path)) {
    csts <- fread(csts_path, select = c("title_description", "barg_unit", "barg_descr"))
    pay <- merge(pay, csts, by = "title_description", all.x = TRUE)
  } else {
    message("No bargaining unit column and civ_serv_data_for_titles.csv missing; bargaining unit comparisons may be incomplete.")
    pay[, barg_unit := NA_character_]
  }
}

# Manager definition
pay[, title_upper := toupper(title_description)]
pay[, is_manager := grepl("(ASSISTANT\\s+DEPUTY\\s+DIRECTOR|ASST\\.?\\s+DEPUTY\\s+DIRECTOR|DEPUTY\\s+DIRECTOR)", title_upper)]

# Bargaining unit flag
pay[, is_bu := !is.na(barg_unit) & barg_unit != "" & barg_unit != "0"]

# Grouping: manager vs bargaining unit (non-overlapping)
pay[, group := fifelse(is_manager, "Manager",
                       fifelse(is_bu, "Bargaining unit", NA_character_))]

pay <- pay[!is.na(group)]

# ---- Outputs ----
raw_path <- file.path(data_dir, "city_council_payroll_2018_2025.csv")
fwrite(pay, raw_path)

by_year <- pay[, .(
  n = .N,
  median_base_salary = median(base_salary, na.rm = TRUE)
), by = .(fiscal_year, group)]

by_year_path <- file.path(data_dir, "city_council_bu_vs_manager_median_by_year.csv")
fwrite(by_year, by_year_path)

by_title_year <- pay[, .(
  n = .N,
  median_base_salary = median(base_salary, na.rm = TRUE)
), by = .(fiscal_year, group, title_description)]

by_title_year_path <- file.path(data_dir, "city_council_bu_vs_manager_median_by_title_year.csv")
fwrite(by_title_year, by_title_year_path)

message("Wrote:")
message(raw_path)
message(by_year_path)
message(by_title_year_path)
