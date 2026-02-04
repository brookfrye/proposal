# ============================================================
# NYC Jobs comparables (policy / counsel / web / data)
# - Pull postings by civil_service_title from kpav-sd4t
# - Remove counsel postings that look like litigation
# - Create simplified text fields for description / min quals / preferred
# ============================================================

library(data.table)
library(httr2)
library(jsonlite)
library(rprojroot)

# ----------------------------
# Inputs
# ----------------------------
# "LEGISLATIVE POLICY ANALYST",
# "SENIOR LEGISLATIVE POLICY ANALYST",
# "LEGISLATIVE COUNSEL",
# "LEGISLATIVE PROGRAMMER/ANALYST"
titles_raw <- c(
  "ADMINISTRATIVE ATTORNEY",
  "ADMINISTRATIVE STAFF ANALYST",
  "STAFF ANALYST",
  "ASSOCIATE STAFF ANALYST",
  "COMPUTER SPECIALIST (SOFTWARE)",
  "CITY RESEARCH SCIENTIST",
  "AGENCY ATTORNEY",
  "AGENCY ATTORNEY DC37"
)

SODA_BASE <- "https://data.cityofnewyork.us/resource/kpav-sd4t.json"
SODA_APP_TOKEN <- Sys.getenv("socrata_token", unset = "")  # optional

root_dir <- find_root(has_file("proposal.Rproj"))
out_dir <- file.path(root_dir, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------
# Helpers (small + readable)
# ----------------------------
clean_title <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("^\\*+", "", x)        # drop leading asterisks if any
  x <- gsub("\\s+", " ", x)
  x
}

clean_text <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- gsub("\r", "\n", x, fixed = TRUE)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\\n{3,}", "\n\n", x)
  trimws(x)
}

# Turn paragraphs + bullets into a short bullet list
# Keeps only "action-y" lines and drops boilerplate/admin text.
summarize_to_bullets <- function(x, max_items = 10L, max_chars = 420L, bullet = "●") {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  
  # normalize common bullet glyphs / dashes into newlines
  x <- gsub("[•·▪◦‣]", "\n", x, perl = TRUE)
  x <- gsub("(?m)^\\s*[-–—]\\s+", "", x, perl = TRUE)
  
  # split into candidate lines
  parts <- unlist(strsplit(x, "\n+"), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  
  # drop obvious boilerplate/admin lines
  drop_re <- paste0(
    "(?i)^(how\\s+to\\s+apply|to\\s+apply|work\\s+location|hours|salary|benefits|",
    "equal\\s+opportunity|reasonable\\s+accommodation|posting\\s+period|job\\s+id|",
    "authorization\\s+to\\s+work|final\\s+appointment|note\\s*:)\\b"
  )
  parts <- parts[!grepl(drop_re, parts, perl = TRUE)]
  
  # keep lines that look like responsibilities / requirements
  keep_re <- paste0(
    "(?i)\\b(",
    paste(c(
      "draft", "review", "prepare", "analy", "research", "advise", "assist", "support",
      "coordinate", "manage", "monitor", "develop", "design", "build", "maintain",
      "implement", "document", "test", "troubleshoot", "present", "liais", "collabor",
      "data", "policy", "legal", "hearing"
    ), collapse = "|"),
    ")\\w*\\b"
  )
  parts <- parts[grepl(keep_re, parts, perl = TRUE)]
  
  # trim long lines + dedupe
  parts <- parts[nchar(parts) <= max_chars]
  parts <- parts[!duplicated(parts)]
  
  if (!length(parts)) return("")
  parts <- head(parts, max_items)
  
  paste0(bullet, " ", parts, collapse = "\n")
}

# quick “one-line-ish” summary (good for min quals / preferred)
summarize_compact <- function(x, max_chars = 600L) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  x <- gsub("\n+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (nchar(x) > max_chars) x <- paste0(substr(x, 1, max_chars), "…")
  x
}

# classify into your Council buckets
role_bucket <- function(civil_service_title, job_category, business_title) {
  t <- toupper(paste(civil_service_title, job_category, business_title, sep = " | "))
  
  if (grepl("RESEARCH SCIENTIST|DATA|ANALYTICS|STATISTIC|QUANT", t)) return("data")
  if (grepl("COMPUTER|SOFTWARE|DEVELOPER|PROGRAMMER|APPLICATION|SYSTEMS|IT", t)) return("web")
  if (grepl("ATTORNEY|COUNSEL|LEGAL", t)) return("counsel")
  return("policy")
}

# litigation filter for counsel
is_litigationy <- function(text) {
  # vectorized: returns TRUE/FALSE per element
  text <- ifelse(is.na(text), "", text)
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)
  text <- gsub("\u00a0", " ", text, fixed = TRUE)
  text <- gsub("[ \t]+", " ", text)
  text <- gsub("\\n{3,}", "\n\n", text)
  text <- trimws(text)
  
  txt <- tolower(text)
  
  grepl(
    "(litigat|prosecut|trial|court|hearings?\\b|argu(e|ing)\\b|tribunal|disciplin(ar|e)|civil\\s+service\\s+law\\s*(72|75))",
    txt
  )
}

# ----------------------------
# Socrata fetch
# ----------------------------
soda_get <- function(query, timeout_sec = 60) {
  req <- request(SODA_BASE) %>% 
    req_timeout(timeout_sec) %>% 
    req_headers(`Accept` = "application/json")
  
  if (nzchar(SODA_APP_TOKEN)) {
    req <- req %>%  req_headers(`X-App-Token` = SODA_APP_TOKEN)
  }
  
  resp <- req %>%  req_url_query(!!!query) %>%  req_perform()
  jsonlite::fromJSON(resp_body_string(resp), flatten = TRUE)
}

fetch_title <- function(title_clean) {
  where <- sprintf("civil_service_title = '%s'", gsub("'", "''", title_clean))
  
  select_cols <- c(
    "job_id", "agency",
    "business_title", "civil_service_title",
    "title_code_no", "level", "career_level",
    "job_category",
    "salary_range_from", "salary_range_to",
    "job_description",
    "minimum_qual_requirements",
    "preferred_skills"
  )
  
  x <- soda_get(list(
    `$select` = paste(select_cols, collapse = ","),
    `$where`  = where
  ))
  
  if (length(x) == 0) return(data.table())
  as.data.table(x)
}

# ----------------------------
# Run
# ----------------------------
titles_clean <- vapply(titles_raw, clean_title, FUN.VALUE = character(1))
jobs_all <- rbindlist(lapply(titles_clean, fetch_title), fill = TRUE)

# normalize text fields
for (col in c("job_description", "minimum_qual_requirements", "preferred_skills")) {
  jobs_all[, (col) := vapply(get(col), clean_text, FUN.VALUE = character(1))]
}

# add your bucket + simplified fields
jobs_all[, bucket := mapply(
  role_bucket,
  civil_service_title,
  job_category,
  business_title,
  SIMPLIFY = TRUE
)]

jobs_all[, desc_bullets := vapply(job_description, summarize_to_bullets, FUN.VALUE = character(1))]
jobs_all[, min_quals_short := vapply(minimum_qual_requirements, summarize_compact, FUN.VALUE = character(1))]
jobs_all[, preferred_short := vapply(preferred_skills, summarize_compact, FUN.VALUE = character(1))]
jobs_all <- jobs_all[career_level == "Experienced (non-manager)"]
jobs_all <- jobs_all[!grep("director|chief|finan|fisc|budget|capital|deputy|litigation|general counsel|prosecuting|revenue", business_title, ignore.case = TRUE), ]

# remove litigation counsel
# - apply only to counsel bucket (keeps policy/web/data untouched)
jobs_all[, drop_litigation := FALSE]
jobs_all[bucket == "counsel", drop_litigation := is_litigationy(paste(job_description, preferred_skills, desc_bullets, sep = "\n"))]
jobs_keep <- jobs_all[!(bucket == "counsel" & drop_litigation == TRUE)]

# ----------------------------
# Write outputs
# ----------------------------
# fwrite(jobs_all,  file.path(out_dir, "comp_city_job_postings_raw.csv"))
fwrite(jobs_keep, file.path(out_dir, "comp_city_job_postings_clean.csv"))

# handy separate counsel file
fwrite(
  jobs_keep[bucket == "counsel"],
  file.path(out_dir, "comp_city_counsel.csv")
)
