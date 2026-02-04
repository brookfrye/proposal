library(data.table)
library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
library(scales)
library(gt)
library(webshot2)

# -----------------------------
# Load data and compute medians 
# -----------------------------
dt25 <- fread(file.path(data_dir, "fy25_sal_active.csv"))

medians <- dt25[
  sys == "city",
  .(
    median_salary = median(as.numeric(base_salary), na.rm = TRUE),
    n = .N
  ),
  by = .(sys, grp, level)
]

# 
meds_in <- copy(medians)[, .(
  display_level = ifelse(
    tolower(trimws(level)) == "senior",
    paste("Senior", tools::toTitleCase(grp)),
    tools::toTitleCase(grp)
  ),
  median_salary
)]

# split base vs senior
base_dt   <- meds_in[!grepl("^Senior\\b", display_level)]
senior_dt <- meds_in[ grepl("^Senior\\b", display_level)]

# -----------------------------
# Make long gt step tables
# -----------------------------
make_salary_steps_gt_long <- function(
    dt,
    years = 10,
    start_bump = 1.10,
    annual_inc = 1.015
) {
  stopifnot(is.data.table(dt))
  stopifnot(all(c("display_level", "median_salary") %in% names(dt)))
  
  x <- copy(dt)[, median_salary := as.numeric(median_salary)]
  x <- x[!is.na(median_salary)]
  
  long <- x[, {
    step <- 0:years
    start_salary <- median_salary * start_bump
    salary <- start_salary * (annual_inc ^ step)
    .(Step = step, Salary = dollar(salary, accuracy = 1))
  }, by = .(display_level)]
  
  tabs <- lapply(unique(long$display_level), function(dl) {
    sub <- long[display_level == dl, .(Step, Salary)]
    
    gt(sub) |>
      tab_header(
        title = md(paste0("**", dl, "**")),
        subtitle = md(paste0(
          "Annual increase = ", round((annual_inc - 1) * 100, 1), "%; ",
          years, " steps"
        ))
      ) |>
      cols_align(align = "center", columns = Step) |>
      cols_align(align = "right",  columns = Salary) |>
      tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(18),
        heading.subtitle.font.size = px(12)
      )
  })
  
  names(tabs) <- unique(long$display_level)
  tabs
}

base_steps   <- make_salary_steps_gt_long(base_dt, years = 10)
senior_steps <- make_salary_steps_gt_long(senior_dt, years = 25)


