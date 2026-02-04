# Compare attrition + median departed salary + median departed experience:
# City Council Bargaining Unit vs Citywide (EXCLUDING City Council), by fiscal year
# Attrition definition: ACTIVE in FY t but NOT ACTIVE in FY t+1
# Departed salary/experience measured in FY t (the last ACTIVE year)
library(rprojroot)
root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
source(file.path(root_dir, "code/00_depends.R"))
library(data.table)
library(ggplot2)
library(scales)

# =========================
# LOAD DATA
# =========================
sals <- fread(file.path(data_dir, "salary_data_2018-2025_attrition.csv"))

sals[, fy_start := as.IDate(sprintf("%d-07-01", fiscal_year - 1L))]
sals[, agency_start_date := as.IDate(agency_start_date)]
sals[, years_exp := as.numeric((fy_start - agency_start_date) / 365.25)]

keep_cols <- c(
  "person_id","fiscal_year","agency_name","title_description",
  "leave_status_as_of_june_30","base_salary","years_exp"
)

sals0 <- unique(sals[, ..keep_cols], by = keep_cols)

years_all <- sort(unique(sals0$fiscal_year))
years_all <- years_all[!is.na(years_all)]


bargaining_unit_titles <- c(
  "SENIOR LEGISLATIVE POLICY ANALYST",
  "LEGISLATIVE POLICY ANALYST",
  "LEGISLATIVE COUNSEL",
  "LEGISLATIVE PROGRAMMER/ANALYST"
)

cc_bu <- sals0[
  agency_name == "CITY COUNCIL" &
    title_description %in% bargaining_unit_titles
]

# Citywide comparison EXCLUDING City Council
citywide <- sals0[agency_name != "CITY COUNCIL"]

# =========================
# ATTRITION FUNCTION (YoY) + MEDIAN SALARY/EXP OF DEPARTURES
# =========================
attrition_yoy_with_stats <- function(dt, years = NULL) {
  d <- copy(dt)
  
  yrs <- sort(unique(d$fiscal_year))
  yrs <- yrs[!is.na(yrs)]
  if (!is.null(years)) yrs <- intersect(yrs, years)
  yrs <- sort(yrs)
  
  if (length(yrs) < 2) stop("Need at least 2 fiscal years in dt.")
  
  out <- vector("list", length(yrs) - 1L)
  
  for (i in 1:(length(yrs) - 1L)) {
    y_from <- yrs[i]
    y_to   <- yrs[i + 1L]
    
    active_from <- d[fiscal_year == y_from & leave_status_as_of_june_30 == "ACTIVE"]
    active_to_ids <- d[fiscal_year == y_to & leave_status_as_of_june_30 == "ACTIVE", unique(person_id)]
    
    departed <- active_from[!person_id %in% active_to_ids]
    
    n_start <- uniqueN(active_from$person_id)
    n_dep   <- uniqueN(departed$person_id)
    
    out[[i]] <- data.table(
      year_from = y_from,
      year_to   = y_to,
      active_start = n_start,
      departed_n   = n_dep,
      attrition_rate = if (n_start == 0L) NA_real_ else n_dep / n_start,
      median_departed_salary = median(departed$base_salary, na.rm = TRUE),
      median_departed_exp    = median(departed$years_exp, na.rm = TRUE)
    )
  }
  
  rbindlist(out)
}

# Run for both series
cc_stats <- attrition_yoy_with_stats(
  cc_bu[, .(person_id, fiscal_year, leave_status_as_of_june_30, base_salary, years_exp)],
  years = years_all
)

city_stats <- attrition_yoy_with_stats(
  citywide[, .(person_id, fiscal_year, leave_status_as_of_june_30, base_salary, years_exp)],
  years = years_all
)

# =========================
# COMBINE RESULTS TABLE
# =========================
cmp <- merge(
  cc_stats[, .(
    year_to,
    cc_active_start = active_start,
    cc_departed = departed_n,
    cc_attrition = attrition_rate,
    cc_med_salary = median_departed_salary,
    cc_med_exp    = median_departed_exp
  )],
  city_stats[, .(
    year_to,
    city_active_start = active_start,
    city_departed = departed_n,
    city_attrition = attrition_rate,
    city_med_salary = median_departed_salary,
    city_med_exp    = median_departed_exp
  )],
  by = "year_to",
  all = TRUE
)

cmp[, `:=`(
  cc_attrition_pct   = 100 * cc_attrition,
  city_attrition_pct = 100 * city_attrition,
  attrition_gap_pp   = 100 * (cc_attrition - city_attrition),
  med_salary_gap     = cc_med_salary - city_med_salary,
  med_exp_gap        = cc_med_exp - city_med_exp
)]

setorder(cmp, year_to)

print(
  cmp[, .(
    Fiscal_Year_End = year_to,
    
    CC_BU_Active_Start = cc_active_start,
    CC_BU_Departed = cc_departed,
    CC_BU_Attrition_Pct = round(cc_attrition_pct, 1),
    CC_BU_Median_Departed_Salary = dollar(cc_med_salary),
    CC_BU_Median_Departed_Exp_Yrs = round(cc_med_exp, 1),
    
    Citywide_ExclCC_Active_Start = city_active_start,
    Citywide_ExclCC_Departed = city_departed,
    Citywide_ExclCC_Attrition_Pct = round(city_attrition_pct, 1),
    Citywide_ExclCC_Median_Departed_Salary = dollar(city_med_salary),
    Citywide_ExclCC_Median_Departed_Exp_Yrs = round(city_med_exp, 1),
    
    Attrition_Gap_pp = round(attrition_gap_pp, 1),
    Median_Salary_Gap = dollar(med_salary_gap),
    Median_Exp_Gap_Yrs = round(med_exp_gap, 1)
  )]
)

# =========================
# PLOT 1: ATTRITION RATE (LINE)
# =========================
plot_attr <- rbindlist(list(
  cmp[, .(year_to, series = "City Council BU", value = cc_attrition_pct)],
  cmp[, .(year_to, series = "Citywide (excl. City Council)", value = city_attrition_pct)]
), use.names = TRUE, fill = TRUE)

p1 <- ggplot(plot_attr, aes(x = year_to, y = value, fill = series)) +
  # geom_line(linewidth = 1.2) +
  # geom_point(size = 2.6) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Attrition Rate (YoY): City Council Bargaining Unit vs Citywide",
    subtitle = "Attrition = ACTIVE in FY t but not ACTIVE in FY t+1",
    x = "Fiscal year (end)",
    y = "Attrition rate (%)",
    color = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")



# =========================
# PLOT 2: MEDIAN DEPARTED SALARY (BAR)
# =========================
plot_med <- rbindlist(list(
  cmp[, .(year_to, series = "City Council BU", med_salary = cc_med_salary)],
  cmp[, .(year_to, series = "Citywide (excl. City Council)", med_salary = city_med_salary)]
), use.names = TRUE, fill = TRUE)

p2 <- ggplot(plot_med, aes(x = factor(year_to), y = med_salary, fill = series)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Median Base Salary of Employees Who Left (YoY)",
    subtitle = "Salary measured in the last ACTIVE fiscal year (FY t)",
    x = "Fiscal year (end)",
    y = "Median base salary of departed employees",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(p2)

# =========================
# PLOT 3: MEDIAN DEPARTED EXPERIENCE (BAR)
# =========================
plot_exp <- rbindlist(list(
  cmp[, .(year_to, series = "City Council BU", med_exp = cc_med_exp)],
  cmp[, .(year_to, series = "Citywide (excl. City Council)", med_exp = city_med_exp)]
), use.names = TRUE, fill = TRUE)

p3 <- ggplot(plot_exp, aes(x = factor(year_to), y = med_exp, fill = series)) +
  geom_col(position = "dodge") +
  labs(
    title = "Median Years of Experience of Employees Who Left (YoY)",
    subtitle = "Experience measured in the last ACTIVE fiscal year (FY t)",
    x = "Fiscal year (end)",
    y = "Median years of experience",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")



# Optional saves:
# ggsave("viz_attrition_rate_ccbu_vs_citywide.png", p1, width = 10, height = 6, dpi = 300)
# ggsave("viz_median_departed_salary_ccbu_vs_citywide.png", p2, width = 10, height = 6, dpi = 300)
# ggsave("viz_median_departed_experience_ccbu_vs_citywide.png", p3, width = 10, height = 6, dpi = 300)
