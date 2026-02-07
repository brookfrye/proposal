library(data.table)
library(rprojroot)
library(ggplot2)
library(scales)

root_dir <- find_root(has_file("proposal.Rproj"))
data_dir <- file.path(root_dir, "data")
source(file.path(root_dir, "code", "00_depends.R"))
base_url <- "https://data.cityofnewyork.us/resource/k397-673e.csv"
# Pull all columns for City Council, FY >= 2018
query <- paste0(
  "?", 
  "$where=agency_name%20%3D%20%27CITY%20COUNCIL%27%20AND%20fiscal_year%20%3E%3D%202018",
  "&$limit=1000000"
)
pay <- fread(paste0(base_url, query))
pay[, full_name := tolower(paste(first_name, last_name, sep = " "))]
pay <- pay[!full_name %in% counsel_rm]

# ---- Active only ----
act <- pay[leave_status_as_of_june_30 == "ACTIVE"]
act2 <- act[title_description %in% c(bu_titles, dep_titles)]

# ---- Remove Deputy Directors not in leg ----
leg <- act2[!(title_description == "DEPUTY DIRECTOR" &
    !(full_name %chin% deps)), ]

leg[, group := ifelse(title_description %in% bu_titles, "bu", "manager")]

# ---- Short legend labels (row-level) ----
leg[, title_short := fcase(
  title_description == "LEGISLATIVE POLICY ANALYST",        "Policy Analyst",
  title_description == "SENIOR LEGISLATIVE POLICY ANALYST", "Sr Policy Analyst",
  title_description == "LEGISLATIVE PROGRAMMER/ANALYST",    "Programmer/Analyst",
  title_description == "LEGISLATIVE COUNSEL",               "Leg Counsel",
  title_description == "DEPUTY DIRECTOR",                   "Deputy Director",
  title_description == "ASSISTANT DIRECTOR OF LEGAL SERVICES","Asst Dep",
  default = title_description
)]

leg[, legend_label := fcase(
  group == "bu",      paste0("Union: ", title_short),
  group == "manager", paste0("Mgr: ",   title_short)
)]

# fwrite(leg, "data/active_city_council_2018-2025.csv")
# ---- Trend (median ONLY by year + title) ----
trend_core <- leg[
  , .(median_base_salary = median(base_salary, na.rm = TRUE), n = .N),
  by = .(fiscal_year, title_description)
][order(title_description, fiscal_year)]
View(trend_core)
View(leg)
# ---- Title lookup (bring back group + legend_label without affecting medians) ----
title_lu <- unique(leg[, .(title_description, group, legend_label)])

# Optional sanity check: each title should map to exactly one group/label
# stopifnot(title_lu[, .N, by = title_description][, all(N == 1)])

trend <- title_lu[trend_core, on = "title_description"]

# ---- Color palettes: BU blues, manager reds ----
labs_bu  <- unique(trend[group == "bu", legend_label])
labs_mgr <- unique(trend[group == "manager", legend_label])

pal_bu <- setNames(
  hcl(h = 210, c = 80, l = seq(35, 70, length.out = max(1, length(labs_bu)))),
  labs_bu
)
pal_mgr <- setNames(
  hcl(h = 10, c = 85, l = seq(35, 70, length.out = max(1, length(labs_mgr)))),
  labs_mgr
)

color_values <- c(pal_bu, pal_mgr)

# ---- Plot (leave as is) ----
p <- ggplot(trend, aes(fiscal_year, median_base_salary, color = legend_label, group = legend_label)) +
  geom_line(linewidth = 1.05, alpha = 0.9) +
  geom_point(size = 2.2, alpha = 0.95) +
  scale_color_manual(values = color_values, name = "Title") +
  scale_y_continuous(labels = dollar_format(accuracy = 1), expand = expansion(mult = c(0.02, 0.06))) +
  scale_x_continuous(breaks = sort(unique(trend$fiscal_year))) +
  labs(
    title = "City Council median base salary by title",
    subtitle = "Active employees, FY 2018-2025",
    x = "Fiscal year",
    y = "Median base salary"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8.8),
    legend.position = "right"
  )

p

# y_min <- 50000
# y_max <- ceiling(max(trend$median_base_salary, na.rm = TRUE) / 50000) * 50000
# p <- ggplot(trend, aes(fiscal_year, median_base_salary, color = legend_label, group = legend_label)) +
#   geom_line(linewidth = 1.05, alpha = 0.9) +
#   geom_point(size = 2.2, alpha = 0.95) +
#   scale_color_manual(values = color_values, name = "Title") +
#   scale_y_continuous(
#     labels = dollar_format(accuracy = 1),
#     limits = c(0, y_max),                 # <-- key change
#     breaks = seq(0, y_max, by = 25000),   # <-- clearer ticks
#     expand = expansion(mult = c(0, 0.03))
#   ) +
#   scale_x_continuous(breaks = sort(unique(trend$fiscal_year))) +
#   labs(
#     title = "City Council median base salary by title",
#     subtitle = "Active employees only; Deputy Directors limited to approved list",
#     x = "Fiscal year",
#     y = "Median base salary"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold"),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 8.8)
#   )
# p <- p +
#   scale_y_continuous(
#     limits = c(y_min, y_max),
#     breaks = seq(y_min, y_max, by = 50000),
#     labels = dollar_format(accuracy = 1),
#     expand = expansion(mult = c(0, 0.03))
#   )


p
ggsave(file.path(root_dir, "code", "images", "cc_median_salary_by_titles.png"),
       p, width = 12, height = 7, dpi = 300)


# counsel in leg
# "johari frasier"        "joshua kingsley"       "pauline syrnik"       
# "margaret lyford"       "austin malone"         "matthew hill"         
#       
# "declan mcpherson"      "kristoffer sartori"   
# "aminta kilawan"        
#  "deborah kerzhner"      "nicholas widzowski"    "alex paulenoff"       
#        "christopher pepe"     
#         "jayasri ganapathy"     "irene byhovsky"       
#                 "sara sucher"           "nicole cata"          
#  "elliot heisler"        "jeremy whiteman"       "julia goldsmith-pinkh"
#  "nadia jean-francois"   "sarah swaine"          "christina yellamaty"  
#         "rie ogasawara"        
#  "morganne barrett"      "rachel conte"          
#           "sierra townsend"      
#          "alex khan"            
