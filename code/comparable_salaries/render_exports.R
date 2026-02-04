# Export standalone and combined reports to HTML
# Run from your project root (final_prop):
# source("render_exports.R")

library(rmarkdown)

dir.create("reports", showWarnings = FALSE)

render("code/Counsel_formatted.Rmd", output_file = "Counsel.html", output_dir = "reports", quiet = TRUE)
render("code/Data_formatted.Rmd",    output_file = "Data.html",    output_dir = "reports", quiet = TRUE)
render("code/Policy_formatted.Rmd",  output_file = "Policy.html",  output_dir = "reports", quiet = TRUE)
render("code/Web_formatted.Rmd",     output_file = "Web.html",     output_dir = "reports", quiet = TRUE)

# Combined single-file export (uses *_body.Rmd includes)
render("code/Combined_formatted.Rmd", output_file = "Combined.html", output_dir = "reports", quiet = TRUE)

