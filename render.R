comp_name <- 'ai'

# Sifan, change company_name
rmarkdown::render(
  "CV/CV_Sifan.Rmd", 
  params = list(col = comp_name),
  output_file = paste0("Report-", comp_name, "-",".pdf"), 
)
