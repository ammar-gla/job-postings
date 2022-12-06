
  #################################################################
  ###  Run scripts
  #################################################################
  
  # Needed to orientate the script into folder structure
  library("here")
  
  # Run the data and markdown scripts. Today's date is used by default to ensure version control.
  source(here::here("SCRIPTS","01. Functions and init.r"))
  source(here::here("SCRIPTS","02. Download data.r"))
  
  # rmarkdown::render(here::here("SCRIPTS",paste0("03a. Permanent markdown.Rmd")),
  #                   output_file = paste0("Understanding online job postings data - ", format(Sys.Date(),"%B %Y"), 
  #                                       ".html"))
  # 
  rmarkdown::render(here::here("SCRIPTS",paste0("03b. Quarterly markdown.Rmd")),
                    output_file = paste0(HTML,"Quarterly job postings analyses - ", format(Sys.Date(),"%B %Y"), 
                                         ".html"))
  