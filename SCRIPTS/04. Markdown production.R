#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________

# Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

# ACTION: set whether to re-download all datasets, even if already exists
redownload_all <- FALSE

# HERE package needed for dynamic pathfinding
library("here") 


#...............................................................................
#### Run Jobs report scripts ----
#...............................................................................

# Create paths as strings
source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))

# Data packages
source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

# Inputs such as borough codes in Nomis
source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))

# Run the subscripts necessary for markdown
source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
source(paste0(SUBSCRIPTS,"GLAE_paye_dataload",".r"))
source(paste0(SUBSCRIPTS,"GLAE_Lightcast_functions",".r"))

# Download the Lightcast data
source(here::here("SCRIPTS","02. Download data.r"))

# Produce the permanent markdown, if needed
# rmarkdown::render(here::here("SCRIPTS",paste0("03a. Permanent markdown.Rmd")),
#                   output_file = paste0("Understanding online job postings data - ", format(Sys.Date(),"%B %Y"), 
#                                       ".html"))

# Produce the quarterly job postings output markdown
rmarkdown::render(here::here("SCRIPTS",paste0("03b. Quarterly markdown.Rmd")),
                  output_file = paste0(HTML,"Quarterly job postings analyses - ", format(Sys.Date(),"%B %Y"), 
                                       ".html"))
