
#_______________________________________________________________________________
#### Load packages and set paths ####
#_______________________________________________________________________________

library("here") # To set project folder dynamically
library("remotes") # Makes it possible to load github package where necesary
library("knitr") # Needed to knit markdown
library("tidyverse") # Whole tidyverse package
library("lubridate") # Tideyverse date manipulations
library("scales") # For scales in ggplots
library("ggplot2") # Used for making main charts
library("ggthemes") # Additional themes for ggplot [not sure if used]
library("nomisr") # Downloading data from Nomis
library("devtools") # Allows downloading ggla packages
library("gglaplot") # GLA plotting functions
library("data.table") # Data table utility
library("janitor") # Cleaning up date files
library("ggrepel") # Used to repel overlapping text in charts [used in gglaplot?]
library("plotly") # Interactive charts
library("leaflet") # Interactive maps
library("rgdal") # Needed for geospatial manipulations
library("httr") # Quering websites to check if data exists
library("flextable") # Summary headline stats table
library("officer") # for fp_text
library("extrafont") #Fonts for the flextable
library("svDialogs") #for pop-ups
library("jsonlite") #for converting JSON formats
library("openxlsx") #exporting to Excel
library("reactable") #For interactive tables
library("sf") # for geometry

# Retrieving data from https://api.emsidata.com/apis/uk-job-postings

INPUT <- paste0(here::here(),"/INPUT/")
INTERMEDIATE <- paste0(here::here(),"/INTERMEDIATE/")
FORMATTING <- paste0(here::here(),"/FORMATTING/")
OUTPUT <- paste0(here::here(),"/OUTPUT/")
AD_HOC <- paste0(here::here(),"/OUTPUT/IMAGES/AD_HOC/")
IMAGES <- paste0(here::here(),"/OUTPUT/IMAGES/MAIN/")
HTML <- paste0(here::here(),"/OUTPUT/HTML/")

#_______________________________________________________________________________
#### Set up EMSI functions ####
#_______________________________________________________________________________

#...............................................................................
## Function to retrieve token

get_token <- function(client_id="greater-london-authority",client_secret="Y4I3SYBr") { #These are our own passcodes, not to be shared!
  
  # Define our client info
  CLIENT_ID <-  client_id
  CLIENT_SECRET <-  client_secret
  
  token_url <- "https://auth.emsicloud.com/connect/token"
  token_payload <- paste0("client_id=",CLIENT_ID,"&client_secret=",CLIENT_SECRET,"&grant_type=client_credentials&scope=postings%3Auk")
  token_encode <- "form"
  
  # Send request
  token_response <- VERB("POST",
                         token_url,
                         body = token_payload,
                         add_headers(Content_Type = 'application/x-www-form-urlencoded'),
                         content_type("application/x-www-form-urlencoded"),
                         encode = token_encode) %>% 
    content("text")
  
  # Extract token and authorisation
  my_token <- fromJSON(token_response)[[1]]
  authorisation <- paste0("Bearer ",my_token) # used for authorisations in data requests
  return(authorisation)
}

#...............................................................................
# Check health 

check_health <- function(auth=NULL) {
  
  if (is.null(auth)) stop("Specify token name, obtained using get_token")
  
  url <- "https://emsiservices.com/uk-jpa/status"
  
  response <- VERB("GET", url, add_headers(Authorization = auth), content_type("application/octet-stream"))
  
  outcome <- fromJSON(content(response, "text",encoding = "UTF-8"))
  
  # Stop if token has expired
  if ("message" %in% names(outcome)) {
    if (outcome$message == "Token expired") stop("Token has expired - run get_token again")
  }
  
  outcome$data$message
}

#...............................................................................
## Find available meta data (used to find most recent month)

emsi_metadata <- function(auth=NULL) {
  
  if (is.null(auth)) stop("Specify token name, obtained using get_token")
  
  url <- "https://emsiservices.com/uk-jpa/meta"

  response <- VERB("GET", url, add_headers(Authorization = auth), content_type("application/octet-stream"))

  metadata <- (fromJSON(content(response, "text",encoding = "UTF-8"),flatten =TRUE))
  
  # Stop if token has expired
  if ("message" %in% names(metadata)) {
    if (metadata$message == "Token expired") stop("Token has expired - run get_token again")
  }
  
  return(metadata)
}

emsi_latest_month <- function(...) {
  metadata <- emsi_metadata(...)
  
  latest_month <- data.frame(month = as.vector(metadata[["data"]][["available_months"]])) %>% 
    mutate(date_day = ym(month))%>% 
    filter(date_day==max(date_day)) %>% 
    pull(month)
  
  return(latest_month)
}

#...............................................................................
## Retrieve time-series data
### First function retrieves single SOC code at a time (codes are combined if queried simultaneously)

emsi_timeseries_singleSOC <- function(soc_code=NULL, #SOC code passed
                                      soc_level=NULL, #Level of SOC
                                      start_date="2020-01",
                                      end_date=NULL,
                                      area_type="nuts1_name",
                                      area_name=NULL,
                                      post_type="active", #type of posting
                                      remote_filter=NULL, 
                                      more_filters_vec=NULL,
                                      auth=NULL) {
  ## Use the below for bugtesting
  # soc_level <- 1
  # soc_code <- 1
  # start_date <- "2020-01"
  # end_date <- "2021-01"
  # area_type <- "nuts1_name"
  # post_type <- "active"
  # remote_filter <- TRUE
  # area_name <- "London"
  # more_filters_vec <- NULL
  # auth <- auth_token
  
  # Ensure necessary parameters are specified
  ## If soc_level is 0, we will download totals
  if (is.null(auth)) stop("Specify token name, obtained using get_token")
  if (is.null(soc_level)) {
    stop("No SOC level specified (use soc_level=0 for totals)")
  }  else if (is.null(soc_code) & soc_level!=0) {
    stop("No SOC code specified (use soc_level=0 for totals)")
  }  else if(soc_level==0) soc_code <- 0
  
  ## If soc_level is 0, we will download totals
  if (is.null(end_date)) {
    stop("Specify end month or use function: emsi_latest_month()")
  }
  
  ## If looking for region, use London as default. Otherwise ask for LA name
  if (area_type=="nuts1_name" & is.null(area_name)) {
    area_name="London"
  }  else if (area_type=="lau1_name"){
    if (is.null(area_name)) stop("Specify local authority name under area_name")
  } else if (area_type=="uk") {
    area_name="UK"
  }

  # All possible levels
  check_soc_levels <- c(0:4)
  check_post_types <- c("active","posted","expired")
  check_area_types <- c("lau1_name","nuts1_name","uk")
  check_remote_filt <- c(TRUE,FALSE)
  
  # Ensure correct parameters defined
  checkmate::assert_choice(soc_level, choices = check_soc_levels)
  checkmate::assert_choice(post_type, choices = check_post_types)
  checkmate::assert_choice(area_type, choices = check_area_types)
  checkmate::assert_choice(remote_filter, choices = check_remote_filt, null.ok = TRUE)
  
  print(paste0("SOC level: ",soc_level,". SOC code: ",soc_code, ". Area: ",area_name,". ",more_filters_vec))
  
  # Assign all necessary filters
  all_filters_vec <- c()
  
  # Area
  json_area_vec <- c(area_name)
  names(json_area_vec) <- area_type
  if (area_type %in% c("nuts1_name","lau1_name")) { #If UK overall, we do not want an area filter
    all_filters_vec <- c(all_filters_vec,json_area_vec)
  } 
  
  # SOC
  soc_entry <- paste0("soc",soc_level)
  json_soc_vec <- c(soc_code)
  names(json_soc_vec) <- soc_entry
  if (soc_level!=0) {
    all_filters_vec <- c(all_filters_vec,json_soc_vec)
  }
  
  
  # Additional filters
  if (!is.null(more_filters_vec)) {
    all_filters_vec <- c(all_filters_vec,more_filters_vec)
  }

  # The URL and encoding needed
  timeseries_url <- "https://emsiservices.com/uk-jpa/timeseries"
  encode <- "json"
  
  # The parameters passed on to query
  when_df <- data.frame("start"=start_date,"end"=end_date,"type"=post_type)
  metrics <- c("unique_postings","median_salary")
  
  
  
  # The JSON query is built using lists
  if (is.null(remote_filter)) {
  filter_list <- c(list("when"=jsonlite::unbox(when_df),
                        "is_internship"=jsonlite::unbox(FALSE)), #parameter to align with EMSI Analyst data
                   all_filters_vec) # All filters combined
  }  else {
    filter_list <- c(list("when"=jsonlite::unbox(when_df),
                          "is_remote"=jsonlite::unbox(remote_filter),
                          "is_internship"=jsonlite::unbox(FALSE)), #parameter to align with EMSI Analyst data
                     all_filters_vec) # All filters combined
  }
  
  # Format query into JSON format
  json_list <- (list("filter"=filter_list,
                     "metrics"=metrics))

  
  json_req <- toJSON(json_list)
  
  # Send query
  response <- VERB("POST",
                   timeseries_url,
                   body = json_req,
                   add_headers(Authorization = auth, Content_Type = 'application/json'),
                   content_type("application/json"),
                   encode = encode) %>% 
    content("text",encoding = "UTF-8")
  
  json_output <- fromJSON(response)
  
  # Stop if token has expired
  if ("message" %in% names(json_output))  {
    if (json_output$message == "Token expired") stop("Token has expired - run get_token again")
  }
  
  # Sub helper function: if there is null list, turn into NA as otherwise cannot coerce to data.frame
  null_to_na_help <- function(obj) {
    if (is.list(obj)) {
      obj <- jsonlite:::null_to_na(obj)
      obj <- lapply(obj, null_to_na_help)
    }
    return(obj)
  }
  
  # Produce data frame with data
  df_temp <- as.data.frame(null_to_na_help(json_output)) %>% 
    clean_names   %>% 
    mutate(soc_code = soc_code,
           soc_level = soc_level,
           geography_name = area_name,
           date_day = ym(data_timeseries_month),
           date_month= format(date_day,"%B %Y")) %>% 
    select(-c(contains("data_totals"))) %>% 
    rename(job_postings=data_timeseries_unique_postings,
           median_salary=data_timeseries_median_salary,
           data_month=data_timeseries_month) %>% 
    relocate(date_day,data_month,soc_code,job_postings,median_salary)
  
  return(df_temp)
}

#...............................................................................
# Combine relevant SOC codes
### Specify a vector of SOC codes and the function creates a full dataframe with all codes

emsi_timeseries_multiSOC <- function(soc_vector=NULL,...) {
  if (is.null(soc_vector)) {
    stop("No SOC vector specified")
  }
    
  df_list <- lapply(soc_vector,emsi_timeseries_singleSOC,...)
  df_combined <- bind_rows(df_list)
  
  return(df_combined)
}


#...............................................................................
## Retrieve rank-based data

emsi_ranking_singleSOC <- function(soc_code=NULL, #SOC code passed
                                   soc_level=NULL, #Level of SOC
                                   start_date="2020-01",
                                   end_date=NULL,
                                   area_type="nuts1_name",
                                   area_name=NULL,
                                   post_type="active", #type of posting
                                   remote_filter=NULL, 
                                   more_filters_vec=NULL,
                                   auth=NULL,
                                   rank_facet=NULL,
                                   rank_num=10) {
  ## Use the below for bugtesting
  # soc_level <- 0
  # soc_code <- 0
  # start_date <- "2020-01"
  # end_date <- "2021-01"
  # area_type <- "nuts1_name"
  # post_type <- "active"
  # remote_filter <- TRUE
  # area_name <- "London"
  # more_filters_vec <- NULL
  # auth <- auth_token
  # rank_facet <- "soc2"
  # rank_num <- 10
  
  # Ensure necessary parameters are specified
  ## If soc_level is 0, we will download totals
  if (is.null(auth)) stop("Specify token name, obtained using get_token")
  if (is.null(soc_level)) {
    stop("No SOC level specified (use soc_level=0 for totals)")
  }  else if (is.null(soc_code) & soc_level!=0) {
    stop("No SOC code specified (use soc_level=0 for totals)")
  }  else if(soc_level==0) soc_code <- 0
  
  ## If soc_level is 0, we will download totals
  if (is.null(end_date)) {
    stop("Specify end month or use function: emsi_latest_month()")
  }
  
  ## If looking for region, use London as default. Otherwise ask for LA name
  if (area_type=="nuts1_name" & is.null(area_name)) {
    area_name="London"
  }  else if (area_type=="lau1_name"){
    if (is.null(area_name)) stop("Specify local authority name under area_name")
  } else if (area_type=="uk") {
    area_name="UK"
  }
  
  # All possible levels
  check_soc_levels <- c(0:4)
  check_post_types <- c("active","posted","expired")
  check_area_types <- c("lau1_name","nuts1_name","uk")
  check_remote_filt <- c(TRUE,FALSE)
  
  # Ensure correct parameters defined
  checkmate::assert_choice(soc_level, choices = check_soc_levels)
  checkmate::assert_choice(post_type, choices = check_post_types)
  checkmate::assert_choice(area_type, choices = check_area_types)
  checkmate::assert_choice(remote_filter, choices = check_remote_filt, null.ok = TRUE)
  
  print(paste0("SOC level: ",soc_level,". SOC code: ",soc_code, ". Area: ",area_name,". ",more_filters_vec))
  
  # Assign all necessary filters
  all_filters_vec <- c()
  
  # Area
  json_area_vec <- c(area_name)
  names(json_area_vec) <- area_type
  if (area_type %in% c("nuts1_name","lau1_name")) { #If UK overall, we do not want an area filter
    all_filters_vec <- c(all_filters_vec,json_area_vec)
  } 
  
  # SOC
  soc_entry <- paste0("soc",soc_level)
  json_soc_vec <- c(soc_code)
  names(json_soc_vec) <- soc_entry
  if (soc_level!=0) {
    all_filters_vec <- c(all_filters_vec,json_soc_vec)
  }
  
  
  # Additional filters
  if (!is.null(more_filters_vec)) {
    all_filters_vec <- c(all_filters_vec,more_filters_vec)
  }
  
  # Rank
  rank_list <- list(
    "by" = jsonlite::unbox("unique_postings"),
    "limit" = jsonlite::unbox(rank_num),
    "extra_metrics" = c("median_salary")
  )
  
  # The URL and encoding needed
  rank_url <- paste0("https://emsiservices.com/uk-jpa/rankings/",rank_facet)
  encode <- "json"
  
  # The parameters passed on to query
  when_df <- data.frame("start"=start_date,"end"=end_date,"type"=post_type)
  
  
  # The JSON query is built using lists
  if (is.null(remote_filter)) {
    filter_list <- c(list("when"=jsonlite::unbox(when_df),
                          "is_internship"=jsonlite::unbox(FALSE)), #parameter to align with EMSI Analyst data
                     all_filters_vec) # All filters combined
  }  else {
    filter_list <- c(list("when"=jsonlite::unbox(when_df),
                          "is_remote"=jsonlite::unbox(remote_filter),
                          "is_internship"=jsonlite::unbox(FALSE)), #parameter to align with EMSI Analyst data
                     all_filters_vec) # All filters combined
  }
  
  # Format query into JSON format
  json_list <- (list("filter" = filter_list,
                     "rank" = rank_list))
  
  
  json_req <- toJSON(json_list)
  
  # Send query
  response <- VERB("POST",
                   rank_url,
                   body = json_req,
                   add_headers(Authorization = auth, Content_Type = 'application/json'),
                   content_type("application/json"),
                   encode = encode) %>% 
    content("text",encoding = "UTF-8")
  
  json_output <- fromJSON(response)
  
  # Stop if token has expired
  if ("message" %in% names(json_output))  {
    if (json_output$message == "Token expired") stop("Token has expired - run get_token again")
  }
  
  # Sub helper function: if there is null list, turn into NA as otherwise cannot coerce to data.frame
  null_to_na_help <- function(obj) {
    if (is.list(obj)) {
      obj <- jsonlite:::null_to_na(obj)
      obj <- lapply(obj, null_to_na_help)
    }
    return(obj)
  }
  
  # Produce data frame with data
  df_temp <- as.data.frame(null_to_na_help(json_output)) %>% 
    clean_names  %>% 
    mutate(within_soc_code = soc_code,
           within_soc_level = soc_level,
           geography_name = area_name,
           date_from = ym(start_date),
           date_to = ym(end_date),
           bucket_rank = rank(-data_ranking_buckets_unique_postings)) %>% 
    rename_with(~gsub("data_","",.x)) %>% 
    relocate(date_from,date_to,bucket_rank,ranking_buckets_name,ranking_buckets_unique_postings,ranking_buckets_median_salary)
  
  return(df_temp)
}

#...............................................................................
# Query which options are available within taxonomy

emsi_taxonomy <- function(tax_name=NULL,query=NULL,out_limit=10000,auth=NULL) {
  
  if (is.null(auth)) stop("Specify token name, obtained using get_token")
  
  # Ensure function parameters are correct
  tax_names <- c("city","contract_type", "company", "country", "employment_type", "lau1", "nuts1", "nuts3", "skills", "soc1", "soc2", "soc3", "soc4", "title")
  checkmate::assert_choice(tax_name, choices = tax_names)
  
  url <- paste0("https://emsiservices.com/uk-jpa/taxonomies/",tax_name)
  
  # Query string is not necessary
  if (is.null(query)) {
    queryString <- list(limit=out_limit)
    response <- VERB("GET", url, add_headers(Authorization = auth),query = queryString, content_type("application/octet-stream"))
  }
  else {
    queryString <- list(q=query,limit=out_limit,autocomplete=FALSE)
    response <- VERB("GET", url, add_headers(Authorization = auth), query = queryString, content_type("application/octet-stream"))
  }

  json_output <- fromJSON(content(response, "text",encoding = "UTF-8"))
  
  # Stop if token has expired
  if ("message" %in% names(json_output)) {
    if (content(response)$message == "Token expired") stop("Token has expired - run get_token again")
  }
  
  tax_data <- as.data.frame(json_output) %>% clean_names
  return(tax_data)
  
}



#_______________________________________________________________________________
#### Set up general GLAE functions ####
#_______________________________________________________________________________


## Functions
### Function to make conditional text based on values
condi_text <- function(x,t=c("increase","noun","rise","growth","up","above")) { #Define names by the positive change
  if(t=="increase") {y = case_when(x<0 ~ "decreased",
                                   x>0 ~ "increased")
  return(y)}
  else if(t=="noun") {y = case_when(x<0 ~ "a decrease",
                                    x>0 ~ "an increase")
  return(y)}
  else if(t=="rise") {y = case_when(x<0 ~ "fell",
                                    x>0 ~ "rose")
  return(y)}
  else if(t=="growth") {y = case_when(x<0 ~ "shrank",
                                      x>0 ~ "grew")
  return(y)}
  else if(t=="up") {y = case_when(x<0 ~ "down",
                                  x>0 ~ "up")
  return(y)}
  else if(t=="above") {y = case_when(x<0 ~ "below",
                                     x>0 ~ "above")
  return(y)}
  else {return("[NB: wrong value for t]")}
}

### Define function to always present percentages with one decimal and figures without decimals
perc_form = function(x, d=1) sprintf(paste0("%1.",d,"f"), x) 

value_form = function(x,s=2,d= -1) format(signif(round(as.numeric(x), d),s), big.mark=",", scientific=FALSE)

### Function to remove negative sign from character strings (needed as formatted numbers are not numerical)
abs2 <- function(x) {
  y = gsub("-","",x)
  return(y)
}

### Helper function to ensure legend labels are placed correctly
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

### Helper function to re-introduce simplified modebar allowing chart download
plotly_modebar <- function(plotly_plot) {
  plotly_plot <- plotly_plot %>% plotly::config(displayModeBar = TRUE) %>% # Allows menu bar such that image can be downloaded
    plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d","pan2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d")) %>% 
    plotly::config(displaylogo = FALSE)
  plotly_plot
}

### To help pull data
pull_dtpt <- function(dt=NULL,flt=NULL,vr=NULL) {
  dtpt <- dt %>% 
    filter(eval(rlang::parse_expr(flt))) %>% 
    pull(var=vr)
  
  return(dtpt)
}
# THe function above can be used as follows:
# pull_dtpt(dt=paye_nat_long,flt="nationality == 'EU' & region== 'London' & date_day == max(date_day)",vr="employee_count")

# Function to save last chart displayed in PNG and SVG
save_GLA_plot <- function(plot_name, path=IMAGES, w=8, h=8,svg_save=FALSE) {
  
  if (svg_save==TRUE) ggsave(paste0(path,Sys.Date(),"_",plot_name,".svg"), device = "svg", width = w, height = h, units = "in")
  
  ggsave(paste0(path,plot_name,".png"), device = "png", width = w, height = h, units = "in")
}

### Standard ggplotly settings
# Note: the title and subtitle text need to be enclosed within a paste function within single quotes, such that functions can be evaluated

LMU_plotly_settings <- function(ch_name, title_text=NULL,subtitle_text=NULL,hover_mode=c("x","closest","none","x unified","y unified"), ...) {
  
  hover_mode <- match.arg(hover_mode) # needs to be one or the other
  if (hover_mode=="none") hover_mode=FALSE
  
  if (hover_mode %in% c("x unified","y unified")) {
    
    gla_colours <- get(paste0("gla_",gla_theme_type)) # Colours are somehow overwritten for hover label, so reintroduce below
    
    ggplotly(ch_name,tooltip = "text") %>% 
      ggla_plotly_settings(...)    %>% 
      layout(title = list(text = (paste0("<b>",
                                         eval(parse(text=title_text)),
                                         "</b>","<br>",
                                         "<sup>",
                                         eval(parse(text=subtitle_text)),
                                         "</sup>","<br>")),
                          font = list(size = 22),
                          y = .95, xref = "plot"),
             xaxis = list(tickfont = list(size = 15)),
             yaxis = list(tickfont = list(size = 15)),
             legend = list(font = list(size = 15),title=list(text="")),
             hovermode = hover_mode,
             hoverlabel = list(bgcolor = gla_colours$headlines,
                               bordercolor = gla_colours$headlines,
                               font = list(family = "Arial",
                                           color = gla_colours$background))) %>% 
      plotly_modebar
  }
  else {
    ggplotly(ch_name,tooltip = "text") %>% 
      ggla_plotly_settings(...)    %>% 
      layout(title = list(text = (paste0("<b>",
                                         eval(parse(text=title_text)),
                                         "</b>","<br>",
                                         "<sup>",
                                         eval(parse(text=subtitle_text)),
                                         "</sup>","<br>")),
                          font = list(size = 22),
                          y = .95, xref = "plot"),
             xaxis = list(tickfont = list(size = 15)),
             yaxis = list(tickfont = list(size = 15)),
             legend = list(font = list(size = 15),title=list(text="")),
             hovermode = hover_mode) %>% 
      plotly_modebar
  }

}



# Round to nearest anything
rounder <- function(x,y) {
  if(y >= 0) {z = x + (y - x %% y)} #if rounding up
  else {z = x - (x %% abs(y))} #if rounding down
  
  z[dplyr::near(z,0)] <- 0 # due to floating point operators, ensure it is zero
  
  return(z)
}

#.............................................................................
#### Map production ----
#.............................................................................

# Helper function to create bins
map_prep <- function(dataset,
                     dt_var=NULL,
                     pal_colour=NULL,
                     diverge_scale=FALSE, #whether negatives and positives needed
                     bin_rounding=NULL, #floors and ceilings rounded by this number
                     perc_txt="") {
  
  checkmate::assertNumber(bin_rounding)
  checkmate::assertCharacter(dt_var)
  
  
  # Create interval ranges for bins
  dataset_bins <- dataset %>%
    arrange(!!sym(dt_var)) %>% 
    mutate(value_floor = rounder(!!sym(dt_var),-bin_rounding),
           value_ceil = rounder(!!sym(dt_var),bin_rounding),
           value_interval = paste0(format(value_floor,big.mark=",",trim=TRUE),perc_txt,
                                   " to ",format(value_ceil,big.mark=",",trim=TRUE),perc_txt)) 
  
  # Helper data for bins
  bin_data <- dataset_bins %>% 
    pivot_longer(cols=c("value_ceil","value_floor"),values_to="bin_val")  %>% 
    arrange(bin_val) %>% 
    distinct(bin_val) %>% 
    mutate(bin_int = paste0(format(bin_val,big.mark=",",trim=TRUE),perc_txt,
                            " to ",format(lead(bin_val,n=1),big.mark=",",trim=TRUE),perc_txt),
           neg_int = bin_val<0) #used when diverging scales
  
  
  # Number of bin intervals and define palette - differs on single scale or diverging
  
  # Define bin cuts for interactive map (only palette needs to be defined as divergent)
  bin_cuts <- bin_data %>%  
    pull(bin_val)
  
  if (diverge_scale==TRUE) {
    
    checkmate::assert_character(pal_colour,len=2) #must be of length two
    
    # Check that not all values are either positive or negative
    checkmate::assert_true(all(c(!all(bin_cuts>=0),!all(bin_cuts<0)))) 
    
    # Define bin cuts for interactive map, both negative and positive numbers
    # bin_cuts_neg <- bin_data %>%  
    #   filter(neg_int==TRUE) %>% 
    #   pull(bin_val)
    # 
    # bin_cuts_pos <- bin_data %>%  
    #   filter(neg_int==FALSE) %>% 
    #   pull(bin_val)
    
    # Define categorical bin  intervals for ggplot, both negative and positive numbers
    bin_ints_neg <- bin_data %>%
      filter(!grepl("NA",bin_int) & neg_int==TRUE) %>% # remove any with NA in interval
      arrange(bin_val) %>% 
      pull(bin_int)
    
    bin_ints_pos <- bin_data %>%
      filter(!grepl("NA",bin_int) & neg_int==FALSE) %>% # remove any with NA in interval
      arrange(bin_val) %>% 
      pull(bin_int)
    
    bin_ints <- c(bin_ints_neg,bin_ints_pos) #combined for easy use in ggplot
    
    # Length of bin vectors separately
    bin_ints_n <- c(length(bin_ints_neg),length(bin_ints_pos))
    
    # Create palette for diverging scale
    map_palette <- c(setNames((gla_pal(palette_type = "quantitative", 
                                       main_colours = pal_colour[1],
                                       n = bin_ints_n[1])),
                              nm=bin_ints_neg),
                     setNames(rev(gla_pal(palette_type = "quantitative", #reverse GLA pal order due to "bug"
                                          main_colours = pal_colour[2],
                                          n = bin_ints_n[2])),
                              nm=bin_ints_pos))
  }
  else {
    checkmate::assert_character(pal_colour,len=1) #must be of length one
    
    # Define bin cuts for interactive map
    # bin_cuts <- bin_data %>%  
    #   pull(bin_val)
    
    # Define categorical bin  intervals for ggplot
    bin_ints <- bin_data %>%
      filter(!grepl("NA",bin_int)) %>% # remove any with NA in interval
      arrange(bin_val) %>% 
      pull(bin_int)
    
    bin_ints_n <- length(bin_ints)
    
    # Create palette for uniform scale
    map_palette <- setNames(rev(gla_pal(palette_type = "quantitative", #reverse GLA pal order due to "bug"
                                        main_colours = pal_colour,
                                        n = bin_ints_n)),
                            nm=bin_ints)
  }
  
  
  # Create list of outputs to use in mapping functions
  map_list <- list(bin_cuts=bin_cuts,bin_ints=bin_ints,map_palette=map_palette,dataset_bins=dataset_bins)
  
  return(map_list)
  
}

## Produce the interactive map ***
leaflet_map_output <- function(dataset,
                               dt_var,
                               perc_bin=TRUE,
                               bin_rounding=NULL,
                               diverge_scale=FALSE, #whether negatives and positives needed
                               pal_colour,
                               title)
{
  
  # If bins have percanteges, include in strings
  if (perc_bin==TRUE) perc_txt="%"
  else perc_txt=""
  
  
  # Automatic binning
  map_list <- map_prep(dataset=dataset,
                       dt_var=dt_var,
                       pal_colour=pal_colour,
                       perc_txt=perc_txt,
                       diverge_scale=diverge_scale,
                       bin_rounding=bin_rounding)
  
  # Pull necessary objects from prep function
  map_bins <- map_list$bin_cuts
  palette <- map_list$map_palette
  
  
  # Create the palette
  pal_func <- colorBin(palette, domain = dataset[[dt_var]], bins = map_bins)
  
  # Make tool tip
  labels <- sprintf("<strong>%s</strong><br/>%s",dataset$geography_name, dataset$tooltip) %>%
    lapply(htmltools::HTML)
  
  # Make map
  map_output1 <- leaflet(dataset) %>%
    addPolygons(fillColor = ~pal_func(dataset[[dt_var]]),
                weight = 1,
                opacity = 1,
                color = "white",
                fillOpacity = 0.8,
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))    %>%
    addLegend(pal = pal_func, 
              values = dataset[[dt_var]], 
              opacity = 0.8,
              title = title,
              position = "bottomright", 
              labFormat = labelFormat(between = paste0(perc_txt," to "), suffix=perc_txt))
  
  
  # Find default bounds
  lng_b <- map_output1[["x"]][["limits"]][["lng"]]
  lat_b <- map_output1[["x"]][["limits"]][["lat"]]
  bounds_txt <- paste0("function(btn, map){ map.flyToBounds([[",lat_b[1],",",lng_b[1],"],[",lat_b[2],",",lng_b[2],"]]);}")
  
  map_output <- map_output1 %>% 
    addEasyButton(easyButton(
      icon="fa-globe", title="Default zoom",
      onClick=JS(bounds_txt)))
  
  return(map_output)
}

## Produce the static map ***

ggplot_map_output <- function(dataset,
                              dt_var,
                              bin_rounding=bin_rounding,
                              perc_bin=TRUE,
                              diverge_scale=FALSE, #whether negatives and positives needed
                              pal_colour=pal_colour,
                              title,
                              chart_name,
                              caption_txt)
{
  
  # If bins have percanteges, include in strings
  if (perc_bin==TRUE) perc_txt="%"
  else perc_txt=""
  
  # Automatic binning
  map_list <- map_prep(dataset=dataset,
                       dt_var=dt_var,
                       pal_colour=pal_colour,
                       diverge_scale=diverge_scale,
                       perc_txt=perc_txt,
                       bin_rounding=bin_rounding)
  
  # Pull necessary objects from prep function
  map_bins <- map_list$bin_ints
  palette <- map_list$map_palette
  dataset_bins <- map_list$dataset_bins
  
  gg_map_chart <- dataset_bins %>%
    ggplot(aes(geometry = geometry,
               fill = factor(value_interval,levels=map_bins))) +
    ggla_sf() +
    scale_fill_manual(values = palette) +
    labs(
      title = paste0(gsub("<br>","\n",title)),
      subtitle = paste0(""),
      caption = paste0(gsub("<br>","\n",caption_txt)))
  
  save_GLA_plot(plot_name = chart_name,h=8,w=12)
  
  return(gg_map_chart)
}


#_______________________________________________________________________________
#### Table of top10 occs ####
#_______________________________________________________________________________

### Function to stylise tables

top_occs_table_func <- function(dataset_name=NULL) {
  
  if (is.null(dataset_name)) {
    stop("Specify compatible dataset name")
  }
  
  reactable(dataset_name, #Settings for the tables
            pagination = FALSE,
            wrap = TRUE,
            striped = TRUE,
            defaultColDef = colDef(
              align = "right",
              width = 80,
              headerStyle = list(align="center"),
              sortNALast = TRUE
            ),
            style=list(fontSize=14,fontFamily="Arial"),
            highlight = TRUE,
            columns = list( #the below gives proper names to columns and formats
              "bucket_rank" = colDef(name = knitr::asis_output("\U25BC Rank"),
                                     width = 80),
              "soc_name" = colDef(name = "Occupation",width = 450,filterable = FALSE),
              "soc_code"= colDef(name = "SOC code",width = 80),
              "ranking_buckets_unique_postings"= colDef(name = "Postings in period"
                                                        ,width = 120),
              "share_postings"= colDef(name = "Share of total postings",
                                       format = colFormat(suffix = "%"),
                                       width = 120))
  )
}

#.............................................................................
### Function for dowloading headline stats ----
#.............................................................................

HeadlineDownload <- function( data_series = "NM_59_1", 
                              time_period = c("1996-01","latest"), 
                              save_intermediate = TRUE,
                              econ_activity=c(0,3,7,9), #main ones used
                              sex=c(5,6,7), #all sexes
                              geography = NULL) {
  
  ### Download both absolute and percentage figures from NOMIS
  
  raw_nomis <- nomis_get_data( id = data_series, 
                               geography = geography,
                               time = time_period,
                               economic_activity=econ_activity,
                               sex=sex)
  
  ### Store downloads as dataframes
  
  lfs_stats <- raw_nomis %>% 
    mutate(DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
    filter(MEASURES %in% c(20207,20100) & !is.na(OBS_VALUE)) %>%
    select( "DATE_DAY", "DATE_NAME", "SEX_NAME", "GEOGRAPHY_NAME", "ECONOMIC_ACTIVITY_NAME", "VALUE_TYPE_NAME", "OBS_VALUE","MEASURES","MEASURES_NAME") %>% 
    pivot_wider( names_from = ECONOMIC_ACTIVITY_NAME, values_from = OBS_VALUE) %>%  
    clean_names()
  
  ### Save intermediate dataframes if specified
  
  if( save_intermediate == TRUE) {
    lfs_stats %>% 
      fwrite(file = paste0(INTERMEDIATE,Sys.Date(),"_LFS_update.csv"))
    
  }
  
  data.table::fwrite( as.data.frame(colnames(lfs_stats)), file = paste0(INTERMEDIATE,"/",Sys.Date(),"_colnames.csv"))
  
  return( lfs_stats)
  
}
