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
