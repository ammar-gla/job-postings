#_______________________________________________________________________________
# SOC code data London ----
#_______________________________________________________________________________

# Token needed
auth_token <- get_token()

check_health(auth_token)

# Get most recent date
last_month <- emsi_latest_month(auth_token)
last_month_date <- as.Date(ym(last_month))
last_month_form <- format(last_month_date,"%B %Y")

# For each SOC level, download the SOC codes available
soc_levels_vec <- c(1:4)

for (sc in soc_levels_vec) {
  soc_level_name <- paste0("soc",sc)

  # Query Lightcast for taxonomy data to find which SOC codes exist
  temp_dta <- emsi_taxonomy(tax_name=soc_level_name,
                            auth=auth_token) %>% 
    rename(soc_code = data_id, soc_name = data_name, soc_desc = data_description) # Create dataset with SOC codes and desc
  temp_vec <- temp_dta$soc_code # Extract into vector with codes
  
  # For each SOC level extract data for job postings
  temp_postings <- emsi_timeseries_multiSOC(soc_vector = temp_vec,
                                            start_date="2012-01",
                                            end_date=last_month,
                                            post_type="posted",
                                            soc_level=sc,
                                            auth=auth_token) %>%  #Download newly posted in month
    merge(temp_dta,by="soc_code")
  
  assign(paste0("postings_",soc_level_name),temp_postings)
  assign(paste0("dta_tax_",soc_level_name),temp_dta)
  assign(paste0("vec_",soc_level_name),temp_vec)
  
  remove(temp_postings,temp_dta,temp_vec,soc_level_name)
}

# Download data for totals usinc SOC=0
postings_soc0 <- emsi_timeseries_singleSOC(start_date="2012-01",
                                           end_date=last_month,
                                           post_type="posted",
                                           soc_level=0,
                                           auth=auth_token) %>% 
  mutate(soc_desc="Total across all occupations",
         soc_name="Total across all occupations")


# Combine all levels
postings_soc_full <- postings_soc0 %>% 
  rbind(postings_soc1,postings_soc2,postings_soc3,postings_soc4) %>% 
  group_by(soc_code) %>% 
  mutate(date_day_yoy=case_when(date_day>=as.Date("2013-01-01") ~ date_day-years(1),
                                TRUE ~ date_day), # helper column to use for yoy increase
         change_p_yoy_salary = case_when(date_day>=as.Date("2013-01-01") ~ 100* ((median_salary)/(median_salary[date_day==date_day_yoy])-1),
                                         TRUE~ NA_real_),
         index_mar20_salary = 100* (median_salary)/(median_salary[date_day=="2020-03-01"]), #Create index
         index_mar20_postings = 100* (job_postings)/(job_postings[date_day=="2020-03-01"]),
         index_jan18_postings = 100* (job_postings)/(job_postings[date_day=="2018-01-01"]),
         year=lubridate::year(date_day),
         soc_name_short = case_when(soc_name == "Managers, Directors and Senior Officials" ~ "Managers & directors",
                                    soc_name == "Professional Occupations" ~ "Professionals",
                                    soc_name == "Associate Professional and Technical Occupations" ~ "Assoc. prof. & technical",
                                    soc_name == "Administrative and Secretarial Occupations"  ~ "Admin & secretarial",
                                    soc_name == "Skilled Trades Occupations"  ~ "Skilled trades",
                                    soc_name == "Caring, Leisure and Other Service Occupations"  ~ "Caring & leisure",
                                    soc_name == "Sales and Customer Service Occupations"  ~ "Sales",
                                    soc_name == "Process, Plant and Machine Operatives"   ~ "Operatives",
                                    soc_name == "Elementary Occupations" ~ "Elementary",
                                    TRUE ~ soc_name),
         three_mon_ave = zoo::rollmean(job_postings,k=3,align="right",fill=NA), #rolling averages
         six_mon_ave = zoo::rollmean(job_postings,k=6,align="right",fill=NA)) %>% 
  ungroup() %>% 
  select(-date_day_yoy)

# Export
write.xlsx(postings_soc_full,paste0(OUTPUT,"/DATA/london_full_postings.xlsx"))

# Create long version in measure values
postings_soc_full_long <- postings_soc_full %>% 
  select(-c("index_mar20_salary","median_salary","data_month")) %>% 
  pivot_longer(cols=c("job_postings",contains("_mon_ave")),names_to="measure_name",values_to="measure_value") %>% 
  mutate(measure_name_clean= case_when(measure_name=="job_postings" ~ "Monthly",
                                       measure_name=="three_mon_ave" ~ "Three-month moving average",
                                       measure_name=="six_mon_ave" ~ "Six-month moving average"))

# Share of level 1 codes for bar charts
soc_share_bar_data <- postings_soc_full  %>% 
  filter(soc_level==1) %>% 
  group_by(year,soc_code,soc_name,soc_name_short) %>% 
  summarise(avg_monthly_postings=mean(job_postings)) %>% 
  group_by(year) %>% 
  mutate(postings_share= avg_monthly_postings/sum(avg_monthly_postings),
         year_date = as.Date(as.character(year),"%Y")) %>% 
  ungroup()

# Names of SOC level 1 categories
soc_names_vector <- soc_share_bar_data %>% pull(soc_name_short) %>% unique()

# Calculate the dissimilarity index at each level
postings_diss_index <- postings_soc_full %>% 
  select(-c(median_salary,geography_name,data_month,soc_desc,soc_name,index_mar20_salary,index_mar20_postings,soc_name_short,six_mon_ave,three_mon_ave)) %>% 
  group_by(soc_level,date_day) %>% # To calculate total postings in levels by month
  mutate(tot_postings_level = sum(job_postings)) %>% 
  ungroup() %>% 
  mutate(share_postings = job_postings / tot_postings_level) %>% 
  group_by(soc_level,year) %>% # To calculate total postings in levels on avg in 2019 
    mutate(avg_postings_level_2019 = case_when(year==2019 ~mean(tot_postings_level),
                                               TRUE ~ NA_real_)) %>% 
  group_by(soc_code,year) %>% # To calculate average job postings and level share by code 
  mutate(avg_postings_code_2019 = case_when(year==2019 ~mean(job_postings),
                                      TRUE ~ NA_real_),
         share_avg_postings_2019 = case_when(year==2019 ~ avg_postings_code_2019/avg_postings_level_2019,
                                        TRUE ~ NA_real_ )) %>% 
  group_by(soc_code) %>% 
  mutate(avg_postings_code_2019 = max(avg_postings_code_2019, na.rm=TRUE),
         avg_postings_level_2019=max(avg_postings_level_2019, na.rm=TRUE),
         share_avg_postings_2019=max(share_avg_postings_2019,na.rm = TRUE),
         abs_diff_2019 = abs(share_postings-share_avg_postings_2019)) %>% 
  group_by(soc_level,date_day,date_month) %>% # collapse to level
  summarise(abs_diff_2019 = sum(abs_diff_2019)/2) # divide by 2 to have range 0-1

# # Create a totals row from SOC 1-9 to compare with SOC "0"
# postings_total <- postings_soc_full %>% 
#   filter(soc_level==1) %>%
#   mutate(soc_level=0,
#          soc_code=0) %>% 
#   group_by(soc_level,date_day,date_month) %>% 
#   summarise(sum_jobs=sum(job_postings)) %>% 
#   merge(postings_soc0,by=c("soc_level", "date_day", "date_month")) %>% 
#   select(date_day,date_month,sum_jobs,job_postings) %>% 
#   relocate(date_day,date_month,sum_jobs,job_postings) %>% 
#   mutate(diff_postings=sum_jobs-job_postings)

# NOTE: the sum across SOC 1-9 is slightly lower than the SOC 0 total !!!!!

#_______________________________________________________________________________
# Remote work indicator ----
#_______________________________________________________________________________

# Both for London and UK
for (geo_area in c("uk","nuts1_name")) { #nuts1_name will return London overall
  for (is_remote in c(TRUE,FALSE)) {
    tempname <- paste0(as.character(is_remote),"_",geo_area)
    postings_remote_temp <- emsi_timeseries_singleSOC(start_date="2012-01",
                                                      end_date=last_month,
                                                      post_type="posted",
                                                      soc_level=0,
                                                      area_type = geo_area,
                                                      auth=auth_token,
                                                      remote_filter = is_remote) %>% 
      mutate(soc_desc="Total across all occupations",
             soc_name="Total across all occupations",
             remote_posting=is_remote)
    
    assign(paste0("postings_remote_",tempname),postings_remote_temp)
    
    remove(postings_remote_temp,tempname)
  }
}

postings_remote <- postings_remote_TRUE_nuts1_name %>% 
  rbind(postings_remote_FALSE_nuts1_name,postings_remote_TRUE_uk,postings_remote_FALSE_uk) %>% 
  group_by(date_day,geography_name) %>% 
  mutate(remote_group_share=job_postings/sum(job_postings)) %>% 
  group_by(remote_posting,geography_name) %>% 
  mutate(index_mar20_postings = 100* (job_postings)/(job_postings[date_day=="2020-03-01"])) %>% 
  ungroup()

remove(postings_remote_TRUE_nuts1_name,postings_remote_FALSE_nuts1_name,postings_remote_TRUE_uk,postings_remote_FALSE_uk)

# Retrieve data  by SOC level 1
for (geo_area in c("uk","nuts1_name")) { 
  for (is_remote in c(TRUE,FALSE)) {
    tempname <- paste0(as.character(is_remote),"_",geo_area,"_socs")
    postings_remote_temp <- emsi_timeseries_multiSOC(start_date=last_month,
                                                     end_date=last_month,
                                                     post_type="posted",
                                                     soc_level=1,
                                                     soc_vector = vec_soc1,
                                                     area_type = geo_area,
                                                     auth=auth_token,
                                                     remote_filter = is_remote) %>% 
      mutate(remote_posting=is_remote)
    
    assign(paste0("postings_remote_",tempname),postings_remote_temp)
    
    remove(postings_remote_temp,tempname)
  }
}

postings_remote_socs <- postings_remote_TRUE_nuts1_name_socs %>% 
  rbind(postings_remote_FALSE_nuts1_name_socs,postings_remote_TRUE_uk_socs,postings_remote_FALSE_uk_socs) %>% 
  select(c(date_day,geography_name,soc_code,job_postings,remote_posting)) %>% 
  group_by(date_day,geography_name,soc_code) %>% 
  mutate(remote_group_share=job_postings/sum(job_postings)) %>% 
  group_by(geography_name,date_day) %>% 
  mutate(in_area_share=(job_postings)/sum(job_postings)) %>% 
  ungroup()

remove(postings_remote_TRUE_nuts1_name_socs,postings_remote_FALSE_nuts1_name_socs,postings_remote_TRUE_uk_socs,postings_remote_FALSE_uk_socs)


#_______________________________________________________________________________
# London LA data ----
#_______________________________________________________________________________

# Retrieve list of local authorities and keep London LAs
london_la_vector <- emsi_taxonomy(tax_name = "lau1",
                                  auth=auth_token) %>% 
  filter(grepl("E09",data_id)) %>% 
  pull(data_name)

# Combine total postings for all local authorities
## Note that all other variables have to be specified so function knows we are giving lau1 list
postings_la_full <- lapply(X=london_la_vector,FUN=emsi_timeseries_singleSOC,
                           soc_code=0,soc_level=0,start_date="2012-01",end_date=last_month,post_type="posted",area_type="lau1_name",auth=auth_token) %>% 
  bind_rows() %>% 
  mutate(year=lubridate::year(date_day)) %>% 
  group_by(geography_name,year) %>% 
  mutate(avg_postings_2019= case_when(year==2019 ~mean(job_postings),
                                      TRUE ~ NA_real_)) %>% 
  group_by(geography_name) %>% 
  mutate(avg_postings_2019 = max(avg_postings_2019,na.rm=TRUE),
         index_2019_postings = 100* (job_postings)/(avg_postings_2019)) %>% 
  ungroup()

## Sum up to compare with London level data
postings_la_tot <- postings_la_full %>% 
  group_by(date_day) %>% 
  summarise(total_la_postings=sum(job_postings))%>% 
  merge(postings_soc0,by="date_day") %>% 
  mutate(share_with_la=total_la_postings/job_postings) # Find what share of London postings have LA location

# Export
write.xlsx(postings_la_full,paste0(OUTPUT,"/DATA/london_la_postings.xlsx"))

#_______________________________________________________________________________
# Regional comparison data ----
#_______________________________________________________________________________

# Pull all regional ones
regions_vector <- emsi_taxonomy(tax_name = "nuts1",
                                auth=auth_token) %>% 
  pull(data_name)


df_list <- lapply(regions_vector,emsi_timeseries_singleSOC,
                  start_date="2012-01",
                  end_date=last_month,
                  post_type="posted",
                  soc_level=0,
                  soc_code=0,
                  area_type="nuts1_name",
                  auth=auth_token)
regions_postings <- bind_rows(df_list) 
remove(df_list) 

# Get UK overall
uk_postings <- emsi_timeseries_singleSOC(start_date="2012-01",
                                         end_date=last_month,
                                         post_type="posted",
                                         soc_level=0,
                                         soc_code=0,
                                         area_type="uk",
                                         auth=auth_token)

uk_region_postings <- uk_postings %>% 
  rbind(regions_postings)%>% 
  mutate(soc_desc="Total across all occupations",
         soc_name="Total across all occupations") %>% 
  group_by(geography_name) %>% 
  mutate(index_mar20_postings = 100*(job_postings/(job_postings[date_day=="2020-03-01"]))) %>% 
  ungroup()

#Comp
tot_regions <- regions_postings %>% 
  group_by(date_day) %>% 
  summarise(job_postings=sum(job_postings))

#_______________________________________________________________________________
# FT/PT overall data ----
#_______________________________________________________________________________

emp_type_names <- emsi_taxonomy(tax_name = "employment_type",
                                auth=auth_token) %>%  
  pull(data_name)

i=0
dat_list <- list()
for (type_name in emp_type_names) {
  i <- i+1
  dat_list[[i]] <- emsi_timeseries_singleSOC(start_date="2012-01",
                                           end_date=last_month,
                                           post_type="posted",
                                           soc_level=0,
                                           more_filters_vec = c("employment_type_name"=type_name),
                                           auth=auth_token) %>% 
    mutate(soc_desc="Total across all occupations",
           soc_name="Total across all occupations",
           emp_type=type_name)
}

ft_pt_postings_total <- bind_rows(dat_list) %>% 
  mutate(emp_type_name = case_when(emp_type=="Full-time (> 32 hours)" ~ "Full-time",
                                   grepl("Part-time \\(. 32 hours\\)",emp_type) ~ "Part-time", #R does not accept 'greater/equal to' symbol hence this workaround
                                   emp_type=="Part-time / full-time" ~ "Flexible hours")) %>% 
  group_by(date_day) %>% 
  mutate(emp_type_share=job_postings/sum(job_postings)) %>% 
  ungroup()
remove(dat_list)
remove(i)

# How many postings have info on hours out of total
hours_total_postings <- ft_pt_postings_total %>% 
  mutate(hours_info = case_when(emp_type=="Full-time (> 32 hours)" ~ "ft",
                                grepl("Part-time \\(. 32 hours\\)",emp_type) ~ "pt", #R does not accept 'greater/equal to' symbol hence this workaround
                                emp_type=="Part-time / full-time" ~ "flex")) %>% 
  select(date_day,soc_code,job_postings,geography_name,soc_desc,soc_name,hours_info,emp_type_share) %>% 
  pivot_wider(id_cols=c("date_day","soc_code","geography_name","soc_desc","soc_name"),names_from = hours_info,values_from=c("job_postings","emp_type_share")) %>% 
  rowwise() %>% 
  mutate(job_postings_hours=sum(job_postings_ft,job_postings_pt,job_postings_flex)) %>% 
  merge(postings_soc0,by=c("date_day","soc_code","geography_name","soc_desc","soc_name"))

#_______________________________________________________________________________
# Geographic data ----
#_______________________________________________________________________________


#.............................................................................
### London borough geographical codes
#.............................................................................
london_la_codes <-readxl::read_excel(path = here("INPUT","London borough codes.xlsx"), sheet = "Codes") %>%
  clean_names() %>%
  rename(geography_name=area)

# Combine jobs data with gss codes
la_jobs_map_data_geo <- postings_la_full %>% 
  merge(london_la_codes,by="geography_name") %>%
  mutate(tooltip= paste0("Job postings: ", value_form(job_postings,s=2), "\n"),
         measure_name = "job postings")  %>% 
  rename(GSS_CODE=gss_code, value=job_postings)   %>%  #Necessary since the shape files use upper caps variable names
  select(value,measure_name,GSS_CODE,tooltip,date_day)

#Read the shape file from the drive
boroughs <- readOGR(dsn = here("INPUT","statistical-gis-boundaries-london","ESRI"),
                    layer = "London_Borough_Excluding_MHW",
                    verbose = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

# Read in data and transform to long-lat format geometry
la_jobs_map_data_geo <- read_sf(here("INPUT","statistical-gis-boundaries-london","ESRI","London_Borough_Excluding_MHW.shp")) %>% 
  clean_names() %>% 
  select(name,geometry) %>% 
  rename(geography_name=name) %>%  
  left_join(postings_la_full,by="geography_name") %>% 
  mutate(tooltip= paste0("Postings: ", value_form(job_postings))) %>% 
  filter(date_day==max(date_day)) %>% 
  st_transform(4326)

#_______________________________________________________________________________
# ONS skill levels ----
#_______________________________________________________________________________

# From ONS website on skill levels at 2-digit code
# https://www.ons.gov.uk/methodology/classificationsandstandards/standardoccupationalclassificationsoc/soc2020/soc2020volume1structureanddescriptionsofunitgroups
ons_skill_groups <-readxl::read_excel(path = here("INPUT","ONS_SOC_skill_levels.xlsx"), sheet = "skill_levels") %>%
  clean_names() %>% 
  rename(soc_code = soc2010_code) %>% 
  select(c(skill_level,soc_code)) %>% 
  filter(!is.na(soc_code))
  
# Combine with job postings data and collapse at skill level
postings_soc2_skill <- postings_soc2 %>%
  left_join(ons_skill_groups,by="soc_code") %>% 
  group_by(skill_level,date_day,date_month) %>% 
  summarise(job_postings=sum(job_postings)) %>% 
  group_by(skill_level) %>% 
  mutate(change_mar20_p = 100 * (job_postings/job_postings[date_day=="2020-03-01"] - 1),
         change_mar22_p = 100 * (job_postings/job_postings[date_day=="2022-03-01"] - 1)) %>% # the peak so far in 2022
  ungroup()


#_______________________________________________________________________________
# ESS Vacancies data ----
#_______________________________________________________________________________

# Import and clean ESS data
ess_vacancies <-readxl::read_excel(path = here("INPUT","ESS_2019_England_Data_Tables_Controlled_v03.01.xlsx"), sheet = "25",skip=9) %>%
  clean_names() %>% 
  rename(soc_desc= x1) %>% 
  filter(!is.na(soc_desc) & !is.na(london)) %>% 
  filter(!(soc_desc %in% c("HIGH-SKILL","MIDDLE-SKILL","SERVICE-INTENSIVE","LABOUR-INTENSIVE", "Significance Level: 95%","Unweighted row","Total"))) %>% 
  mutate(not_unclassified = case_when(soc_desc == "Unclassified staff" ~ 0,
                                      TRUE ~ 1),
         vacancies = as.numeric(london)) %>% 
  group_by(not_unclassified) %>% # exclude unclassified from shares
  mutate(vacancies_share = vacancies/sum(vacancies)) %>% 
  ungroup() %>% 
  select(c(soc_desc,vacancies,vacancies_share)) %>% 
  mutate(soc_code = case_when(soc_desc == "Managers"~ 1,
                              soc_desc == "Professionals" ~2,
                              soc_desc == "Associate professionals" ~3 ,
                              soc_desc == "Administrative/clerical staff" ~4,
                              soc_desc == "Skilled trades occupations"~5,
                              soc_desc == "Caring, leisure and other service staff" ~6 ,
                              soc_desc == "Sales and customer services staff"~7 ,
                              soc_desc == "Machine operatives" ~8,
                              soc_desc == "Elementary staff" ~9 ,
                              soc_desc == "Unclassified staff"~0))

 # Comparson data for ESS and EMsi
ess_comparison_data <- soc_share_bar_data  %>% 
  filter(year==2019) %>% 
  merge(ess_vacancies,by="soc_code",all=TRUE) %>% 
  mutate(soc_name_short= case_when(soc_code==0 ~ "Unclassified",
                                   TRUE ~ soc_name_short)) %>% 
  pivot_longer(cols=c("postings_share","vacancies_share"),names_to = "measure",values_to = "share") %>% 
  mutate(data_set = case_when(measure=="postings_share" ~ "Online job postings",
                              measure=="vacancies_share" ~ "ESS vacancies"))


#_______________________________________________________________________________
# ONS monthly vacancies data ----
#_______________________________________________________________________________

# Download data
vac_path <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/vacanciesbyindustryvacs02/current/vacs02may2022.xls"

#Download latest file
ons_vacancies_dl <- download.file( url = vac_path,
                           destfile = paste0(OTHERDATA,format(as.Date(Sys.Date()),"%b_%y"),"_ONS_vacs02.xls"),
                           mode = "wb")

# Import and clean ONS data
ons_vacancies_level <-readxl::read_excel(path = paste0(OTHERDATA,format(as.Date(Sys.Date()),"%b_%y"),"_ONS_vacs02.xls"), sheet = "levels",skip=3) %>%
  clean_names() %>% 
  rename(all_vacancies=all_vacancies1,quarter=sic_2007_sections) %>% 
  filter(!is.na(all_vacancies)) %>% 
  filter(!(is.na(quarter) | grepl("Change",quarter))) %>% 
  select(where( ~!all(is.na(.x)))) %>% # where column is empty 
  select(!x2) %>% 
  mutate(across(!c("quarter"), ~as.numeric(.)),
         quart_end = gsub(".*-","",quarter),
         date_day = (my(quart_end)),
         year=lubridate::year(date_day)) %>% 
  group_by(year) %>% 
  mutate(avg_vacancies_2019= case_when(year==2019 ~mean(all_vacancies),
                                      TRUE ~ NA_real_)) %>% 
  ungroup() %>% 
  mutate(avg_vacancies_2019 = max(avg_vacancies_2019,na.rm=TRUE),
         index_2019_vacancies = 100* (all_vacancies)/(avg_vacancies_2019)) %>% 
  relocate(year,quarter,quart_end,date_day,index_2019_vacancies)

# The # of vacancies per 100 jobs
ons_vacancies_rate <-readxl::read_excel(path = paste0(OTHERDATA,format(as.Date(Sys.Date()),"%b_%y"),"_ONS_vacs02.xls"), sheet = "ratios",skip=3) %>%
  clean_names() %>% 
  rename(all_vacancies=all_vacancies1,quarter=sic_2007_sections) %>% 
  filter(!is.na(all_vacancies)) %>% 
  filter(!(is.na(quarter) | grepl("Change",quarter))) %>% 
  select(where( ~!all(is.na(.x)))) %>% # where column is empty 
  select(!x2) %>% 
  mutate(across(!c("quarter"), ~as.numeric(.)),
         quart_end = gsub(".*-","",quarter),
         date_day = (my(quart_end)))
  
# Merge with UK and London total postings data
## First need to adjust the Lightcast data structure
emsi_uk <- uk_postings %>% 
  select(date_day,job_postings) %>% 
  rename(all_vacancies=job_postings) %>% 
  mutate(data_source="Lightcast UK")

emsi_london <- postings_soc0 %>% 
  select(date_day,job_postings) %>% 
  rename(all_vacancies=job_postings) %>% 
  mutate(data_source="Lightcast London")

emsi_ons_compare_data <- ons_vacancies_level %>% 
  select(date_day,all_vacancies) %>% 
  mutate(data_source="ONS UK",
         all_vacancies=all_vacancies*1000) %>% 
  rbind(emsi_uk,emsi_london) %>% 
  mutate(year=lubridate::year(date_day)) %>% 
  group_by(year,data_source) %>% 
  mutate(avg_vacancies_2019= case_when(year==2019 ~mean(all_vacancies),
                                       TRUE ~ NA_real_)) %>% 
  group_by(data_source) %>% 
  mutate(avg_vacancies_2019 = max(avg_vacancies_2019,na.rm=TRUE),
         index_2019_vacancies = 100* (all_vacancies)/(avg_vacancies_2019),
         three_mon_ave = case_when(data_source=="ONS UK" ~ index_2019_vacancies, #ONS is already an 3month average
                                   grepl("Lightcast",data_source) ~ zoo::rollmean(index_2019_vacancies,k=3,align="right",fill=NA))) %>% 
  relocate(year,date_day,index_2019_vacancies)

#_______________________________________________________________________________
# ONS & Indeed ----
#_______________________________________________________________________________

# Index all sources to March 2020, but note that Indeed and ONS already show fall from feb to march

ons_region_estimate <- readxl::read_excel(path = paste0(INPUT,"onlinejobadvertestimatesdataset131022.xlsx"), sheet = "Adverts by region Feb 2020 DD",skip=2) %>% 
  rename(geography_name="...2") %>% 
  filter(geography_name=="London") %>% 
  pivot_longer(cols=matches("\\d+"),names_to = "date_day_help",values_to="measure_value_help",
               values_transform = list(measure_value_help=as.numeric)) %>% 
  mutate(date_day_help=as.Date(as.numeric(date_day_help), origin = "1899-12-30"),
         date_month=format(date_day_help,"%b-%y"),
         measure_name="ons_index_mar20",
         measure_name_clean="ONS estimated job adverts") %>% 
  group_by(date_month) %>% #calculate monthly index
  mutate(monthly_mean=mean(measure_value_help),
         date_day=my(date_month)) %>% 
  filter(row_number()==1) %>% #keep one per date 
  ungroup() %>% 
  mutate(measure_value=100*monthly_mean/monthly_mean[date_day=="2020-03-01"]) %>%  #create index average in Feb20
  select(date_day,geography_name,measure_name,measure_name_clean,measure_value)

indeed_postings <- read.csv(here("INPUT","indeed_regional_gb.csv")) %>% 
  mutate(indeed_index_feb_01 = 100 + pct_chng_feb_1, #indexed to 1st Feb
         date=as.Date(date),
         date_month=format(date,"%b-%y"),
         measure_name="indeed_index_mar20",
         measure_name_clean="Indeed") %>% 
  rename(geography_name=region, date_day=date) %>% 
  filter(geography_name=="London") %>% 
  group_by(geography_name,date_month) %>% 
  mutate(monthly_mean=mean(indeed_index_feb_01), 
         date_day=my(date_month)) %>% 
  filter(row_number()==1) %>% #keep one per date 
  ungroup() %>% 
  mutate(measure_value=100*monthly_mean/monthly_mean[date_day=="2020-03-01"]) %>%  #create index average in Feb20
  select(date_day,geography_name,measure_name,measure_name_clean,measure_value)

# Textkernel data via ONS
textkernel_postings <- readxl::read_excel(here("INPUT","professiondemandbylocalauthority.xlsx"),sheet="Table 8",skip=4) %>% 
  clean_names() %>% 
  rename(geography_name=region) %>% 
  filter(geography_name=="London")%>% 
  mutate(across(matches("\\w{3}_\\d{2}"),~na_if(., "[x]")),
         across(matches("\\w{3}_\\d{2}"),~as.numeric(.))) %>% 
  adorn_totals("row",name="London") %>% 
  mutate(summary_profession_category=case_when(summary_profession_category=="-"~"Total",
                                               TRUE  ~ summary_profession_category),
         detailed_profession_category=case_when(detailed_profession_category=="-"~"Total",
                                               TRUE  ~ detailed_profession_category),
         measure_name="textkernel_index_mar20",
         measure_name_clean="Textkernel")

textkernel_postings_total <- textkernel_postings %>% 
  filter(detailed_profession_category=="Total") %>% 
  select(geography_name,matches("\\w{3}_\\d{2}"),measure_name,measure_name_clean) %>% 
  pivot_longer(cols=matches("\\w{3}_\\d{2}"), names_to = "date_month",values_to = "job_postings") %>% 
  mutate(date_day=lubridate::my(date_month),
         measure_value=100*job_postings/job_postings[date_day=="2020-03-01"]) %>% 
  select(date_day,geography_name,measure_name,measure_name_clean,measure_value)


postings_helper <- postings_soc_full %>% 
  filter(soc_level==0) %>% 
  select(date_day,geography_name,index_mar20_postings) %>% 
  rename(measure_value=index_mar20_postings) %>% 
  mutate(measure_name="lightcast_index_mar20",
         measure_name_clean="Lightcast")


ons_indeed_textkernel_postings <- ons_region_estimate %>% 
  rbind(indeed_postings,postings_helper,textkernel_postings_total) %>% 
  arrange(date_day,measure_name)

#_______________________________________________________________________________
# Top 10s ----
#_______________________________________________________________________________

soc2_ranks <- function(end_date=NULL,soc_level=NULL) {
  
  # First rank top 10 level 2 SOCs overall, fetching names too
  bucket_soc_level <- paste0("soc",soc_level)
  within_soc_level <- 0
  
  ## Use most recent three month period
  if (is.null(end_date)) {
    end_dt <- format(ym(last_month),"%Y-%m")
  } else {
    end_dt <- format(ym(end_date),"%Y-%m")
  }
  
  start_dt <- format(ym(end_dt)-months(3),"%Y-%m")
  
  soc_names_df <- emsi_taxonomy(tax_name=bucket_soc_level,
                                auth=auth_token) %>% 
    rename(soc_code = data_id, soc_name = data_name, soc_desc = data_description) # Create dataset with SOC codes and desc
  
  # Retrieve top 10 codes in 2021
  top10 <- emsi_ranking_singleSOC(
    soc_level=within_soc_level,
    start_date = start_dt,
    end_date = end_dt,
    rank_facet = bucket_soc_level,
    rank_num = 10,
    auth = auth_token) %>% 
    rename(soc_code=ranking_buckets_name) %>% 
    mutate(share_postings=ranking_buckets_unique_postings/totals_unique_postings)
  
  # prep for tables
  top10_named <- top10 %>% 
    merge(soc_names_df,by="soc_code") %>% 
    arrange(bucket_rank) %>% 
    select(bucket_rank,soc_code,soc_name,ranking_buckets_unique_postings,share_postings) %>% 
    relocate(bucket_rank,soc_name,soc_code,ranking_buckets_unique_postings,share_postings) %>% 
    mutate(ranking_buckets_unique_postings=value_form(ranking_buckets_unique_postings,s=4),
           share_postings= perc_form(100 * share_postings,d=1))
  
  return(top10_named)
}

top10_soc2_newquarter <- soc2_ranks(soc_level=2)

top10_soc2_3yrs <- soc2_ranks(soc_level=2, end_date = format((Sys.Date()-years(3)),"%Y-%m"))

top10_soc4_newquarter <- soc2_ranks(soc_level=4)

top10_soc4_3yrs <- soc2_ranks(soc_level=4, end_date = format((Sys.Date()-years(3)),"%Y-%m"))

#_______________________________________________________________________________
# RTI PAYE data ----
#_______________________________________________________________________________
  
  # Check if there is a file this month, otherwise take previous month
  
  paye_online_path_now <- paste0("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/",
                                 paste0("rtisa",tolower(format(Sys.Date(),'%b%Y'))),
                                 ".xlsx")
  
  paye_online_path_prev <-  paste0("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/",
                                   paste0("rtisa",tolower(format(floor_date(Sys.Date(),"month")-months(1),'%b%Y'))),
                                   ".xlsx")
  
  ## Function to assert URL exists
  urlFileExist <- function(url){
    HTTP_STATUS_OK <- 200
    hd <- httr::HEAD(url)
    status <- hd$all_headers[[1]]$status
    list(exists = status == HTTP_STATUS_OK)
  }
  
  # Actual control
  if (urlFileExist(paye_online_path_now)==TRUE) {
    paye_online_path <- paye_online_path_now
  }  else {
    paye_online_path <- paye_online_path_prev
  }
  
  #### Download latest PAYE file 
  raw_paye <- download.file( url = paye_online_path,
                             destfile = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"),
                             mode = "wb")
  
  
  paye_emp_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "7. Employees (NUTS1)", skip = 6) %>% 
    mutate(date_day=my(Date),
           measure_name = "emps") %>% 
    select(date_day, London, UK,measure_name) 
  
  paye_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "8. Median pay (NUTS1)", skip = 6) %>% 
    mutate(date_day=my(Date),
           measure_name = "median_pay") %>% 
    select(date_day, London, UK,measure_name) 
  
  paye_stats <- paye_emp_stats %>% 
    rbind(paye_pay_stats) %>% 
    pivot_longer(cols = c("UK","London"),names_to = "geography_name",values_to = "measure_value") %>% 
    group_by(geography_name,measure_name) %>% 
    mutate(d_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                      date_day > as.Date("2020-02-01") ~ measure_value - measure_value[date_day == "2020-02-01"]),
           p_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                      date_day > as.Date("2020-02-01") ~ (measure_value - measure_value[date_day == "2020-02-01"])/measure_value[date_day == "2020-02-01"]),
           d_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 1))),
           p_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1)),
           d_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 12))),
           p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 12))/lag(measure_value, n = 12)),
           index_mar20 = 100*measure_value/measure_value[date_day==as.Date("2020-03-01")]) %>% 
    ungroup()
  
  # Combine PAYE and jobs data
  postings_helper <- postings_soc_full %>% 
    filter(soc_level==0) %>% 
    select(date_day,job_postings,median_salary,geography_name,three_mon_ave,contains("index_mar20"))
  
  
    paye_postings_index <- paye_stats %>% 
    filter(measure_name=="emps" & geography_name=="London") %>% 
    select(date_day,geography_name,measure_value,index_mar20) %>% 
      rename(index_mar20_emps = index_mar20, emps_value = measure_value) %>% 
    merge(postings_helper,by=c("date_day","geography_name")) %>% 
    pivot_longer(cols=c(emps_value,index_mar20_emps,job_postings,median_salary,index_mar20_salary,index_mar20_postings),
                 values_to = "measure_value",
                 names_to = "measure_name") %>% 
    mutate(measure_name_clean = case_when(measure_name=="index_mar20_emps" ~ "Payrolled employees",
                                          measure_name=="index_mar20_postings" ~ "Online job postings",
                                          TRUE ~ NA_character_))
  
  remove(paye_emp_stats,paye_pay_stats)
  
  
  #.............................................................................
  ### Download LFS stats, automatically clean and save ----
  #.............................................................................
  london_geo_code <- 2013265927
  uk_geo_code <- 2092957697
  
  
  lfs_stats <- HeadlineDownload( time_period = c("2010-01","latest"),
                                 geography = c(london_geo_code),
                                 econ_activity=c(0,3,7,9), #main ones used for charts
                                 sex=7) %>% 
    filter(value_type_name=="Level") %>% 
    mutate(measures_name=case_when(measures_name=="number of people" ~ "level",
                                   TRUE ~ "rate")) %>% 
    select(date_day,date_name,measures_name,geography_name,contains("total")) %>% 
    pivot_wider(names_from = measures_name,values_from=contains("total"))
  
  lfs_latest_date <- max(lfs_stats$date_day)
  
  lfs_postings_stats <- lfs_stats %>% 
    left_join(postings_helper,by=c("date_day","geography_name")) %>% 
    mutate(postings_unemp_3m_ratio = three_mon_ave/total_unemployed_aged_16_and_over_level) %>% 
    pivot_longer(cols=c(contains("total"),postings_unemp_3m_ratio,three_mon_ave,job_postings,median_salary,index_mar20_salary,index_mar20_postings),
                 values_to = "measure_value",
                 names_to = "measure_name") %>% 
    mutate(measure_name_clean = case_when(measure_name=="total_unemployed_aged_16_and_over_rate" ~ "Unemployment rate (16+)",
                                          measure_name=="total_in_employment_aged_16_to_64_rate" ~ "Employment rate (16-64)",
                                          measure_name=="total_economically_inactive_aged_16_to_64_rate" ~ "Inactivity rate (16-64)",
                                          measure_name=="total_unemployed_aged_16_and_over_level" ~ "Unemployed people (16+)",
                                          measure_name=="total_in_employment_aged_16_to_64_level" ~ "Employed people (16-64)",
                                          measure_name=="total_economically_inactive_aged_16_to_64_level" ~ "Inactive people (16-64)",
                                          measure_name=="job_postings" ~ "Online job postings",
                                          measure_name=="three_mon_ave" ~ "Three-month average job postings",
                                          measure_name=="postings_unemp_3m_ratio" ~ "Online job postings per unemployed person (16+)",
                                          TRUE ~ NA_character_))
  
  remove(postings_helper)
  
