#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Services mapping script 1 - Primary and Secondary Care locations

# This script extracts data on primary and secondary care locations from 
# publicly available resources and aggregates them into a consistent format
# for mapping. 

# Specifically:
# - GP practices
# - Care homes
# - Hospitals (All types)
# - Pharmacies

# The main data source is opendata.nhs.scot (operated by PHS), however, it also
# uses the Scottish Care inspectorate (careinspectorate.com) and National
# Records of Scotland data to link service data together. The script has been
# structured to largely function automatically however, some URLs may need to
# be updated as geographic boundaries/relationships change over time.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. User Inputs ----

healthboard_oi <- "Forth Valley"


# 1 Housekeeping, setup, etc. ---------------------------------------------

library(tidyverse) # General data maniupulation package
library(tidylog) # Accompanies tidyverse
library(phsopendata) # OpenData API
library(ckanr) # Web scraping 
library(rvest) # Web scraping 

gc() # Tidying environment


# 1.1. Format User Inputs ----

source("Scripts/HB Name To Readable Function.R")

healthboard_oi <-  readable_HB_name(healthboard_oi)

# 1.2. Get Necessary Geogrpahy Data ----

most_recent_hb_code_lookup = package_show(id = "geography-codes-and-labels", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(name == "Health Board 2014 - Health Board 2019") %>%
  filter(created == max(created)) %>%
  pull(id)

hb_code_oi <- get_resource(most_recent_hb_code_lookup) %>%
  group_by(HBName) %>%
  filter(HBDateEnacted == max(HBDateEnacted)) %>%
  ungroup() %>%
  mutate(HBName = readable_HB_name(HBName)) %>%
  filter(HBName == healthboard_oi) %>%
  distinct(HB,HBName)


most_recent_council_area_lookup = package_show(id = "geography-codes-and-labels", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(name == "Council Area 2011 - Council Area 2019") %>%
  filter(created == max(created)) %>%
  pull(id)

local_authorities_oi <- get_resource(most_recent_council_area_lookup) %>%
  group_by(HBName) %>%
  filter(HBDateEnacted == max(HBDateEnacted)) %>%
  ungroup() %>%
  mutate(HBName = readable_HB_name(HBName)) %>%
  filter(HBName == healthboard_oi) %>%
  distinct(CA,CAName)

most_recent_hscp_code_lookup = package_show(id = "geography-codes-and-labels", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(name == "Health and Social Care Partnership 2016 - 2019") %>%
  filter(created == max(created)) %>%
  pull(id)

hscps_oi <- get_resource(most_recent_hscp_code_lookup) %>%
  group_by(HBName) %>%
  filter(HBDateEnacted == max(HBDateEnacted)) %>%
  ungroup() %>%
  mutate(HBName = readable_HB_name(HBName)) %>%
  filter(HBName == healthboard_oi) %>%
  distinct(HSCP,HSCPName)


# 2. Loading location data ------------------------------------------------

## 2.1. GP Data ----

# Identify most recent data
most_recent_practice_sizes = package_show(id = "gp-practice-contact-details-and-list-sizes", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

# Extract most recent data
GP_Locations <- get_resource(most_recent_practice_sizes) %>%
  dplyr::select(practice_code=PracticeCode,practice_name=GPPracticeName,postcode=Postcode) %>%
  mutate(practice_code = as.character(practice_code))

# Some GP practices are registered as a dispensary site. This code helps prevent 
# double-counting of these sites.
pharmacy_filter <- GP_Locations$practice_code

## 2.2 . Care Home Data ----

# Identify most recent data
link = "https://www.careinspectorate.com/index.php/publications-statistics/93-public/datastore"

# most_recent_care_home_data_url = read_html(link) %>% 
#   html_elements(css = ".button") %>%
#   html_attr(name = "href") %>%
#   na.omit() %>%
#   as.character() %>%
#   data.frame(full_url=.) %>%
#   mutate(end_of_url=str_split(full_url,"/")) %>%
#   mutate(end_of_url=unlist(lapply(end_of_url,last))) %>%
#   mutate(date=as.Date(end_of_url,"MDSF_data_%d %B %Y.csv")) %>%
#   na.omit() %>%
#   filter(date == max(date)) %>%
#   pull(full_url) %>%
#   gsub(" ","%20",.)

most_recent_care_home_data_url = read_html(link) %>% 
  html_elements(css = ".button") %>%
  html_attr(name = "href") %>%
  na.omit() %>%
  as.character() %>%
  data.frame(full_url=.) %>%
  mutate(end_of_url=str_split(full_url,"/")) %>%
  mutate(end_of_url=unlist(lapply(end_of_url,last))) %>%
  filter(grepl(".csv",end_of_url)) %>%
  mutate(number=parse_number(end_of_url)) %>%
  filter(number == max(number)) %>%
  pull(full_url) 


# Extract most recent data
Care_Service_Locations <- read_csv(paste0("https://www.careinspectorate.com",most_recent_care_home_data_url)) %>%
  dplyr::select(care_home_number=CSNumber,type=CareService,subtype=Subtype,postcode=Service_Postcode)

## 2.3. Hospital Info ----

# Determines *types* of hospitals
most_recent_hospital_info = package_show(id = "nhs-scotland-accident-emergency-sites", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

Hospital_Info <- get_resource(most_recent_hospital_info)  %>%
  dplyr::select(hosp_code=TreatmentLocationCode,hosp_name=TreatmentLocationName,type=CurrentDepartmentType,status=Status) %>% 
  # Converting Type codes into descriptive category
  mutate(type = case_when(type == "Type 1" ~ "Emergency Department",
                          type == "Type 2" ~ "Emergency Department",
                          type == "Type 3" ~ "Minor Injuries Unit/Other"))

# Determines *location* of hospitals
most_recent_hospital_codes_info = package_show(id = "hospital-codes", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

Hospital_Locations <-   get_resource(most_recent_hospital_codes_info)  %>%
  dplyr::select(hosp_code=HospitalCode,hosp_name=HospitalName,postcode=Postcode)

### Combining hospital data ----
Hospital_Locations_Full <- Hospital_Info %>%
  left_join(Hospital_Locations,by=c("hosp_code","hosp_name")) 

## 2.4 Pharmacy locations ----

# Identify most recent data
most_recent_dispenser_sites = package_show(id = "dispenser-location-contact-details", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

# Extract most recent data
Pharmacy_Locations <- get_resource(most_recent_dispenser_sites) %>%
  dplyr::select(pharm_code = DispCode, pharm_name = DispLocationName, postcode = DispLocationPostcode) %>%
  mutate(pharm_code = as.character(pharm_code)) %>% 
  filter(!(pharm_code %in% pharmacy_filter)) # Removing GP practices from pharmacy data


## 2.5 Dentist locations ----

# Identify most recent data
most_recent_dentist_sites = package_show(id = "dental-practices-and-patient-registrations", url = "https://www.opendata.nhs.scot/", as = "table")[["resources"]] %>%
  as_tibble() %>%
  select(id, name, created) %>%
  arrange(desc(created)) %>%
  filter(created == max(created)) %>%
  pull(id)

# Extract most recent data
Dentist_Locations <- get_resource(most_recent_dentist_sites) %>%
  dplyr::select(dentist_code = Dental_Practice_Code, dentist_name = address1, postcode = pc7) %>%
  mutate(dentist_code = as.character(dentist_code))

## 2.6. Other Services ----

Other_Hospital_Locations <- Hospital_Locations %>% 
  dplyr::select(hosp_code, hosp_name, postcode) %>%
  anti_join(Hospital_Locations_Full,by=c("hosp_code"))
  
# Community_Hospital_Locations <- readxl::read_xlsx("Extra sites data.xlsx") %>% 
#   dplyr::select(code, name, postcode) %>%
#   mutate(code = as.character(code))

Community_Hospital_Locations <- Other_Hospital_Locations

# 3. Split Some Data Into Service Types ----

## 3.1. Care Services ----

Elder_Care_Services <- Care_Service_Locations %>%
  filter(type == "Care Home Service") %>%
  filter(!(subtype %in% c("Day Care of Children (under 3s)",
                          "Day Care of Children (over 3s)",
                          "Mainstream Residential School",
                          "Residential Special School",
                          "School Hostel",
                          "Children & Young People"))) %>%
  filter(subtype == "Older People")


Other_Care_Services <- Care_Service_Locations %>%
  filter(type == "Care Home Service") %>%
  filter(!(subtype %in% c("Day Care of Children (under 3s)",
                          "Day Care of Children (over 3s)",
                          "Mainstream Residential School",
                          "Residential Special School",
                          "School Hostel",
                          "Children & Young People"))) %>%
  filter(subtype != "Older People")


## 3.2. Hospitals ----

MIU_Locations <- Hospital_Locations_Full %>%
  filter(type == "Minor Injuries Unit/Other") 

ED_Locations <- Hospital_Locations_Full %>%
  filter(type == "Emergency Department") 

# 4. Prepare Data To Be Joined ----

# The following code standardises the format of each data frame so we can 
# bind them together later on into one.

GP_Locations <- GP_Locations %>%
  dplyr::select(code=practice_code,name=practice_name,postcode) %>%
  mutate(service_type="GP Practice") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

MIU_Locations <- MIU_Locations %>%
  dplyr::select(code=hosp_code,name= hosp_name,postcode) %>%
  mutate(service_type="Minor Injuries Unit") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

ED_Locations <- ED_Locations %>%
  dplyr::select(code=hosp_code,name= hosp_name,postcode) %>%
  mutate(service_type="Emergency Department") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Elder_Care_Locations <- Elder_Care_Services %>%
  dplyr::select(code=care_home_number,name=care_home_number,postcode) %>%
  mutate(service_type="Care Home") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Other_Care_Services <- Other_Care_Services %>%
  dplyr::select(code=care_home_number,name=care_home_number,postcode) %>%
  mutate(service_type="Care Home") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Pharmacy_Locations <- Pharmacy_Locations %>% 
  dplyr::select(code=pharm_code,name=pharm_name,postcode) %>%
  mutate(service_type="Pharmacy") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

Dentist_Locations <- Dentist_Locations %>% 
  dplyr::select(code=dentist_code,name=dentist_name,postcode) %>%
  mutate(service_type="Dentist") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

# Community_Hospital_Locations <- Community_Hospital_Locations %>% 
#   mutate(service_type="Community hospital") %>%
#   mutate(code=as.character(code)) %>%
#   mutate(source = "Board website")

Community_Hospital_Locations <- Community_Hospital_Locations %>%
  dplyr::select(code=hosp_code,name= hosp_name,postcode) %>%
  mutate(service_type="Other Hospitals") %>%
  mutate(code=as.character(code)) %>%
  mutate(source = "Opendata etc.")

# 5. Combine All Services Data ----

All_Services_Locations <- rbind(GP_Locations,
                                MIU_Locations,
                                ED_Locations,
                                Elder_Care_Locations,
                                Other_Care_Services,
                                Pharmacy_Locations,
                                Dentist_Locations,
                                Community_Hospital_Locations)

# Tidying environment - removing all unnecessary data:
rm(most_recent_practice_sizes,
   GP_Locations,
   link,
   most_recent_care_home_data_url,
   Care_Service_Locations,
   Elder_Care_Locations,
   Elder_Care_Services,
   Other_Care_Services,
   most_recent_hospital_codes_info,
   most_recent_hospital_info,
   Hospital_Locations_Full,
   Hospital_Info,
   Hospital_Locations,
   MIU_Locations,
   ED_Locations,
   most_recent_dispenser_sites,
   Pharmacy_Locations,
   pharmacy_filter,
   Dentist_Locations,
   most_recent_dentist_sites,
   Community_Hospital_Locations)

# 6. Fix Postcode (Remove Space) ----

All_Services_Locations <- All_Services_Locations %>%
  mutate(postcode = gsub(" ", "",postcode))

# 7. Attach Postcode Lookup ----

# Now we need to make sure all our locations have geographic data attached.


# Via National Records of Scotland - Postcode Index file
# https://www.nrscotland.gov.uk/publications/scottish-postcode-directory-2025/ 
post_code_data <- "https://www.nrscotland.gov.uk/media/dwmdsj1k/spd_postcodeindex_cut_25_1_csv.zip"

temp <- tempfile() # Creating a temporary storage location for our zipped data
download.file(post_code_data,temp) # Assigning the data available in the URL above to the URL
Postcode_Lookup <- rbind(
  
  # Binding postcode CSV files together
  
  read_csv(unz(temp, "SmallUser.csv")) %>% # Reading in CSV postcode file 
    janitor::clean_names() %>% # Cleaning variable names
    select(postcode, datazone2011 = data_zone2011code, latitude, longitude), 
  
  read_csv(unz(temp, "LargeUser.csv")) %>% # Reading in CSV postcode file 
             janitor::clean_names() %>% # Cleaning variable names
             select(postcode, datazone2011 = data_zone2011code, latitude, longitude)
  
  ) %>%  # End of rbind() call 
  distinct() %>% # Removing duplicate entries
  group_by(postcode) %>% 
  slice_head() # Removing duplicate long/lat values
  
unlink(temp)


# Small geography (Datazone) relationships
DataZone_Lookup <- get_resource("d6e500c4-c1f2-4507-979a-e18855efd7a4") %>% 
  janitor::clean_names() %>% 
  dplyr::select(datazone2011 = data_zone, hscp_locality = sub_hscp_name, hscp, hb)

# Large geography (HSCP and beyond) relationships
Large_Geography_Lookup <- get_resource("944765d7-d0d9-46a0-b377-abb3de51d08e") %>% 
  janitor::clean_names() %>% 
  dplyr::select(hscp, hscp_name, hb, hb_name)


# Joining geographic data
DataZone_Lookup <- left_join(DataZone_Lookup, Large_Geography_Lookup)

Postcode_Lookup <-  Postcode_Lookup %>% 
  left_join(
    DataZone_Lookup,
    by = "datazone2011",
    relationship = "many-to-one"
  ) 


# Formatting geographic data before joining with Care site data
Postcode_Lookup <- Postcode_Lookup %>%
  mutate(postcode = gsub(" ", "", postcode)) %>%
  select(
    postcode, latitude, longitude, datazone2011,
    hscp_locality, hscp2019name = hscp_name, hscp2019 = hscp, hb2019name = hb_name, hb2019 = hb) 


All_Services_Locations <- All_Services_Locations %>%
  left_join(Postcode_Lookup,by="postcode") %>% 
  filter(!(if_any(everything(), is.na))) # Removing observations with empty data


# Tidying up
rm(DataZone_Lookup, Large_Geography_Lookup, Postcode_Lookup)

# 8. Filter For Location Of Interest ----

All_Services_Locations_filtered <- All_Services_Locations %>%
  mutate(hb2019name = readable_HB_name(hb2019name)) %>%
  filter(hb2019name == healthboard_oi)


