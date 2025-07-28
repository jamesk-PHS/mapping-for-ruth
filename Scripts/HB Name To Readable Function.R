
# Takes Vector of Healthboard Names and Formats to Readable Format 
# Designed for use in mutate e.g. data %>% mutate(Healthboard_Name=readable_HB_name(Healthboard_Name))

## Input: HB_Names: A vector of healthboard names 
## Works for the following formats: NHS AYRSHIRE & ARRAN, NHS AYRSHIRE AND ARRAN, NHS Ayrshire & Arran, NHS Ayrshire And Arran, 
##                                  AYRSHIREARRAN, AYRSHIRE & ARRAN, AYRSHIRE AND ARRAN, Ayrshire & Arran, Ayrshire And Arran

## Outputs: The vector of healthboard names reformatted in the following format: NHS Ayrshire & Arran

readable_HB_name <- function(HB_Names){
  
  fixed_names_df <- HB_Names %>%
    # Makes all capitals
    str_to_upper() %>%
    # Turns & into and
    gsub(" and "," & ",.,ignore.case = TRUE) %>%
    # Removes any blank spaces
    gsub(" ","",.) %>%
    # Removes & from name 
    gsub("&","",.) %>%
    # Removes NHS From Name
    gsub("NHS","",.) %>%
    #Removes - From Name
    gsub("-","",.) %>%
    # Puts in a dataframe (so new names can be left_joined)
    data.frame(Old_Name=.)
  
  # Name is now in e.g AYRSHIREARRAN format
  
  # Creates dataframe which converts between AYRSHIREARRAN format and more readable NHS Ayrshire & Arran format
  convertion_df <- data.frame(
    
    Old_Name=c("AYRSHIREARRAN","BORDERS","DUMFRIESGALLOWAY","FIFE","FORTHVALLEY",
               "GRAMPIAN","GREATERGLASGOWCLYDE","HIGHLAND","LANARKSHIRE","LOTHIAN","ORKNEY",
               "SHETLAND","TAYSIDE","WESTERNISLES","SCOTLAND","NATIONALFACILITY","GOLDENJUBILEE","NATIONALSUPPORT",
               "BÒRDSSNNANEILEANSIAR","NONPROVIDER","SPECIALHEALTHBOARDS","SPECIALHEALTHBOARD","NONENTITIES","NONENTITY"),
    New_Name=c("NHS Ayrshire & Arran","NHS Borders","NHS Dumfries & Galloway","NHS Fife","NHS Forth Valley",
               "NHS Grampian","NHS Greater Glasgow & Clyde","NHS Highland","NHS Lanarkshire","NHS Lothian","NHS Orkney",
               "NHS Shetland","NHS Tayside","NHS Western Isles","NHS Scotland","National Facility","NHS Golden Jubilee",
               "National Support","NHS Western Isles","Non-NHS Provider","Special Healthboard","Special Healthboard",
               "Non-Entity","Non-Entity")
    
  )
  
  # Attaches new more reable names to dataframe and pulls new healthboard names
  output <- fixed_names_df %>%
    left_join(convertion_df,by="Old_Name") %>%
    pull(New_Name) 
  
  # If any of the names are an NA return warning message
  if (any(is.na(output))){
    
    problem_names <- fixed_names_df %>%
      left_join(convertion_df,by="Old_Name") %>%
      filter(is.na(New_Name)) %>% 
      pull(Old_Name) %>%
      unique() 
    
    warning(paste("There has been NAs produced in Healthboard Name Formatting. The Errors Occured With:",paste(problem_names,collapse=", ")))
    
  } 
  
  # returns vector of formatted healthboard
  output %>%
    return()
  
}


