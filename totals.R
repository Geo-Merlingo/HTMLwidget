setwd("D:/PFWRA/website/htmlwidget")

library(dplyr)
library(lubridate)
library(readxl)

#renv::snapshot(prompt = FALSE)


#--------------------------------------------------------------------------------
#filter to date frames:
#since 2020, since 2021, since 2022, since 2023, since 2024,since begginning of 2025, last 90 days, last 30 days, since beggining of the month.

#Filter to data frames for both catches and all traps

#load all catches data
All_catches <- read.csv("data/All_catches.csv", stringsAsFactors = FALSE)


#get todays date
today <- Sys.Date()
print(today)


# Define date filters (rolling and fixed starts)
date_filters <- list(
  since_2020     = as.Date("2020-01-01") #,
  # since_2021     = as.Date("2021-01-01"),
  # since_2022     = as.Date("2022-01-01"),
  # since_2023     = as.Date("2023-01-01"),
  # since_2024     = as.Date("2024-01-01"),
  # since_2025     = as.Date("2025-01-01"),
  # last_90_days   = Sys.Date() - 90,
  # last_30_days   = Sys.Date() - 30,
  # this_month     = as.Date(format(Sys.Date(), "%Y-%m-01"))
  
)

# Initialize totals
catch_totals   <- list()
# record_totals  <- list()
# trap_totals    <- list()

# All_traps_df <- All_traps %>% st_drop_geometry()
All_catches_df <- All_catches #%>% st_drop_geometry()


# Loop
for (name in names(date_filters)) {
  date_val <- date_filters[[name]]
  
  # Filter
  catches_df <- All_catches_df %>% filter(Date >= date_val)
  # traps_df   <- All_traps_df   %>% filter(Date >= date_val)
  
  # Assign filtered catches if needed
  assign(paste0("catches_", name), catches_df)
  
  # Count totals
  catch_totals[[name]]  <- nrow(catches_df)
  # record_totals[[name]] <- nrow(traps_df)
   # trap_totals[[name]]   <- traps_df %>% distinct(trap_id) %>% nrow()
}

# Assemble summary
summary_df <- tibble(
  filter_name     = names(date_filters),
  total_catches   = unlist(catch_totals) #,
  # total_records   = unlist(record_totals),
  # total_traps     = unlist(trap_totals)
)

print(summary_df)

#------------------------------------------------------------------------------------------------
# #in year calcs
# 
# # Define years you want to include
# years <- 2020:2025
# 
# # Create a new list for in_year filters
# in_year_filters <- list()
# 
# for (y in years) {
#   in_year_filters[[paste0("in_", y)]] <- list(
#     start = as.Date(paste0(y, "-01-01")),
#     end = as.Date(paste0(y, "-12-31"))
#   )
# }
# 
# # Initialize output lists
# in_year_catch_totals  <- list()
# in_year_record_totals <- list()
# in_year_trap_totals   <- list()
# 
# # Loop through in_year filters
# for (name in names(in_year_filters)) {
#   range <- in_year_filters[[name]]
#   start_date <- range$start
#   end_date <- range$end
#   
#   catches_df <- All_catches_df %>% filter(Date >= start_date, Date <= end_date)
#   traps_df   <- All_traps_df   %>% filter(Date >= start_date, Date <= end_date)
#   
#   in_year_catch_totals[[name]]  <- nrow(catches_df)
#   in_year_record_totals[[name]] <- nrow(traps_df)
#   in_year_trap_totals[[name]]   <- traps_df %>% distinct(trap_id) %>% nrow()
# }
# 
# # Assemble the summary table
# in_year_summary_df <- tibble(
#   filter_name   = names(in_year_filters),
#   total_catches = unlist(in_year_catch_totals),
#   total_records = unlist(in_year_record_totals),
#   total_traps   = unlist(in_year_trap_totals)
# )
# 
# print(in_year_summary_df)



#-----------------------------------------------------------------------------------------------------
# Do calculations:
#sum trap records

# CIT_trap_records_l<-length(CIT_trap[[1]])
# 
# TNZ_trap_records_l<-length(TNZ_trap[[1]])
# 
# #all trap records
# total_trap_record_length=CIT_trap_records_l+TNZ_trap_records_l
# message((total_trap_record_length), " total trap records")
# 
# #Percent from each:
# TNZ_trap_percent <- (TNZ_trap_records_l/total_trap_record_length)*100
# message((TNZ_trap_percent), " TrapNZ Percent ")
# 
# CIT_trap_percent <- (CIT_trap_records_l/total_trap_record_length)*100
# message((CIT_trap_percent), " CIT Percent ")



#---------------------------------------------------------------------------------


# Years to process
years <- 2020

# Define rat alias grouping
rat_aliases <- c('Rat - Ship', 'Rat - Norway', 'Ship Rat', 'Norway Rat', 'Rat')
mustelids <- c('Stoat', 'Ferret', 'Weasel')

# Loop through each year
for (y in years) {
  
  df_name <- paste0("catches_since_", y)
  df <- get(df_name)
  
  # Clean species names
  df <- df %>%
    mutate(Species = case_when(
      Species %in% rat_aliases ~ 'Rat',
      TRUE ~ Species
    ))
  
  # Update the cleaned data frame in environment
  assign(df_name, df)
  
  # Filter and count species
  rats <- df %>% filter(Species == 'Rat')
  possums <- df %>% filter(Species == 'Possum')
  musts <- df %>% filter(Species %in% mustelids)
  
  # Save as summary variables
  assign(paste0("rats_", y, "_VAR"), nrow(rats))
  assign(paste0("possums_", y, "_VAR"), nrow(possums))
  assign(paste0("mustelids_", y, "_VAR"), nrow(musts))
}





