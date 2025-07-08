#setwd("C:/Users/gmant/OneDrive - PEST FREE WAITAKERE RANGES ALLIANCE INCORPORATED/Database_copy/updateDataGit")

library(dplyr)
library(lubridate)
library(readxl)
library(sf)
library(renv)

#library(arcgis) #for trying to write to agol

# in your project’s R console (or an R script)
#renv::snapshot(prompt = FALSE)

source("Date_parser.R") 

#improve, write a parser for all data which ensures everything is as expected, like data parser. 

#Check/Fix:
#*Check if traplength_90 is the same if i use trap specific data instead of the trap reports

#*I am not clipping to areas. So data outisde WRHA is included

#*I am removing duplicates, for CIT data and saving them to a duplicateRows.csv, check on this with racheal Fewster (cacthit)

#if using for map then create another ensure areas is updated to group extents aswell as create another area which include buffer zones.

#seperate updating database and widget calculation logic

#used csv for trapnz data, and got rid of areas, so can remove sf package-making project more lightweight. 

#Load CIT data
CIT_trap <- read.csv("data/CIT.csv", stringsAsFactors = FALSE)

#Load lastYearCIT.csv which is updated daily
Year_CIT_trap <- read.csv("data/LastYearCIT.csv", stringsAsFactors = FALSE )

#Parse date col, setting uniform format
CIT_trap$Date <- parse_if_needed(CIT_trap$Date)
Year_CIT_trap$Date <- parse_if_needed(Year_CIT_trap$Date)

#Check for duplicates
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates
# head(duplicates)  # Preview

#count how many duplicates of each record, consider contacting trapper if need clarification
# CIT_trap %>%
#   group_by_all() %>%
#   tally(sort = TRUE) %>%
#   filter(n > 1)->dupes

#check for duplicates in year data
duplicatesYear <- Year_CIT_trap[duplicated(Year_CIT_trap), ]
nrow(duplicatesYear)  # Total number of full-row duplicates

#need to load duplicates .csv after i first write it. 
saved_duplicates <- read.csv("data/duplicateRows.csv", stringsAsFactors = FALSE )
saved_duplicates$Date <- parse_if_needed(saved_duplicates$Date)

#merge duplicates
duplicateRows <- bind_rows(duplicatesYear, duplicates )

#check if duplicates already recorded in saved duplicates
new_rows <- anti_join(duplicateRows, saved_duplicates)

#if new duplicates to add then add them hear and write to file
if (nrow(new_rows) > 0) {
  duplicateRows <- bind_rows(new_rows, saved_duplicates)
  write.csv(duplicateRows, "data/duplicateRows.csv", row.names = FALSE)
  message(nrow(duplicateRows), " new duplicates saved.")
} else {
  message("No new duplicates to add.")
}

#remove duplicates from data
Year_CIT_trap <- Year_CIT_trap[!duplicated(Year_CIT_trap), ]
CIT_trap <- CIT_trap[!duplicated(CIT_trap), ]

#Check duplicates removed
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates
duplicatesYear <- Year_CIT_trap[duplicated(Year_CIT_trap), ]
nrow(duplicatesYear)  # Total number of full-row duplicates

#clean up environment
rm(duplicatesYear, new_rows, duplicateRows, saved_duplicates)

#check if new data is already in CIT trap database
new_rows <- anti_join(Year_CIT_trap, CIT_trap)

#if new rows to add, then add them and write to file
if (nrow(new_rows) > 0) {
  CIT_trap <- bind_rows(CIT_trap, new_rows)
  write.csv(CIT_trap, "data/CIT.csv", row.names = FALSE)
  message(nrow(new_rows), " new rows added.")
} else {
  message("No new rows to add.")
}

#clean environment
rm( new_rows)

#check you haven't made any duplicates
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates

#clean environment
rm(duplicates)

#include following if plotting
# Convert data table to an sf object using longitude and latitude columns as geometry.
CIT_trap <- st_as_sf(CIT_trap, coords = c("Longitude", "Latitude"))

#Set Coordinate reference System to WGS84 (EPSG:4326), as the coordinates are in degrees (longitude/latitude).
CIT_trap <- st_set_crs(CIT_trap, 4326)  #

#then transform coordinates to NZGD2000 New Zealand Transverse Mercator (EPSG:2193) for analysis in meters
CIT_trap <- st_transform(CIT_trap, crs =2193 )


#--------------------------------------------------------------------------------
#Load trap NZ data, loads all trap records, should get data for total traps from trap database, which shows traps without records too. 

#TNZ_trap <- st_read("data/my-projects-trap-records.shp")

TNZ_trap <- read.csv("data/trapnz_data.csv", stringsAsFactors = FALSE)

#parse date col to get uniform date format
TNZ_trap$record_date <- parse_if_needed(TNZ_trap$record_date)

TNZ_trap <- st_as_sf(TNZ_trap, wkt = "geom")

TNZ_trap <- st_set_crs(TNZ_trap, 4326)  #

#then transform coordinates to NZGD2000 New Zealand Transverse Mercator (EPSG:2193) for analysis in meters
TNZ_trap <- st_transform(TNZ_trap, crs =2193 )


#------------------------------------------------------------------------------------
#Load community group shape files, note this is the one i have created in ArcGIS and aims to represent trapping activity inside WRHA
#areas<- st_read("areas3.shp")

#ensure using the same crs
#st_crs(areas)
#areas <- st_transform(areas, crs =2193 )

#filter data to areas, so PF swanson which extends beyond WRHA boundary a long way isn;t included.
#TNZ_trap <- st_filter(TNZ_trap, areas)

#---------------------------------------------------------------
#CIT transformations

#Get a col with just the year
CIT_trap<- mutate(CIT_trap, year = year(Date))

# Add a season column
CIT_trap$Season <- ifelse(
  format(CIT_trap$date, "%m") %in% c("03", "04", "05"), "Autumn",
  ifelse(format(CIT_trap$date, "%m") %in% c("06", "07", "08"), "Winter",
         ifelse(format(CIT_trap$date, "%m") %in% c("09", "10", "11"), "Spring",
                "Summer"
         )))

# Add a months column
CIT_trap$Month <- format(CIT_trap$date, "%B")

#Order months by the calendar
CIT_trap$Month <- factor(CIT_trap$Month, levels = month.name)

#Filter to catches by selecting all char values not c('NA', 'None', 'Unspecified', "")
CIT_trap %>%filter(!(Species %in% c('NA', 'None', 'Unspecified', ""))) -> CIT_catches24

#an additional filter, probably can remove
CIT_catches24 %>% 
  filter(!is.na(Species)) -> CIT_catches24

#reduce data for whole set
#reduce col data from catches subset
CIT_reduced_whole <- CIT_trap %>% 
  select(Area, Species, TrapType, TimeTaken, Bait, Line, Date, Month,TrapName, Season)

#reduce col data from catches subset
CIT_catches_reduced <- CIT_catches24 %>% 
  select(Area, Species, TrapType, TimeTaken, Bait,Line, Date,Month, TrapName, Season)

#rename data to merge sets for all data
CIT_reduced_whole <- CIT_reduced_whole %>%
  rename(Trap_type=TrapType, Time_taken= TimeTaken,trap_id=TrapName )

#rename data to merge sets for catch subset
CIT_catches_reduced <- CIT_catches_reduced %>%
  rename(Trap_type=TrapType, Time_taken= TimeTaken, trap_id=TrapName)

#--------------------------------------------------------------------
#TNZ transformations

#add a column with just the year
TNZ_trap<- mutate(TNZ_trap, year = year(record_date))

# Add a season column
TNZ_trap$Season <- ifelse(
  format(TNZ_trap$record_date, "%m") %in% c("03", "04", "05"), "Autumn",
  ifelse(format(TNZ_trap$record_date, "%m") %in% c("06", "07", "08"), "Winter",
         ifelse(format(TNZ_trap$record_date, "%m") %in% c("09", "10", "11"), "Spring",
                "Summer")
  )
)


# Add a months column
TNZ_trap$Month <- format(TNZ_trap$record_date, "%B")

#Order months by the calendar
TNZ_trap$Month <- factor(TNZ_trap$Month, levels = month.name)

#Filter to catches by selecting all char values not c('NA', 'None', 'Unspecified', "")
TNZ_trap %>%
  filter(!(species_caught %in% c('NA', 'None', 'Unspecified'))) -> TNZ_catches2020

#reduce TNZ_trap data to selected cols
TNZ_reduced_whole <-TNZ_trap %>% 
  select(project, species_caught,trap_type, bait_at_departure, line, record_date, Month,trap_id, Season)

#reduce TNZ_catches2020 data to selected cols
TNZ_catches_reduced <-TNZ_catches2020 %>% 
  select(project, species_caught,trap_type, bait_at_departure, line, record_date, Month,trap_id, Season)

# Rename data columns to merge with CIT_reduced_whole
TNZ_reduced_whole <- TNZ_reduced_whole %>%
  rename(Species = species_caught, Area= project,Trap_type = trap_type, Bait=bait_at_departure, Line=line, Date=record_date, geometry=geom)

# Rename data columns to merge with CIT_catches_reduced
TNZ_catches_reduced <- TNZ_catches_reduced %>%
  rename(Species = species_caught, Area= project,Trap_type = trap_type, Bait=bait_at_departure, Line=line, Date=record_date, geometry=geom)

#add some code here to select the correct col for bait, as there is bait at arrival and if not re baited then this bait persists, but is not captured by current selection.

#add error handling here/ maybbe a function like date parser
TNZ_reduced_whole$trap_id <- as.character(TNZ_reduced_whole$trap_id)

TNZ_catches_reduced$trap_id <- as.character(TNZ_catches_reduced$trap_id)
#--------------------------------------------------------------------------------
#Bind rows

#bind whole 24 sets
All_traps <- bind_rows(TNZ_reduced_whole, CIT_reduced_whole)

write.csv(All_traps, "data/All_traps.csv", row.names = FALSE)

#st_write(All_traps, "data/All_traps.shp", delete_layer = TRUE)

#bind catches subsets
All_catches <- bind_rows(TNZ_catches_reduced, CIT_catches_reduced)

write.csv(All_catches, "data/All_catches.csv", row.names = FALSE)

All_catches <- st_set_geometry(All_catches, "geometry")

All_traps <- st_set_geometry(All_traps, "geometry")

#Write shapefiles and zip:
# 1) Write all_catches to a temporary folder
out_dir <- "data/tmp_shp"
dir.create(out_dir, showWarnings = FALSE)
st_write(All_catches, file.path(out_dir, "All_catches.shp"))

# 2) Zip up everything in that folder
shp_files <- list.files(out_dir, pattern = "^All_catches\\.", full.names = TRUE)
zip::zipr("data/All_catches_shp.zip", shp_files)

#3) clean up
unlink(out_dir, recursive = TRUE, force = TRUE)

# 1) Write all_catches to a temporary folder
out_dir <- "data/tmp_shp"
dir.create(out_dir, showWarnings = FALSE)
st_write(All_traps, file.path(out_dir, "All_traps.shp"))

# 2) Zip up everything in that folder
shp_files <- list.files(out_dir, pattern = "^All_traps\\.", full.names = TRUE)
zip::zipr("data/All_traps_shp.zip", shp_files)

#3) clean up
unlink(out_dir, recursive = TRUE, force = TRUE)

#--------------------------------------------------------------------------------------
#following are my tries to write to AGOL:

#should try with small shapefile

#usethis::edit_r_environ() #to edit .Renviron

# what are your env‐vars right now?
#Sys.getenv(c("ARCGIS_HOST","ARCGIS_CLIENT","ARCGIS_SECRET"))

# show exactly what’s in the ARCGIS_HOST env var
#print( Sys.getenv("ARCGIS_HOST") )


#token <- auth_user()
#set_arc_token(token)

#publish_layer(All_catches, "AllCatches")


#publish_layer(All_Traps, "All_Traps")

#--------------------------------------------------------------------------------.
#follwong might work but need to chunk the post

# library(httr2)
# library(jsonlite)
# 
# 
# # 1) Turn sf into an Esri FeatureSet list
# fs <- as_featureset(All_catches)
# features_json <- toJSON(fs$features, auto_unbox = TRUE)
# 
# # 2) Authenticate
# token <- auth_user(
#   username = Sys.getenv("ARCGIS_USER"),
#   password = Sys.getenv("ARCGIS_PASSWORD"),
#   host     = Sys.getenv("ARCGIS_HOST")
# )
# set_arc_token(token)
# 
# fs_url<-"https://services3.arcgis.com/oPAwo60qFKuNlkTL/arcgis/rest/services/All_catches/FeatureServer/0/applyEdits"
# 
# 
# resp <- request(fs_url) %>%
#   req_method("POST") %>%
#   # move 'f=json' into the URL query—sometimes the REST API only honors f in the URL
#   req_url_query(f = "json") %>%
#   req_body_form(
#     token = token$access_token,
#     adds  = features_json
#   ) %>%
#   req_perform()
# 
# # 1) What HTTP status code did we get?
# cat("Status:", resp_status(resp), "\n\n")
# 
# # 2) What are the raw headers?
# print(resp_headers(resp))
# 
# # 3) Dump the body as text so we can read the server’s own error
# raw <- resp_body_string(resp)
# cat("Body:\n", raw)
# 
# #-----------------------------------------------------------------------------------------------------------------------
# 
# 
# token <- auth_user(
#     username = Sys.getenv("ARCGIS_USER"),
#     password = Sys.getenv("ARCGIS_PASSWORD"),
#     host     = Sys.getenv("ARCGIS_HOST")
#   )
# 
# set_arc_token(token)
# 
# furl <- "https://services3.arcgis.com/oPAwo60qFKuNlkTL/arcgis/rest/services/All_catches/FeatureServer/0/applyEdits"
# 
# county_fl <- arc_open(furl)
# county_fl
# 
# 



























