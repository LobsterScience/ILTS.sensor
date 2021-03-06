#' This function converts survey tables recorded in netmind format to Esonar format 
#' @param main_directory specifies path to main folder where netmind text tables are stored and where converted csv files will be outputed. Default ="R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Data & Code Files/ILTS Data/netmind_to_esonar"). Directory must contain a folder called data containing the txt files and a folder called output.

netmind2esonar <- function(main_directory = "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Data & Code Files/ILTS Data/netmind_to_esonar"){

library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringi)
library(lubridate)
library(stringr)

########## change main directory if applicable

output_directory <- file.path(main_directory,"output")
data_directory <- file.path(main_directory,"data")

folder_list <- list.files(path=data_directory)


for (i in folder_list){
  
folder <- i
file_list <- list.files(path=file.path(data_directory,folder))



for (j in file_list){

filename <- j

###### set number works for sets up to 3 digits (999), 
###### if > 999 sets, add additional nested ifelse statement
setno <- ifelse(stri_length(filename)==9,
                substr(filename,5,5),
                ifelse(stri_length(filename)==10,
                       substr(filename,5,6),substr(filename,5,7)))

columnames <- c("Date","Time","Latitude","Longitude","Speed","Primary","DoorSpread")
netmind <- read.fwf(file.path(data_directory,folder,filename),
                    skip = 6, header = FALSE, 
                    widths = c(7,7,16,16,5,11,11), 
                    col.names = columnames, colClasses = c("Time"="character"))

netmind <- netmind %>% 
  mutate(Primary = as.numeric(gsub("\\*","", Primary))) %>%
  mutate(DoorSpread = as.numeric(gsub("\\*","", DoorSpread))) %>%
  rename(GPSTIME = Time, GPSDATE = Date, LATITUDE = Latitude,
         LONGITUDE = Longitude, SPEED = Speed)

netmind <- netmind %>% mutate(GPSDATE = paste0(20,GPSDATE))
netmind$GPSDATE = as.Date(netmind$GPSDATE, format = "%Y%m%d")
netmind$GPSTIME = factor(netmind$GPSTIME, ordered = TRUE)

netmind$LATITUDE = trimws(netmind$LATITUDE, which = "both")
netmind$LATITUDE = str_squish(netmind$LATITUDE)
netmind$LONGITUDE = trimws(netmind$LONGITUDE, which = "both")
netmind$LONGITUDE = str_squish(netmind$LONGITUDE)
netmind$GPSTIME = trimws(netmind$GPSTIME, which = "both")

netmind_long <- netmind %>% 
  pivot_longer(c(Primary,DoorSpread), 
  names_to = "TRANSDUCERNAME", values_to = "SENSORVALUE")

## GMT to AST time (function will still work if this section is removed):
#####################################################
netmind_long <- netmind_long %>% mutate(date.time = paste(GPSDATE,GPSTIME))
netmind_long$date.time = as_datetime(netmind_long$date.time)
netmind_long <- netmind_long %>% mutate(date.time = date.time - 10800)
netmind_long <- netmind_long %>% separate(date.time,c("Date","Time"), sep = " ")
netmind_long <- netmind_long %>% mutate(GPSDATE = Date, GPSTIME = Time) %>%
  select(-Date,-Time)
netmind_long <- netmind_long %>% mutate(GPSTIME = gsub(":","",GPSTIME))
netmind_long$GPSTIME = as.character(netmind_long$GPSTIME)
####################################################


######### if possible SENSORNAME values other than "Headline" and "DoorMaster" 
######### exist specify here with additional ifelse statement
netmind_long <- netmind_long %>%
  mutate(SENSORNAME = ifelse(TRANSDUCERNAME %in% "Primary","Headline",
                             ifelse(TRANSDUCERNAME %in% "DoorSpread",
                                    "DoorMaster","")))

netmind_long <- netmind_long %>%
  mutate(CPUDATEANDTIME="", HEADING="", VALIDITY="", ERRORCODE="", 
         HYDROPHONE="", SIGNALSTRENGTH="",
         SET_NO = setno, TRIP_ID="")

netmind_long_arranged <- netmind_long[c("CPUDATEANDTIME",
                                        "GPSTIME",
                                        "LATITUDE",
                                        "LONGITUDE",
                                        "SPEED",
                                        "HEADING",
                                        "VALIDITY",
                                        "TRANSDUCERNAME",
                                        "SENSORNAME",
                                        "SENSORVALUE",
                                        "ERRORCODE",
                                        "HYDROPHONE",
                                        "SIGNALSTRENGTH",
                                        "SET_NO",
                                        "TRIP_ID",
                                        "GPSDATE")]

writename = paste0("ILTS.",year(first(netmind_long_arranged$GPSDATE)),".")
write.csv(netmind_long_arranged, file = paste0(output_directory,"/",writename,setno,".csv"), row.names = FALSE)

} 
} 


}
