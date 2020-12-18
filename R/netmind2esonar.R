
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringi)
library(lubridate)

########## change main directory if applicable

main_directory <- "R:/Science/Population Ecology Division/Shared/!PED_Unit17_Lobster/Lobster Unit/Data & Code Files/ILTS Data/netmind_to_esonar"
output_directory <- file.path(main_directory,"output")
data_directory <- file.path(main_directory,"data")

folder_list <- list.files(path=data_directory)

for (i in folder_list){
  
folder <- i
file_list <- list.files(path=file.path(data_directory,folder))



for (j in file_list){

filename <- j

###### set number works for sets up to 3 digits (999)
setno <- ifelse(stri_length(filename)==9,
                substr(filename,5,5),
                ifelse(stri_length(filename)==10,
                       substr(filename,5,6),substr(filename,5,7)))

columnames <- c("Date","Time","Latitude","Longitude","Speed","Primary","DoorSpread")
netmind <- read.fwf(file.path(data_directory,folder,filename),
                    skip = 6, header = FALSE, 
                    widths = c(7,7,16,16,5,11,11), 
                    col.names = columnames)

netmind <- netmind %>% 
  mutate(Primary = as.numeric(gsub("\\*","", Primary))) %>%
  mutate(DoorSpread = as.numeric(gsub("\\*","", DoorSpread))) %>%
  rename(GPSTIME = Time, GPSDATE = Date, LATITUDE = Latitude,
         LONGITUDE = Longitude, SPEED = Speed)

netmind <- netmind %>% mutate(GPSDATE = paste0(20,GPSDATE))
netmind$GPSDATE = as.Date(netmind$GPSDATE, format = "%Y%m%d")
netmind$GPSTIME = factor(netmind$GPSTIME, ordered = TRUE)

netmind$LATITUDE = trimws(netmind$LATITUDE, which = "both")
netmind$LONGITUDE = trimws(netmind$LONGITUDE, which = "both")

netmind_long <- netmind %>% 
  pivot_longer(c(Primary,DoorSpread), 
  names_to = "TRANSDUCERNAME", values_to = "SENSORVALUE")


######### if other possible TRANSDUCER names exist specify here with 
######### additional ifelse statement
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



