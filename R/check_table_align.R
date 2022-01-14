#' @title check_table_align
#' @description checks for date/time misalignment for tows between ILTS_SENSORS and ILTS_TEMPERATURE tables.
#' @description outputs to working directory a table showing number of TEMPERATURE rows found within SENSOR time range for each tow.
#' @description also outputs a filtered table only showing tows with 0 matching rows (no alignment).
#' @author Geraint Element
#' @export

check_table_align <- function(){

library(bio.lobster)
db.setup()

temp <- connect.command(con, 'select * from lobster.ILTS_TEMPERATURE' )
sens <- connect.command(con, 'select * from lobster.ILTS_SENSORS')
library(tidyverse)
library(lubridate)

temp <- temp %>% mutate(tow = paste(TRIP_ID,SET_NO))
sens <- sens %>% mutate(tow = paste(TRIP_ID,SET_NO))


temp$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(temp$UTCDATE)), temp$UTCTIME, sep=" "), tz="UTC" )
temp = temp[ order(temp$timestamp , decreasing = FALSE ),]

sens$GPSTIME[which(nchar(sens$GPSTIME)==5)] = paste("0", sens$GPSTIME[which(nchar(sens$GPSTIME)==5)], sep="")
sens$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(sens$GPSDATE)), sens$GPSTIME, sep=" "), tz="UTC" )
sens = sens[ order(sens$timestamp , decreasing = FALSE ),]
err = which(is.na(sens$timestamp))
if(length(err)>0)sens = sens[-err,]

sens_back <- sens

failtest <- NULL
for(h in unique(year(sens$timestamp))){
  sens_year <- sens %>% filter(year(timestamp) %in% h)
  for(i in unique(sens_year$tow)){
    sens_tow <- sens_year %>% filter(tow %in% i)
    test_tow <- temp %>% filter(tow %in% sens_tow$tow[1] & timestamp>sens_tow$timestamp[1] & timestamp<sens_tow$timestamp[length(sens_tow$timestamp)])
    vec <- c(year(sens_tow$timestamp[1]),sens_tow$tow[1],nrow(sens_tow),nrow(test_tow),round(nrow(test_tow)/nrow(sens_tow)*100,0))
    failtest <- rbind(failtest,vec)
  }
}

failtest <- as.data.frame(failtest)
colnames(failtest) <- c("year","Sensor_tow","nrows_Sensor_data","matching_Temp_rows","as.perc")

temp_dat_check <- temp %>% select(tow,SOURCE) %>% group_by(tow) %>% summarise(SOURCE=first(SOURCE))
colnames(temp_dat_check) <- c("Sensor_tow","SOURCE_Temp")
failtest <- left_join(failtest,temp_dat_check)
failtest_1 <- failtest %>% filter(as.perc == 0)

write.csv(failtest, file = "align_check.csv", row.names = FALSE)
write.csv(failtest_1, file = "align_check_filtered.csv", row.names = FALSE)

print("diagnostic files written to working directory")

}
