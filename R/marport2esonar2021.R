
#' @title marport2esonar2021
#' @description for converting 2021 marport data to esonar format.
#' @param data_directory is file path to where unconverted Marport files are stored.
#' @param output_directory is file path to create directory where converted data will be stored.
#' @param correct.col.names 2021 Marport data had incorrect column headers, if this problem is not resolved in later outputs, use correct.col.names=T to correct column names (function will override to T if 2021 data is being run)
#' @param heading chooses whether to use "true" or "magnetic" heading. Default = "magnetic"
#' @author Geraint Element
#' @export

marport2esonar2021 <- function(data_directory = file.path(bio.datadirectory,'bio.lobster','data','survey','Marport 2021'),
                               output_directory = file.path(data_directory,'Marport converted'),
                               correct.col.names=T,heading = "magnetic"){

# require(bio.lobster)
# require(bio.utilities)
require(devtools) || stop("install devtools")
require(lubridate) || stop("install lubridate")
require(dplyr) || stop("install dplyr")
require(tidyr) || stop("install tidyr")
require(readr) || stop("install readr")
require(stringr) || stop("install stringr")



#### Read in file, separate columns nd clean up headers
file_list <- list.files(path=data_directory, pattern = "^[2]")

for (i in file_list){

  file = file.path(data_directory,i)
  ncol <- max(count.fields(file, sep = ","))
  mar <- read.csv(file, header = FALSE, col.names = paste0("V", seq_len(ncol)))

  mar1 <- separate(mar, V1, into = c("a","b","c","d","e","f","g","h","i","j","k","l","m"), sep =";")
  header <- as.character(mar[1,1])
  header <- append("Time",header)
  header <- strsplit(header, ";")
  header[[2]][1] = "NMEA"
  header = unlist(header)
  names(mar1) =c(header,"a","b","c","d","e","f","g","h","i","j","k","l","m","n")
  mar1 = mar1[-1,]

  #### Convert epoch time to readable/local time
  options(scipen=999)
  options(digits = 13)
  mar1 <- mar1 %>% mutate(Time1 = as.numeric(Time)/1000)
  mar1 <- mar1 %>% mutate(Time1 = format(as.POSIXct(Time1, origin = "1970-01-01", tz = "Canada/Atlantic"), "%Y-%m-%d %H:%M:%S"))
  mar1 <-  mar1 %>% mutate(Time = Time1) %>% select(-Time1)
  #############################################################

  #### Extract NMEA data into it's own table
  gpst <- mar1 %>% select(Time,NMEA,a,b,c,d,e,f,g,h,i,j,k,l,m,n)

  NMEA <- gpst$NMEA


  out.gpst = NULL
  for (j in 1:length(NMEA)){
 #   if(gpst$NMEA[j] == "$GPGGA"){
 #     ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$a[j]),as.character(gpst$b[j]),as.character(gpst$d[j]), NA,NA,NA,NA,NA)
 #     out.gpst = rbind(out.gpst, ooo)
 #   }

 #   if(gpst$NMEA[j] == "$GPGLL"){
 #     ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$e[j]),as.character(gpst$a[j]),as.character(gpst$c[j]), NA,NA,NA,NA,NA)
 #     out.gpst = rbind(out.gpst, ooo)
 #   }

    if(gpst$NMEA[j] == "$GPRMC"){
      ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$a[j]),as.character(gpst$c[j]),as.character(gpst$e[j]),as.character(gpst$h[j]),as.character(as.numeric(as.character(gpst$h[j]))+as.numeric(as.character(gpst$j[j]))),as.character(gpst$g[j]),NA,as.character(gpst$i[j]))
      out.gpst = rbind(out.gpst, ooo)
    }

 #   if(gpst$NMEA[j] == "$GPVTG"){
 #     ooo = cbind(gpst$Time[j],gpst$NMEA[j],NA,NA,NA,as.character(gpst$a[j]),as.character(gpst$c[j]),as.character(gpst$e[j]),as.character(gpst$g[j]),NA )
 #     out.gpst = rbind(out.gpst, ooo)
 #   }

  }

  out.gpst = as.data.frame(out.gpst)
  out.gpst <- out.gpst %>% rename(Time = V1, NMEA = V2)

  #### Make table with just sensor data
  sensort <- mar1 %>% select(!(a:n))

  #### Recombine sensor and NMEA data into useful table
  dat <- left_join(sensort,out.gpst)

  #### clean up column headers
  dat <- dat %>% rename(CPUDATEANDTIME =Time, GPSTIME = V3, LATITUDE = V4, LONGITUDE = V5)
  dat <- dat %>% rename(Track_made_good_deg_true = V6, Track_made_good_deg_magnetic = V7, Speed_knots = V8, Speed_over_ground_kph = V9, GPSDATE = V10)
  dat$CPUDATEANDTIME = as_datetime(dat$CPUDATEANDTIME)


  #### change GPSTIME from UTC to Local
  # dat <- dat %>% mutate(GPSTIME1 = parse_time(as.character(GPSTIME), "%H%M%S"))
  # dat <- dat %>% mutate(GPSDATE = as.Date(CPUDATEANDTIME))
  # dat <- dat %>% mutate(GPSDATETIME = ifelse(GPSTIME1 %in% NA, NA, paste(GPSDATE,"",GPSTIME1)))
  # dat <- dat %>% mutate(GPSTIME2 = as_datetime(ifelse(GPSDATETIME %in% NA, NA, as_datetime(GPSDATETIME) - 10800)))
  # dat <- dat %>% separate(GPSTIME2, c(NA,"GPSTIME3"), sep= " ")
  # dat <- dat %>% mutate(GPSTIME = gsub(":","",GPSTIME3))
  # dat <- dat %>% select(-GPSTIME1,-GPSDATETIME,-GPSTIME3)

  #### Remove unused NMEA rows
  dat <- dat %>% filter(!(NMEA %in% c("$GPRMB","$GPGGA","$GPGLL","$GPVTG")))


  #### Fill down GPSTIME, GPSDATE, SPEED, HEADING and coordinate values
  dat$GPSTIME = smartfill(dat$GPSTIME,reftime=dat$CPUDATEANDTIME,timeflex=4)


  #### Can't use smartfill for GPSDATE; must switch to next date if GPSTIME rolls over 24 hours
  ### also needs to fill from down for initial rows if this is NA.
  dat$GPSDATE = as.character(dat$GPSDATE)

  ### first block:fill from down for initial rows if this is NA.
  for (j in 1:length(dat$GPSDATE)){
    if(j==1){
      for(k in 1:30){
        if(is.na(dat$GPSDATE[j])){
          if(parse_time(as.character(dat$GPSTIME[j]),format = "%H%M%S")<=parse_time(as.character(dat$GPSTIME[(k+1)]),format = "%H%M%S")){dat$GPSDATE[j] = as.character(dat$GPSDATE[(k+1)])}
          if(!is.na(dat$GPSDATE[k+1])){break}
        }
      }
    }
    ### second block: adds 1 day to GPSDATE if previous row's GPSTIME is greater value, otherwise just fills value from previous row
    if(dat$GPSDATE[j] %in% NA){
      dat$GPSDATE[j] = ifelse(parse_time(as.character(dat$GPSTIME[j]),format = "%H%M%S")<parse_time(as.character(dat$GPSTIME[(j-1)]),format = "%H%M%S"),
                              ifelse(nchar(as.character(dat$GPSDATE[(j-1)]))==5,
                                     (paste0(day(parse_date(paste0("0",as.character(dat$GPSDATE[(j-1)])),format = "%d%m%y")+1),
                                      substr(parse_date(paste0("0",as.character(dat$GPSDATE[(j-1)])),format = "%d%m%y")+1,6,7),
                                      substr(parse_date(paste0("0",as.character(dat$GPSDATE[(j-1)])),format = "%d%m%y")+1,3,4))
                                      ),

                                     paste0(day(parse_date(as.character(dat$GPSDATE[(j-1)]),format = "%d%m%y")+1),
                                     substr(parse_date(as.character(dat$GPSDATE[(j-1)]),format = "%d%m%y")+1,6,7),
                                     substr(parse_date(as.character(dat$GPSDATE[(j-1)]),format = "%d%m%y")+1,3,4)
                                     )),
                              as.character(dat$GPSDATE[(j-1)]))
      }
    }


#### smartfill remaining variables
  dat$Track_made_good_deg_true = smartfill(dat$Track_made_good_deg_true,reftime=dat$CPUDATEANDTIME,timeflex=4)
  dat$Track_made_good_deg_magnetic = smartfill(dat$Track_made_good_deg_magnetic,reftime=dat$CPUDATEANDTIME,timeflex=4)
  dat$Speed_knots= smartfill(dat$Speed_knots,reftime=dat$CPUDATEANDTIME,timeflex=4)
  dat$LATITUDE = smartfill(dat$LATITUDE,reftime=dat$CPUDATEANDTIME,timeflex=4)
  dat$LONGITUDE = smartfill(dat$LONGITUDE,reftime=dat$CPUDATEANDTIME,timeflex=4)



# # ##### If you don't want to use smartfill function use this (same thing):
  # for(j in 1:length(dat$GPSTIME)){
  #   if(j==1){
  #     for(k in 1:30){
  #       if(is.na(dat$GPSTIME[j])){
  #         if(difftime(as_datetime(dat$CPUDATEANDTIME[j+1]),as_datetime(dat$CPUDATEANDTIME[(j)]),units = "secs")<=4){dat$GPSTIME[j] = as.character(dat$GPSTIME[(k+1)])}
  #         if(!is.na(dat$GPSTIME[k+1])){break}
  #       }
  #     }
  #   }
  #   if(is.na(dat$GPSTIME[j])){
  #     if(difftime(as_datetime(dat$CPUDATEANDTIME[j]),as_datetime(dat$CPUDATEANDTIME[(j-1)]), units = "secs")<=4){dat$GPSTIME[j] = as.character(dat$GPSTIME[(j-1)])}
  #     if(is.na(dat$GPSTIME[j])){
  #       for (k in 1:30){
  #         if(!is.na(dat$GPSTIME[j])){break}
  #         if(!is.na(dat$GPSTIME[(j+k)])){
  #           if(difftime(as_datetime(dat$CPUDATEANDTIME[(j+k)]),as_datetime(dat$CPUDATEANDTIME[j]), units = "secs")<=4){
  #             dat$GPSTIME[j] = as.character(dat$GPSTIME[(j+k)])
  #           }
  #         }
  #       }
  #     }
  #   }
  #   }


  #### format lat and long columns
  dat <- dat %>% mutate(LATITUDE = ifelse(LATITUDE %in% NA, NA, paste0(str_sub(LATITUDE, 1,2)," ",str_sub(LATITUDE, 3,-1)," N")))
  dat <- dat %>% mutate(LONGITUDE = ifelse(LONGITUDE %in% NA, NA, paste0(str_sub(LONGITUDE, 1,3)," ",str_sub(LONGITUDE, 4,-1)," W")))

  ##### check if column names need correcting

  if(year(dat$CPUDATEANDTIME[1])=='2021'){correct.col.names=T}

  if(correct.col.names==T){
    dat <- dat%>% select(-SNR,-Status, -`Noise Floor`)
    dat <- dat %>% rename(SNR=Value, Status=`Type of Data`, Value= `Sensor Protocol`, `Type of Data`=`Sensor Location`, `Sensor Location`=Receiver, `Noise Floor`=Quality )
  }


  #### remove leading zeros from track (heading) columns
  dat$Track_made_good_deg_magnetic = as.numeric(as.character(dat$Track_made_good_deg_magnetic))
  dat$Track_made_good_deg_true = as.numeric(as.character(dat$Track_made_good_deg_true))




  ##################################### running to here gives useable marport format script with correct column names.
  ###### Now convert to Esonar format:


  ##### Use either TRUE or MAGNETIC heading:

  if(heading %in% "true"){
    dat <- dat %>% select(-Track_made_good_deg_magnetic)
    dat <- dat %>% rename(HEADING =Track_made_good_deg_true)
  }

  if(heading %in% "magnetic"){
    dat <- dat %>% select(-Track_made_good_deg_true)
    dat <- dat %>% rename(HEADING =Track_made_good_deg_magnetic)
  }

  #### Format CPUEDATEANDTIME and remove unused columns
  dat <- dat %>% select(-`Noise Floor`,-Periode,-RxKey, -Speed_over_ground_kph, -NMEA)

  #### create TRANSDUCERNAME values
  dat <- dat %>% mutate(TRANSDUCERNAME = ifelse(`Sensor Location` %in% "10", "HEADLINE",
                                                ifelse(`Sensor Location` %in% c("12","23","26"), "WINGSPREAD",
                                                       ifelse(`Sensor Location` %in% "171", "HYDROPHONE",
                                                              ifelse(`Sensor Location` %in% "8","GRID",`Sensor Location`)))))

  dat <- dat %>% select(-`Sensor Location`)

  ##rename remaining columns
  dat <- dat %>% rename(SENSORNAME = `Type of Data`,SENSORVALUE=Value,VALIDITY =Status,SIGNALSTRENGTH=SNR,SPEED = Speed_knots)

  ## reformat GPSDATE
  dat <- dat %>% mutate(GPSDATE= if_else(nchar(GPSDATE)==5,
                               parse_date(paste0("0",as.character(GPSDATE)),format = "%d%m%y"),
                               parse_date(as.character(GPSDATE),format = "%d%m%y")))

  ## remove redundant rows:

  dat <- unique(dat)

  ### add additional columns and arrange:

  dat$ERRORCODE = NA
  dat$HYDROPHONE = NA
  dat$DDLAT = NA
  dat$DDLON = NA
  dat$SET_NO = NA
  dat$TRIP_ID =NA
  dat$SOURCE = "MARPORT"

  dat <- dat[c("CPUDATEANDTIME",
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
               "GPSDATE",
               "DDLAT",
               "DDLON",
               "SOURCE")]

  
  ###### change NAs to white space
  dat <- sapply(dat, as.character)
  dat[is.na(dat)] <- ""
  dat <- as.data.frame(dat)

  ### additional formatting
  dat$SPEED = round(as.numeric(dat$SPEED), digits = 1)
  dat$SENSORVALUE = round(as.numeric(dat$SENSORVALUE), digits = 2)
  dat$SIGNALSTRENGTH = round(as.numeric(dat$SIGNALSTRENGTH), digits = 4)
  dat <- dat %>% relocate(SIGNALSTRENGTH, .after = last_col())
  dat$CPUDATEANDTIME = as_datetime(dat$CPUDATEANDTIME)
  dat$CPUDATEANDTIME = format(dat$CPUDATEANDTIME, "%a %b %d %H:%M:%S %Y")
  
  
  
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE )
  write.csv(dat, file = paste0(output_directory,"/",gsub(".csv","",i),"_converted.csv"), row.names = F)


  }


}
####################################

