#'Function for converting 2021 marport data to esonar format.
#' @param data_directory is file path to where unconverted Marport files are stored.
#' @param output_directory is file path to create directory where converted data will be stored.
#' @param correct.col.names June-July 2021 Marport data had incorrect column headers, if this problem is not resolved in later outputs, use correct.col.names=T (function will override to T if June-July 2021 data is being run)
#' @param heading chooses whether to use "true" or "magnetic" heading. Default = "magnetic"
#' @author Geraint Element
#' @export

marport2esonar2021 <- function(data_directory = file.path(bio.datadirectory,'bio.lobster','data','survey','Marport 2021'),
                               output_directory = file.path(data_directory,'Marport converted'),
                               correct.col.names=F,heading = "magnetic"){

# require(bio.lobster)
# require(bio.utilities)
require(devtools) || stop("install devtools")
require(lubridate) || stop("install lubridate")
require(tidyverse) || stop("install tidyverse")


#### Read in file, separate columns and clean up headers
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
    if(gpst$NMEA[j] == "$GPGGA"){
      ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$a[j]),as.character(gpst$b[j]),as.character(gpst$d[j]), NA,NA,NA,NA)
      out.gpst = rbind(out.gpst, ooo)
    }

    if(gpst$NMEA[j] == "$GPGLL"){
      ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$e[j]),as.character(gpst$a[j]),as.character(gpst$c[j]), NA,NA,NA,NA)
      out.gpst = rbind(out.gpst, ooo)
    }

    if(gpst$NMEA[j] == "$GPRMC"){
      ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$a[j]),as.character(gpst$c[j]),as.character(gpst$e[j]),as.character(gpst$h[j]),as.character(as.numeric(as.character(gpst$h[j]))+as.numeric(as.character(gpst$j[j]))),NA,NA)
      out.gpst = rbind(out.gpst, ooo)
    }

    if(gpst$NMEA[j] == "$GPVTG"){
      ooo = cbind(gpst$Time[j],gpst$NMEA[j],NA,NA,NA,as.character(gpst$a[j]),as.character(gpst$c[j]),as.character(gpst$e[j]),as.character(gpst$g[j]) )
      out.gpst = rbind(out.gpst, ooo)
    }

  }

  out.gpst = as.data.frame(out.gpst)
  out.gpst <- out.gpst %>% rename(Time = V1, NMEA = V2)

  #### Make table with just sensor data
  sensort <- mar1 %>% select(!(a:n))

  #### Recombine sensor and NMEA data into useful table
  dat <- left_join(sensort,out.gpst)

  #### clean up column headers
  dat <- dat %>% rename(CPUDATEANDTIME =Time, GPSTIME = V3, LATITUDE = V4, LONGITUDE = V5)
  dat <- dat %>% rename(Track_made_good_deg_true = V6, Track_made_good_deg_magnetic = V7, Speed_knots = V8, Speed_over_ground_kph = V9)
  dat$CPUDATEANDTIME = as_datetime(dat$CPUDATEANDTIME)

  #### change GPSTIME from UTC to Local and add date column
  # dat <- dat %>% mutate(GPSTIME1 = parse_time(as.character(GPSTIME), "%H%M%S"))
  dat <- dat %>% mutate(GPSDATE = as.Date(CPUDATEANDTIME))
  # dat <- dat %>% mutate(GPSDATETIME = ifelse(GPSTIME1 %in% NA, NA, paste(GPSDATE,"",GPSTIME1)))
  # dat <- dat %>% mutate(GPSTIME2 = as_datetime(ifelse(GPSDATETIME %in% NA, NA, as_datetime(GPSDATETIME) - 10800)))
  # dat <- dat %>% separate(GPSTIME2, c(NA,"GPSTIME3"), sep= " ")
  # dat <- dat %>% mutate(GPSTIME = gsub(":","",GPSTIME3))
  # dat <- dat %>% select(-GPSTIME1,-GPSDATETIME,-GPSTIME3)

  #### Remove GPRMB rows
  dat <- dat %>% filter(!(NMEA %in% "$GPRMB"))

  #### fill NA coordinates with proceeding Lat and Long values

  dat <- dat %>% mutate(lat1 = LATITUDE)
  dat <- dat %>% mutate(long1 = LONGITUDE)

  for (j in 1:length(dat$lat1)){
    dat$lat1[j] = ifelse(dat$lat1[j] %in% NA, as.character(dat$lat1[(j-1)]), as.character(dat$lat1[j]))
  }

  for (j in 1:length(dat$long1)){
    dat$long1[j] = ifelse(dat$long1[j] %in% NA, as.character(dat$long1[(j-1)]), as.character(dat$long1[j]))
  }

  #### format lat and long columns
  dat <- dat %>% mutate(lat1 = paste(str_sub(lat1, 1,2)," ",str_sub(lat1, 3,-1)," N"))
  dat <- dat %>% mutate(long1 = paste(str_sub(long1, 1,3)," ",str_sub(long1, 4,-1)," W"))
  dat <- dat %>% rename(lat_filled = lat1, long_filled = long1)

  #### Fill down GPSTIME, SPEED and HEADING values


  for (j in 1:length(dat$GPSTIME)){
    dat$GPSTIME[j] = ifelse(dat$GPSTIME[j] %in% NA, as.character(dat$GPSTIME[(j-1)]), as.character(dat$GPSTIME[j]))
  }

  for (j in 1:length(dat$Speed_knots)){
    dat$Speed_knots[j] = ifelse(dat$Speed_knots[j] %in% NA, as.character(dat$Speed_knots[(j-1)]), as.character(dat$Speed_knots[j]))
  }


  for (j in 1:length(dat$Track_made_good_deg_magnetic)){
    dat$Track_made_good_deg_magnetic[j] = ifelse(dat$Track_made_good_deg_magnetic[j] %in% NA, as.character(dat$Track_made_good_deg_magnetic[(j-1)]), as.character(dat$Track_made_good_deg_magnetic[j]))
  }


  for (j in 1:length(dat$Track_made_good_deg_true)){
    dat$Track_made_good_deg_true[j] = ifelse(dat$Track_made_good_deg_true[j] %in% NA, as.character(dat$Track_made_good_deg_true[(j-1)]), as.character(dat$Track_made_good_deg_true[j]))
  }


  ##### check if column names need correcting

  if(year(dat$CPUDATEANDTIME[1])=='2021' & month(dat$CPUDATEANDTIME[1]) %in% c('6','7')){correct.col.names=T}

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
  dat <- dat %>% select(-LATITUDE, -LONGITUDE,-`Noise Floor`,-Periode,-RxKey, -Speed_over_ground_kph, -NMEA) %>% rename(LATITUDE = lat_filled, LONGITUDE = long_filled)

  #### create TRANSDUCERNAME values
  dat <- dat %>% mutate(TRANSDUCERNAME = ifelse(`Sensor Location` %in% "10", "HEADLINE",
                                                ifelse(`Sensor Location`%in% "23" | `Sensor Location`%in% "26", "WINGSPREAD",
                                                       ifelse(`Sensor Location` %in% "171", "HYDROPHONE",
                                                              ifelse(`Sensor Location` %in% "8","GRID",`Sensor Location`)))))

  dat <- dat %>% select(-`Sensor Location`)

  ##rename remaining columns
  dat <- dat %>% rename(SENSORNAME = `Type of Data`,SENSORVALUE=Value,VALIDITY =Status,SIGNALSTRENGTH=SNR,SPEED = Speed_knots)

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



  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE )
  write.csv(dat, file = paste0(output_directory,"/",gsub(".csv","",i),"_converted.csv"), row.names = F)


  }


}
####################################

