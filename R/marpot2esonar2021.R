
##### IN PROGRESS: script for converting 2021 marport data to esonar format, will eventually be a function.

require(bio.lobster)
require(bio.utilities)
require(devtools)
library(lubridate)
library(tidyverse)
fpath = file.path(project.datadirectory('bio.lobster'),'data','survey','Marport 2021')

#### Read in file, separate columns and clean up headers
mar <- read.csv(file.path(fpath,'20210629.csv'), header = FALSE)
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
mar1 <- mar1 %>% mutate(Time1 = format(as.POSIXct(Time1, origin = "1970-01-01", tz = "Canada/Atlantic"), "%Y-%m-%d %H:%M:%OS3"))
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
    ooo = cbind(gpst$Time[j],gpst$NMEA[j],as.character(gpst$a[j]),as.character(gpst$c[j]),as.character(gpst$e[j]), NA,NA,NA,NA)
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

#### change GPSTIME from UTC to Local and add date column
dat <- dat %>% mutate(GPSTIME1 = parse_time(as.character(GPSTIME), "%H%M%S"))
dat <- dat %>% mutate(GPSDATE = as.Date(CPUDATEANDTIME))
dat <- dat %>% mutate(GPSDATETIME = ifelse(GPSTIME1 %in% NA, NA, paste(GPSDATE,"",GPSTIME1)))
dat <- dat %>% mutate(GPSTIME2 = as_datetime(ifelse(GPSDATETIME %in% NA, NA, as_datetime(GPSDATETIME) - 10800)))
dat <- dat %>% separate(GPSTIME2, c(NA,"GPSTIME3"), sep= " ")
dat <- dat %>% mutate(GPSTIME = gsub(":","",GPSTIME3))
dat <- dat %>% select(-GPSTIME1,-GPSDATETIME,-GPSTIME3)

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


##### check if column names need correcting (make this option when making function)
#correct.col.names=T
#correct.col.names=F

if(year(dat$CPUDATEANDTIME[1])=='2021' & month(dat$CPUDATEANDTIME[1]) =='6'){correct.col.names=T}

if(correct.col.names==T){
  dat <- dat%>% select(-SNR,-Status, -`Noise Floor`)
  dat <- dat %>% rename(SNR=Value, Status=`Type of Data`, Value= `Sensor Protocol`, `Type of Data`=`Sensor Location`, `Sensor Location`=Receiver, `Noise Floor`=Quality )
}

