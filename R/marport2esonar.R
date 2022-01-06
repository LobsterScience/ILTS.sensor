#' this function converts the marport data tables produced by ILTSMarportSummary.r into Esonar format
#' @param fpath is the path to where marport text data files are stored
#' @param year specifies the marport survey year for which R tables will be converted
#' @author Geraint Element
#' @export

marport2esonar <- function(fpath = file.path("C:","bio.data",'bio.lobster','data','survey'), year= year) {

#library(plyr)
require(dplyr) || stop("Install dplyr")
require(stringi) || stop("Install stringi")
require(lubridate) || stop("Install lubridate")

########## change year or directories if applicable


load(file.path(fpath,paste0('marport.',year,'.rdata')))
load(file.path(fpath,paste0('marport.gps.',year,'.rdata')))

#### clean reporting irregularities
# gps$X_clean = gsub("W","",gps$X)
# gps$X_clean = as.numeric(gsub(" ","",gps$X_clean))
# gps$Y_clean = gsub("N","",gps$Y)
# gps$Y_clean = as.numeric(gsub(" ","",gps$Y_clean))
# clean <- gps %>% filter(X_clean %in% NA | Y_clean %in% NA)
#### gps prep

gps <- gps %>% mutate(Date = sub("00","20",Date))
gps <- gps %>% mutate(CPUDATEANDTIME =paste0(Date," ",CPU.Time))
gps$CPUDATEANDTIME = do.call(rbind,strsplit(gps$CPUDATEANDTIME, "\\."))[,1]

gps$Ya = substr(gps$Y,1,2)
gps$Yb = substr(gps$Y, 3, nchar(gps$Y))
gps$Xa = substr(gps$X,1,3)
gps$Xb = substr(gps$X, 4, nchar(gps$X))

gps$Y = paste0(gps$Ya," ",gps$Yb)
gps$X = paste0(gps$Xa," ",gps$Xb)


## drop GPGGA rows unless no GPRMC data exists for that second

gps <- gps %>% group_by(CPUDATEANDTIME) %>% mutate(
  keepall = ifelse(all(Speed %in% NA) & all(Heading %in% NA), "yes","no")
) %>% ungroup()

gps <- gps %>% group_by(CPUDATEANDTIME) %>% mutate(
  keep = ifelse((keepall %in% "no" & Speed %in% NA & Heading %in% NA),
  "no", "yes")) %>% ungroup()

gps <- gps %>% filter(keep %in% "yes")

gps <- gps %>% select(-Date, -keepall, -keep)

###test for multiple gps recordings in a second
gps <- gps %>% group_by(CPUDATEANDTIME) %>% mutate(ngps=length(CPUDATEANDTIME)) %>% ungroup()

## keep only first row of a second where this is true
gps <- gps %>% group_by(CPUDATEANDTIME) %>% mutate(keep.first = ifelse((ngps>1 & row_number()==1), "yes",
                                                                   ifelse((ngps>1 & row_number()>1),"no","yes")))
gps <- gps %>% filter(keep.first %in% "yes")


#### sensors prep

sensors <- sensors %>% mutate(Date = sub("00","20",Date))
sensors <- sensors %>% mutate(CPUDATEANDTIME =paste0(Date," ",CPU_Time))
sensors$CPUDATEANDTIME = do.call(rbind,strsplit(sensors$CPUDATEANDTIME, "\\."))[,1]
sensors$CPU_Time = do.call(rbind,strsplit(sensors$CPU_Time, "\\."))[,1]



###### join tables by time

x <- full_join(gps, sensors)

#### finishing touches
x <- x %>% select(GPS.Time,Y,X,Speed,Heading,Set_no,CPUDATEANDTIME,Date,Info2,Measure,Validity,X2,Info3) %>%
  rename(GPSTIME = GPS.Time, GPSDATE = Date, LATITUDE =Y, LONGITUDE = X, VALIDITY = Validity, TRANSDUCERNAME = Info2,
         SENSORNAME = Measure, SENSORVALUE = X2, HYDROPHONE = Info3, SET_NO = Set_no, SPEED = Speed, HEADING = Heading) %>%
  mutate(ERRORCODE = NA, SIGNALSTRENGTH = NA, TRIP_ID = NA)

#x <- x %>% mutate(VALIDITY = ifelse(VALIDITY >=1000 & VALIDITY < 1010, "Raw", ifelse(VALIDITY >= 1010 & VALIDITY <= 1011,"Filtered",
#                                                                              ifelse(VALIDITY ==1001,"Rejected", VALIDITY))))

x <- x %>% mutate(LATITUDE = as.character(LATITUDE), LONGITUDE = as.character(LONGITUDE))

x <- x %>% mutate(GPSDATE =ifelse(GPSDATE %in% NA,as.character(date(as_datetime(CPUDATEANDTIME))),GPSDATE))

x <- x[c("CPUDATEANDTIME",
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

##### output is to survey/year/marport2esonar folder
out.dir = file.path(fpath,year,"marport2esonar")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE )

write.csv(x, file = file.path(out.dir,paste0(year,".csv")), row.names = F)

}

