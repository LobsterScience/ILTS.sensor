## NOTE: install dependancies (see Readme)


#assign('manual.archive', "", pkg.env)
# assign('oracle.server', oracle.personal.server, pkg.env)
# assign('oracle.user', oracle.personal.user, pkg.env)
# assign('oracle.password', oracle.personal.password, pkg.env)


#' @title  init.project.vars
#' @description  Set global database variables by user account
#' @import tcltk gWidgets2 gWidgets2tcltk
#' @export

init.project.vars = function() {

  require(devtools) || stop("install devtools")

  pkg.env = new.env(parent = emptyenv())

  if(exists("bio.datadirectory.ilts")){
    assign('manual.archive', bio.datadirectory.ilts , pkg.env)
  }
  else{
    assign('manual.archive', gWidgets2::gfile(text = "Select project work directory", type = "selectdir"), pkg.env)

  }

  #Take from snowcrab users .Rprofile.site
  if(exists("oracle.snowcrab.server")){
    assign('oracle.server', oracle.snowcrab.server, pkg.env)
    assign('oracle.user', oracle.snowcrab.user, pkg.env)
    assign('oracle.password', oracle.snowcrab.password, pkg.env)
  }
  #Take from lobster users .Rprofile.site, CHANGE TO WHAT IT IS ACTUALLY CALLED
  if(exists("oracle.personal.server")){
    assign('oracle.server', oracle.personal.server, pkg.env)
    assign('oracle.user', oracle.personal.user, pkg.env)
    assign('oracle.password', oracle.personal.password, pkg.env)
  }
  #If still not set prompt for values
  if(pkg.env$oracle.server == ""){
    options(guiToolkit="tcltk")
    assign('oracle.server',  gWidgets2::ginput("Enter your Oracle server (exa. ptran):"), pkg.env)
    assign('oracle.user',  gWidgets2::ginput("Enter your Oracle username:"), pkg.env)
    assign('oracle.password',  gWidgets2::ginput("Enter your Oracle passsword:"), pkg.env)

  }

  pkg.env <<- pkg.env
}

#' @title  esonar2df
#' @description  Change esonar data format into useable dataframe
#' @param esonar The esonar data to convert
#' @import lubridate
#' @return dataframe
#' @export
esonar2df = function(esonar = NULL, years=NULL, set_seabf=NULL) {
  names(esonar)
  #colnames(esonar) = c("CPUDateTime","GPSDate","GPSTime","Latitude","Longitude","Speed","Heading","Validity","TransducerName","SensorName","SensorValue","ErrorCode","Hydrophone","SignalStrength", "setno", "latedit", "trip", "datetime")

  esonar$primary = NA  #Headline
  #esonar$secondary = NA #Is nothing but may need in file
  esonar$wingspread = NA
  esonar$sensor_depth = NA
  esonar$sensor_depth1 = NA
  esonar$sensor_depth2 = NA
  #esonar$temperature = NA
  #esonar$STBDRoll = NA
  #esonar$STBDPitch = NA



  ## headline height
  if(years %in% "2021"){
    esonar$primary[which(esonar$SENSORNAME == 'SENSORDTB' & esonar$TRANSDUCERNAME=="HEADLINE")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "SENSORDTB" & esonar$TRANSDUCERNAME == "HEADLINE")]
    esonar$wingspread[which(esonar$SENSORNAME == 'DISTANCE' & esonar$TRANSDUCERNAME=="WINGSPREAD")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DISTANCE" & esonar$TRANSDUCERNAME == "WINGSPREAD")]
  }
  if(years %in% c("2020","2019","2018","2017","2014")){
    esonar$primary[which(esonar$SENSORNAME == "Headline" & esonar$TRANSDUCERNAME=="Primary")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "Headline" & esonar$TRANSDUCERNAME == "Primary")]
  }

  ###for sets with no depths in seabf, use depth readings from HEADLINE, NBTE and wingspread(PRP) sensors, clickable depth line will combine depth data from all available of these sources
#browser()
    if(all(is.na(set_seabf$DEPTHM))){
      #browser()
      depth.opts <<- c()

      for(i in unique(esonar$TRANSDUCERNAME)){

        trans.tab = esonar %>% filter(TRANSDUCERNAME %in% i & SENSORNAME %in% "DEPTH") %>% filter(!(SENSORVALUE %in% NA))
        if(nrow(trans.tab)>0){
          depth.opts <<- c(depth.opts,i)
        }

        }
    # PRP = esonar %>% filter(TRANSDUCERNAME %in% "PRP" & SENSORNAME %in% "DEPTH") %>% filter(!(SENSORVALUE %in% NA))
    # NBTE = esonar %>% filter(TRANSDUCERNAME %in% "NBTE" & SENSORNAME %in% "DEPTH") %>% filter(!(SENSORVALUE %in% NA))
    # HEADLINE = esonar %>% filter(TRANSDUCERNAME %in% "HEADLINE" & SENSORNAME %in% "DEPTH") %>% filter(!(SENSORVALUE %in% NA))
    # WINGSPREAD = esonar %>% filter(TRANSDUCERNAME %in% "WINGSPREAD" & SENSORNAME %in% "DEPTH") %>% filter(!(SENSORVALUE %in% NA))

    # PRPrange = as.numeric(difftime(PRP$timestamp[length(PRP$timestamp)],PRP$timestamp[1], units = "secs"))
    # if(length(PRPrange)==0){PRPrange=0}
    # NBTErange = as.numeric(difftime(NBTE$timestamp[length(NBTE$timestamp)], NBTE$timestamp[1], units = "secs"))
    # if(length(NBTErange)==0){NBTErange=0}
    # HEADLINErange = as.numeric(difftime(HEADLINE$timestamp[length(HEADLINE$timestamp)], HEADLINE$timestamp[1], units = "secs"))
    # if(length(HEADLINErange)==0){HEADLINErange=0}
    # WINGSPREADrange = as.numeric(difftime(WINGSPREAD$timestamp[length(WINGSPREAD$timestamp)], WINGSPREAD$timestamp[1], units = "secs"))
    # if(length(WINGSPREADrange)==0){WINGSPREADrange=0}

    #depth.range.max = which.max(c(PRPrange, NBTErange, HEADLINErange, WINGSPREADrange))

    depth.alts = NULL  ### for netmensuration alternate graph options if wanted

    esonar$sensor_depth[which(esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DEPTH")]

# for(i in 1:length(depth.opts)){
#   esonar$sensor_depth[which(esonar$TRANSDUCERNAME == depth.opts[i] & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == depth.opts[i] & esonar$SENSORNAME == "DEPTH")]
# }

    # esonar$sensor_depth[which(esonar$TRANSDUCERNAME == depth.alts[1] & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == depth.alts[1] & esonar$SENSORNAME == "DEPTH")]
    # esonar$sensor_depth[which(esonar$TRANSDUCERNAME == depth.alts[2] & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == depth.alts[2] & esonar$SENSORNAME == "DEPTH")]
    # esonar$sensor_depth[which(esonar$TRANSDUCERNAME == depth.alts[3] & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == depth.alts[3] & esonar$SENSORNAME == "DEPTH")]
    # esonar$sensor_depth[which(esonar$TRANSDUCERNAME == depth.alts[4] & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == depth.alts[4] & esonar$SENSORNAME == "DEPTH")]
    #
    ## code for choosing specificesonar depth source based on most data points - don't delete yet
    # depth.opts = c(difftime((esonar %>% filter(TRANSDUCERNAME %in% "PRP" & SENSORNAME %in% "DEPTH"))$timestamp[length],
    #               esonar %>% filter(TRANSDUCERNAME %in% "NBTE" & SENSORNAME %in% "DEPTH"),
    #               esonar %>% filter(TRANSDUCERNAME %in% "HEADLINE" & SENSORNAME %in% "DEPTH"))
    #                  #nrow(esonar %>% filter(TRANSDUCERNAME %in% "WINGSPREAD" & SENSORNAME %in% "DEPTH"))
    # if(which.max(depth.opts)==1){
    #   esonar$sensor_depth[which(esonar$TRANSDUCERNAME == "PRP" & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == "PRP" & esonar$SENSORNAME == "DEPTH")]
    #   depth.source <<- "PRP:DEPTH"
    #   }
    # if(which.max(depth.opts)==2){
    #   esonar$sensor_depth[which(esonar$TRANSDUCERNAME == "NBTE" & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == "NBTE" & esonar$SENSORNAME == "DEPTH")]
    #   depth.source <<- "NBTE:DEPTH"
    # }
    # if(which.max(depth.opts)==3){
    #   esonar$sensor_depth[which(esonar$TRANSDUCERNAME == "HEADLINE" & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == "HEADLINE" & esonar$SENSORNAME == "DEPTH")]
    #   depth.source <<- "HEADLINE:DEPTH"
    # }
    # if(which.max(depth.opts)==4){
    #   esonar$sensor_depth[which(esonar$TRANSDUCERNAME == "WINGSPREAD" & esonar$SENSORNAME == "DEPTH")] = esonar$SENSORVALUE[which(esonar$TRANSDUCERNAME == "WINGSPREAD" & esonar$SENSORNAME == "DEPTH")]
    #   depth.source <<- "WINGSPREAD:DEPTH"
    # }
#browser()
  }
  ###Wingspread
  if(years %in% c("2020","2019")){
    esonar$wingspread[which(esonar$SENSORNAME %in% c("STBDDoorMaster","DoorMaster") & esonar$TRANSDUCERNAME == "DoorSpread")] = esonar$SENSORVALUE[which(esonar$SENSORNAME %in% c("STBDDoorMaster","DoorMaster") & esonar$TRANSDUCERNAME == "DoorSpread")]
  }
  if(years %in% c("2018","2017","2014")){
    esonar$wingspread[which(esonar$SENSORNAME == "DoorMaster" & esonar$TRANSDUCERNAME == "DoorSpread")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DoorMaster" & esonar$TRANSDUCERNAME == "DoorSpread")]
  }
  if(years %in% c("2016","2015")){
    esonar$wingspread[which(esonar$SENSORNAME == "DISTANCE" & esonar$TRANSDUCERNAME == "PRP")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DISTANCE" & esonar$TRANSDUCERNAME == "PRP")]
  }

  # for future years, assume names are same as 2021, change this if needed:
  if(as.numeric(years)>2021){
    #esonar$primary[which(esonar$SENSORNAME == 'SENSORDTB' & esonar$TRANSDUCERNAME=="HEADLINE")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "SENSORDTB" & esonar$TRANSDUCERNAME == "HEADLINE")]
    ### in Fall 2023, primary (Opening) began coming from TRAWLEXPLORER:OPN (occasional values from HEADLINE:SENSORDTB but are probably erroneous; clicktouch should automatically filter these out)
    esonar$primary[which((esonar$SENSORNAME == 'OPN' & esonar$TRANSDUCERNAME=="TRAWLEXPLORER") | (esonar$SENSORNAME == 'SENSORDTB' & esonar$TRANSDUCERNAME=="HEADLINE"))] = esonar$SENSORVALUE[which((esonar$SENSORNAME == "OPN" & esonar$TRANSDUCERNAME == "TRAWLEXPLORER") | (esonar$SENSORNAME == "SENSORDTB" & esonar$TRANSDUCERNAME == "HEADLINE"))]
    esonar$wingspread[which(esonar$SENSORNAME == 'DISTANCE' & esonar$TRANSDUCERNAME=="WINGSPREAD")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DISTANCE" & esonar$TRANSDUCERNAME == "WINGSPREAD")]
  }

  #esonar$temperature[which(esonar$SensorName == "Temperature")] = esonar$SensorValue[which(esonar$SensorName == "Temperature")]
  #esonar$STBDRoll[which(esonar$SENSORNAME == "STBDRoll")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "STBDRoll")]
  #esonar$STBDPitch[which(esonar$SENSORNAME == "STBDPitch")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "STBDPitch")]
  #esonar$depth[which(esonar$SensorName == "Depth")] = esonar$SensorValue[which(esonar$SensorName == "Depth")]

  esonar$CPUDATETIME = NULL
  esonar$TRANSDUCERNAME = NULL
  esonar$SENSORNAME = NULL
  esonar$SENSORVALUE = NULL
  esonar$HYDROPHONE = NULL
  esonar$SIGNALSTRENGTH = NULL
  esonar$VALIDITY = NULL
  esonar$ERRORCODE = NULL
  esonar$HEADING = NULL


  esonar <- esonar %>% select(GPSDATE,GPSTIME,LATITUDE,LONGITUDE,SPEED,SET_NO,DDLAT,TRIP_ID,timestamp,primary,wingspread,sensor_depth,sensor_depth1,sensor_depth2)
  #####NOTE: DDLAT is probably the wrong column but didn't know what "latedit" was supposed to be so used DDLAT to fill that space, but doesn't seem to affect running of function - Geraint E.

  colnames(esonar) = c("Date","Time","Latitude","Longitude","Speed", "Setno", "latedit","Trip","timestamp", "Primary","WingSpread","SensorDepth","SensorDepth1","SensorDepth2")

  return(esonar)
}

#' @title  get.oracle.table
#' @description  Get data from Oracle Database table
#' @import ROracle DBI
#' @param tn tablename to get
#' @param oracle.user your oracle username
#' @param oracle.password your oracle password
#' @return dataframe
#' @export
get.oracle.table = function(tn = "",server = pkg.env$oracle.server, user =pkg.env$oracle.user, password = pkg.env$oracle.password){
  if(tn == "")stop("You must provide a tablename to 'get.oracle.table'!!")


  drv <- ROracle::Oracle()
  con <- ROracle::dbConnect(drv, username = user, password = password, dbname = server)
  res <- ROracle::dbSendQuery(con, paste("select * from ", tn, sep=""))
  res <- ROracle::fetch(res)
  ROracle::dbDisconnect(con)
  return(res)
}


#' @title  click_touch
#' @description  Get all data, format and merge.
#' @param  update TRUE: Add new startion and overwite any previous. FALSE: Skip previously completed set.nos.
#' @param user Unique user. This is used to keep user files seperate
#' @param years single or vector of years to process
#' @param skiptows skip specific tows specified by TRIP_ID:SET (example: skiptows = '100054289:51')
#' @param no.depth if real depth data is insufficient but still want to check headline distance and wingspread, run specified tows with dummy depth (example: no.depth = '100054289:51')
#' @import netmensuration lubridate
#' @return list of lists. Format (top to bottom) year-set-data
#' @export
click_touch = function(update = FALSE, user = "", years = "", skiptows = NULL, select.tows = NULL, dummy.depth = NULL, divert.messages = FALSE, bcp ){

  #Set up database server, user and password
  init.project.vars()

  #####set options for diverting console output
  outputfile <- file(file.path(pkg.env$manual.archive,"output.txt"), open = "a")
  sink(outputfile,type = "output", append = TRUE,split = TRUE)
  if(divert.messages){
    msgfile <- file(file.path(pkg.env$manual.archive,"messages.txt"), open = "a")
    sink(msgfile, type = "message")
  }

  on.exit(sink(type = "message"))
  on.exit(sink(type = "output"),add = TRUE)
  #####

  cont=TRUE
  if(user == "")stop("You must call this function with a user. exa. ilts.format.merge(update = TRUE, user = 'John'" )

  if(file.exists(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))){
    current = read.csv(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))
    current$set.no = as.character(current$set.no)
    current$trip = as.character(current$trip)
    current$start = paste(current$startdate,current$starttime)
    current$end = paste(current$enddate,current$endtime)
  }else{
    current=NULL
  }
  #Need this to come from rdata file based on user
  if(file.exists(file.path(pkg.env$manual.archive,  paste("iltsStats_",user, ".RDATA", sep = "")))){
    load(file.path(pkg.env$manual.archive,  paste("iltsStats_",user, ".RDATA", sep = "")))
  }else{
    iltsStats = list()
  }
  Sys.setenv(TZ = "America/Halifax")
  Sys.setenv(ORA_SDTZ = "America/Halifax")

  plotdata = F #Alternative plotting that we do not require

  #Pull in the sensor data, this will be formatted and looped thru by trip then set.
  esona = get.oracle.table(tn = paste0("LOBSTER.ILTS_SENSORS WHERE GPSDATE between to_date('",years,"','YYYY') and to_date('",years+1,"','YYYY')"))
  #the behaviour of date range queries from R to Oracle is inconsistent, if year in esona doesn't match that called by user, try reverse method:
  if(length(unique(year(esona$GPSDATE)))==0 ||!(unique(year(esona$GPSDATE)) %in% years) ){
    esona = get.oracle.table(tn = paste0("LOBSTER.ILTS_SENSORS WHERE GPSDATE between to_date('",years-1,"','YYYY') and to_date('",years,"','YYYY')"))
  }

  esona$GPSTIME[which(nchar(esona$GPSTIME)==5)] = paste("0", esona$GPSTIME[which(nchar(esona$GPSTIME)==5)], sep="")
  esona$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(esona$GPSDATE)), esona$GPSTIME, sep=" "), tz="UTC" )
  esona = esona[ order(esona$timestamp , decreasing = FALSE ),]
  err = which(is.na(esona$timestamp))
  if(length(err)>0)esona = esona[-err,]

  ##### Addition to solve LATITUDE data logging issues - Geraint E.
  require(tidyr)
  require(dplyr)
  esona = esona %>% mutate(LATITUDE = ifelse(substr(LATITUDE,5,5) %in% " ", paste0(substr(LATITUDE,1,4),substr(LATITUDE,6,nchar(LATITUDE))),
                                             ifelse(substr(LATITUDE,6,6) %in% " ", paste0(substr(LATITUDE,1,5),substr(LATITUDE,7,nchar(LATITUDE))),LATITUDE)))
  esona <- esona %>% mutate(LATITUDE = gsub("n","N",LATITUDE)) %>% mutate(LATITUDE = gsub("F","N",LATITUDE))
  esona = separate(esona, LATITUDE, c("a","b","c"), " ", remove = FALSE)
  esona <- esona %>% filter(!(b %in% ""))
  #test4 = esona %>% filter(!(c %in% "N"))
  esona <- esona %>% select(-a,-b,-c)

  #####
  esona$LATITUDE = format.lol(x = esona$LATITUDE)
  esona$LONGITUDE = format.lol(x = esona$LONGITUDE)
  #If specific years desired filter unwanted
  if(years != ""){
    # years = as.character(years)
    # yind = which(as.character(lubridate::year(esona$timestamp)) %in% years)
    # if(length(yind)>0)esona = esona[yind,]
    if(length(esona$timestamp)==0)warning("No ILTS_SENSOR data found for your year selection!", immediate. = TRUE)
  }


  # pull in temperature data
  seabf = get.oracle.table(tn =  paste0("LOBSTER.ILTS_TEMPERATURE WHERE UTCDATE between to_date('",years,"','YYYY') and to_date('",years+1,"','YYYY')"))
  if(length(unique(year(seabf$UTCDATE)))==0 ||!(unique(year(seabf$UTCDATE)) %in% years) ){
    seabf = get.oracle.table(tn = paste0("LOBSTER.ILTS_TEMPERATURE WHERE UTCDATE between to_date('",years-1,"','YYYY') and to_date('",years,"','YYYY')"))
  }

  #rebuild datetime column as it is incorrect and order
  seabf$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(seabf$UTCDATE)), seabf$UTCTIME, sep=" "), tz="UTC" )
  seabf = seabf[ order(seabf$timestamp , decreasing = FALSE ),]

  ## filter ILTS_TEMPERATURE dataset for desired years also, in case it needs to be used as the set reference
  if(years != ""){
     years = as.character(years)
    # yind = which(as.character(lubridate::year(seabf$timestamp)) %in% years)
    # if(length(yind)>0)seabf = seabf[yind,]
    if(length(seabf$timestamp)==0)warning("No ILTS_TEMPERATURE data found for your year selection!", immediate. = TRUE)
  }

###### this code block can be put anywhere, but should be before trip/set looping starts to avoid repeating query
  ## bring in additional tow info for user from iltssets_mv (gear, tow length), will add to interactive graph later
  new_con <- ROracle::dbConnect(drv = DBI::dbDriver("Oracle"),  username = oracle.username, password = oracle.password, dbname = "PTRAN")
  addit.tow.info <<- ROracle::dbGetQuery(new_con,
                                      "SELECT DISTINCT trip_id, set_no, gear, board_date, station,
                                round(6371 * 2 * asin(sqrt(power(sin((set_lat - abs(haul_lat)) * 3.1415926 / 180 / 2), 2) + cos(set_lat * 3.1415926 / 180) * cos(abs(haul_lat) * 3.1415926 / 180) * power(sin((set_long - haul_long) * 3.1415926 / 180 / 2), 2))), 2) distancekm,
                                round(3440.065 * 2 * asin(sqrt(power(sin((set_lat - abs(haul_lat)) * 3.1415926 / 180 / 2), 2) + cos(set_lat * 3.1415926 / 180) * cos(abs(haul_lat) * 3.1415926 / 180) * power(sin((set_long - haul_long) * 3.1415926 / 180 / 2), 2))), 2) distancenm
                            FROM lobster.iltssets_mv
                            WHERE board_date > to_date('2014-09-01','YYYY-MM-DD')
                            AND haulccd_id = 1
                            order by board_date,
                                trip_id,
                                set_no")
######

  ## remove tows selected by user
  esona <- esona %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  esona <- esona %>% filter(!(tow %in% skiptows))
  seabf <- seabf %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  seabf <- seabf %>% filter(!(tow %in% skiptows))

  #### OR filter for specific tow selected by user
if(!is.null(select.tows)){
  esona <- esona %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  esona <- esona %>% filter(tow %in% select.tows)
  seabf <- seabf %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  seabf <- seabf %>% filter(tow %in% select.tows)
}

   ## create a logic to reference ILTS_TEMPERATURE trip and set instead if ILTS_SENSORS data is missing for any tows
  reftemp = FALSE
  if(length(unique(seabf$tow)) > length(unique(esona$tow))){reftemp = TRUE}

  #Loop through each esonar (or seabf) file to convert and merge with temp
  if(reftemp){tripref = unique(seabf$TRIP_ID)}else{tripref = unique(esona$TRIP_ID)}


  for(i in tripref){
    if(cont){ #Condition fails if program exited
      trip_eson <- esona %>% filter(TRIP_ID %in% i)
      trip_seabf <- seabf %>% filter(TRIP_ID %in% i)
      if(nrow(trip_eson)==0){
        setref = unique(trip_seabf$SET_NO)
        warning(paste("No ILTS_SENSOR data found for Trip ID:",i,"!"), immediate. = TRUE)
      }else{
          if(length(unique(trip_seabf$SET_NO))>length(unique(trip_eson$SET_NO))){
            setref = unique(trip_seabf$SET_NO)
          }else{setref = unique(trip_eson$SET_NO)}
      }
      for(j in setref){
        if(cont){ #Condition fails if program exited
          set_eson <- esona %>% filter(TRIP_ID %in% i) %>% filter(SET_NO %in% j)
          set_seabf <- seabf %>% filter(TRIP_ID %in% i) %>% filter(SET_NO %in% j)
          if(nrow(set_eson)==0){
            warning(paste("No ILTS_SENSOR data found for TRIP ID:",set_seabf$TRIP_ID[1],"Set:",set_seabf$SET_NO[1],"!"), immediate. = TRUE)
          }
          ### filter for only raw sensor values (or "A" if esonar) and if none, use seabf as set
          set_eson <- set_eson %>% filter(VALIDITY %in% c("A","RAW",1000) | SENSORNAME  %in% "OPN")
          using.seabf.for.set = FALSE
          if(nrow(set_eson)==0 | nrow(set_eson %>% filter(!(SENSORVALUE %in% NA)))==0){
            using.seabf.for.set = TRUE
            set = set_seabf %>% select(SET_NO,TRIP_ID,timestamp) %>%
            mutate(Date=NA,Time=NA,Latitude=NA,Longitude=NA,Speed=NA,Setno=SET_NO,latedit=NA,Trip=TRIP_ID,
                   timestamp=timestamp,Primary=NA,WingSpread=NA,SensorDepth=NA) %>%
            select(-SET_NO,-TRIP_ID)
          warning(paste("No RAW ILTS_SENSOR data found for TRIP ID:",i,", SET:",j,"!"), immediate. = TRUE)
          }else{set = esonar2df(set_eson, years, set_seabf)}


          #Dont continue if update is false and set.no is already complete

          if((paste(unique(na.omit(set$Setno)), unique(na.omit(set$Trip)), sep = ".") %in% paste(current$set.no, current$trip, sep = ".")) & (update==FALSE)){
            message(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Already added call with update = TRUE to redo.", sep = ""))
          }else{
            # minisub = NULL
            # mini.ind.0 = which(mini$timestamp>set$timestamp[1])[1]
            # mini.ind.1 = which(mini$timestamp>set$timestamp[length(set$timestamp)])[1]-1
            # if(!(is.na(mini.ind.0) | is.na(mini.ind.0))){
        #   minisub = mini[c(mini.ind.0:mini.ind.1),]
            #   #### Only keep relevant data, If you encounter a minilog file
            #   #    depth, add that column to the following line to catch that case
            #   if("depth" %in% names(minisub)){
            #     minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C", "depth"))]
            #     names(minisub) = c("temperature","depth","timestamp")
            #   }
            #   else{
            #     minisub = minisub[,which(names(minisub) %in% c("datetime", "TEMP_C"))]
            #     names(minisub) = c("temperature","timestamp")
            #   }
            # }

            seabsub = NULL
            #Get seabird indicies and extend mins on either side so that depth profile isn't cut off
            # OR reference start and end of seabf directly when set = seabf (because set_eson has no data)

            #OR skip making seasub entirely and make dummy table if there's no seabf data
            if(nrow(set_seabf)==0 | nrow(set_seabf %>% filter(SOURCE %in% "NO_DATA")) == nrow(set_seabf)){
              seabsub = data.frame(temperature = rep(NA,nrow(set)), depth = rep(NA,nrow(set)), timestamp = set$timestamp)
            }else{

            if(using.seabf.for.set){
              seab.ind.0 = which(seabf$timestamp==set$timestamp[1])
              seab.ind.1 = which(seabf$timestamp==set$timestamp[length(set$timestamp)])
              }else{
                    seab.ind.0 = which(seabf$timestamp>set$timestamp[1]-lubridate::minutes(15))[1]

                    ## in case it is last tow of ILTS_TEMPERATURE
                    if(max(which(seabf$tow == set_eson$tow[1])) == max(as.numeric(rownames(seabf)))){
                    seab.ind.1 = max(as.numeric(rownames(seabf)))
                    if(is.null(select.tows)){print(paste("Last tow of ILTS_TEMPERATURE data set for",years,"!"))}
                    }else{
                    seab.ind.1 = which(seabf$timestamp>set$timestamp[length(set$timestamp)]+lubridate::minutes(15))[1]-1
                    }
            }

            if(all(is.na(seab.ind.0)) | all(is.na(seab.ind.1)))stop(paste0("No Date/Time alignment between ILTS_SENSOR and ILTS_TEMPERATURE tables for Trip:",set$Trip[1]," Set:",set$Setno[1]))


            if(!(all(is.na(seab.ind.0)) | all(is.na(seab.ind.1)))){

              seabsub = seabf[c(seab.ind.0:seab.ind.1),]

              ## trim any hangover into other tows:
              seabsub <- seabsub %>% mutate(tow = paste(TRIP_ID,SET_NO))
              set <- set %>% mutate(tow = paste(Trip,Setno))
              seabsub <- seabsub %>% filter(tow %in% set$tow[1])
              seabsub <- seabsub %>% select(-tow)
              set <- set %>% select(-tow)
              if(nrow(seabsub)==0)stop(paste0("No Date/Time alignment between ILTS_SENSOR and ILTS_TEMPERATURE tables for Trip:",set$Trip[1]," Set:",set$Setno[1]))
              ##

              seabsub = seabsub[,which(names(seabsub) %in% c("timestamp", "TEMPC", "DEPTHM"))]
              names(seabsub) = c("temperature","depth","timestamp")
            }



            }



            #### make a dummy depth parabola if there are no (or too few) depth data in any dataset
            parabola = FALSE
            no.depth = FALSE
            if(nrow(seabsub %>% filter(!(depth %in% NA)))<5 & nrow(set %>% filter(!(SensorDepth %in% NA)))<5){
              parabola=TRUE
              warning("Not enough depth data found in any dataset; netmensuration will use dummy depth parabola", immediate. = TRUE)}
            if(!is.null(dummy.depth) && dummy.depth == paste0(set$Trip[1],":",set$Setno[1])){
              parabola=TRUE
              warning("using dummy depth parabola at user's call", immediate. = TRUE)}
            if(parabola){
              seabsub$dum_time = as.numeric(rownames(seabsub)) - 0.5*length(seabsub$depth)
              seabsub$depth = 0.001*(-seabsub$dum_time^2)
              seabsub$depth = seabsub$depth + max(-seabsub$depth) + 3
              ### if sd(depth) is too small (because of small seabf dataset length, make a bigger parabola)
              if(sd(seabsub$depth)<1){
                seabsub$depth = 0.1*(-seabsub$dum_time^2)
                seabsub$depth = seabsub$depth + max(-seabsub$depth) + 3
                }
              no.depth = TRUE
            }



            if(is.null(seabsub) && is.null(minisub))stop(paste("No temperature/depth file found for trip - set:  ", unique(na.omit(set$Trip)), " - ", unique(na.omit(set$Setno)), sep=""))
        #    if(is.null(seabsub)) seabsub = minisub

            #Remove depths = <0 #Not sure why but came accross set.nos with low depth values mixed in with real bottom depths.
            seabsub$depth[which(seabsub$depth <= 2)] = NA

            #Merge sensor data.
            if(all(is.na(seabsub$depth))) mergset = set #if no depth in temp file dont merge amc Jan 31 2023
            if(any(!is.na(seabsub$depth))) mergset = merge(seabsub, set, "timestamp", all = TRUE)
            #Build the full, unbroken timeseries and merge
            timestamp = data.frame(seq(min(mergset$timestamp), max(mergset$timestamp), 1))
            names(timestamp) = c("timestamp")
            mergset = merge(mergset, timestamp, "timestamp", all = TRUE)
            mergset$timestamp = lubridate::ymd_hms(as.character(mergset$timestamp), tz="UTC" )
            #### if using sensor_depths as depth values because no seabf depths, replace here:
            if(all(is.na(mergset$depth))){
              mergset <- mergset %>% mutate(depth = SensorDepth) %>% select(-SensorDepth)
              # do some pre-filtering for these tows as sensor depths are erratic
              # make a loess model and remove depths that are obviously much larger than the model
              loess_depth_mod = loess(depth~as.numeric(timestamp), data=mergset, span = 0.75)
              loess_depth = predict(loess_depth_mod, data.frame(timestamp = mergset$timestamp))
              mergset = cbind(mergset,loess_depth)
              mergset <- mergset %>% mutate(depth = ifelse(depth - loess_depth > 50, NA, depth))
              mergset <- mergset %>% select(-loess_depth)
              warning(paste0("No depth values in ILTS_TEMPERTAURE for TRIP:",set$Trip[1]," Set:",set$Setno[1],", using DEPTHs from ",paste(depth.opts,collapse = ",")," from ILTS_SENSORS instead for clickable line."), immediate. = TRUE)

              # do the same for alternate depth sources
              if(nrow(mergset %>% filter(!(SensorDepth1 %in% NA)))>3){
              loess_alt1_mod = loess(SensorDepth1~as.numeric(timestamp), data=mergset, span = 0.75)
              loess_alt1 = predict(loess_alt1_mod, data.frame(timestamp = mergset$timestamp))
              mergset = cbind(mergset,loess_alt1)
              mergset <- mergset %>% mutate(SensorDepth1 = ifelse(SensorDepth1 - loess_alt1 > 50, NA, SensorDepth1))
              mergset <- mergset %>% select(-loess_alt1)
              }
              if(nrow(mergset %>% filter(!(SensorDepth2 %in% NA)))>3){
              loess_alt2_mod = loess(SensorDepth2~as.numeric(timestamp), data=mergset, span = 0.75)
              loess_alt2 = predict(loess_alt2_mod, data.frame(timestamp = mergset$timestamp))
              mergset = cbind(mergset,loess_alt2)
              mergset <- mergset %>% mutate(SensorDepth2 = ifelse(SensorDepth2 - loess_alt2 > 50, NA, SensorDepth2))
              mergset <- mergset %>% select(-loess_alt2)
              }
            }
            #Find deepest point and extend possible data from that out to 20min on either side
            aredown = mergset$timestamp[which(mergset$depth == max(mergset$depth, na.rm = T))]
            time.gate =  list( t0=as.POSIXct(aredown)-lubridate::dminutes(20), t1=as.POSIXct(aredown)+lubridate::dminutes(20) )


            # Build the variables need for the proper execution of the bottom contact function from
            # the netmensuration package
            
            bcp = list(
              set.no = unique(na.omit(set$Setno)),
              trip = unique(na.omit(mergset$Trip)),
              YR=as.character(unique(lubridate::year(mergset$timestamp))),
              user.interaction = TRUE,
              from.manual.archive = pkg.env$manual.archive,
              from.manual.file = file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")),
              id=paste("Trip:", unique(na.omit(mergset$Trip)), "  -  Set:", unique(na.omit(set$Setno)),sep=""),
              datasource="lobster",
              nr=nrow(mergset),
              tdif.min=2,
              tdif.max=32, #set based on longest tow encountered to date
              time.gate=time.gate,
              depth.min=3,
              depth.range=c(-40,90),#amc changed from -20 to -40 to pick up some tows that touch down was missed Jan 25 2024
              depthproportion=0.6,
              eps.depth=.4, ##Must set this low due to shallow tows resulting in small depth variability
              smooth.windowsize=5,
              modal.windowsize=5,
              noisefilter.trim=0.025,
              noisefilter.target.r2=0.85,
              noisefilter.quants=c(0.025, 0.975))
#browser()

            bcp = netmensuration::bottom.contact.parameters( bcp ) # add other default parameters .. not specified above


            names(mergset) = tolower(names(mergset))
            mergset$opening = mergset$primary

            #filter headline distance for positive values
            mergset <- mergset %>% mutate(opening = ifelse(opening < 0,0,opening))

            #Fix missing position data by repeating the last know position. No NA positions allowed in bottom.contact function
            #BUT, running tows with no esonar data means no coordinates, so in these cases insert a dummy coordinate
            #AND if depth data come from seabf and are too sparse, use loess smoothing instead to avoid too many duplicate values

             if(any(!is.na(set_seabf$depth)) & nrow(set_seabf)/nrow(set_eson) < 0.05){
              merg_loess = loess(depth~as.numeric(timestamp), data=mergset, span = 0.5)
              merg_depth = predict(merg_loess, data.frame(timestamp = mergset$timestamp))
              mergset$depth = merg_depth
              }else{

              if(all(is.na(mergset$latitude))){
                mergset$latitude[1] = 00.00000
                message("no latitude data found, using dummy values")
              }
              if(all(is.na(mergset$longitude))){
                mergset$longitude[1] = -00.00000
                message("no longitude data found, using dummy values")
              }
              for(k in 1:nrow(mergset)){
                if(k==1 && is.na(mergset$latitude[k])){
                  mergset$latitude[k] = mergset$latitude[!is.na(mergset$latitude)][1]
                  mergset$longitude[k] = mergset$longitude[!is.na(mergset$longitude)][1]
                }
                if(is.na(mergset$latitude[k])){
                  mergset$latitude[k] = mergset$latitude[k-1]
                  mergset$longitude[k] = mergset$longitude[k-1]
                }

                #Fix missing depth data by repeating the last know depth. No NA depth allowed in bottom.contact function
                if(k==1 && is.na(mergset$depth[k])){
                  mergset$depth[k] = mergset$depth[!is.na(mergset$depth)][1]
                }
                if(is.na(mergset$depth[k])){
                  mergset$depth[k] = mergset$depth[k-1]
                }
              }

              }

            ###filter out any depth = 0 rows:
            mergset <- mergset %>% filter(depth>0)


            ### filter Doorspread values for minimum and maximum spread and filter based on gear type
            tow.info <- addit.tow.info %>% filter(TRIP_ID %in% set$Trip & SET_NO %in% set$Setno)
            if(tow.info$GEAR %in% "280 BALLOON"){
              mergset <- mergset %>% mutate(wingspread = ifelse(wingspread>30,30,
                                                                ifelse(wingspread<5,5,wingspread)))
            }
            if(tow.info$GEAR %in% "NEST"){
              mergset <- mergset %>% mutate(wingspread = ifelse(wingspread>20,20,
                                                                ifelse(wingspread<5,5,wingspread)))
            }

            mergset$doorspread = mergset$wingspread

            #Try to recover from user termination in order to write the current stat list to file

#browser()
            tryCatch(
               # print(mergset$depth)
              {
#browser()

                bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )


                if ( is.null(bc) || ( !is.null(bc$res)  && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  bcp$noisefilter.target.r2=0.75
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  mergset$depth = jitter( mergset$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inlah.h = 0.1
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  mergset$depth = jitter( mergset$depth, amount = bcp$eps.depth/10 )
                  bcp$noisefilter.inla.h = 0.25
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                }
                eps.depth.backup = NULL
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  eps.depth.backup = bcp$eps.depth
                  for(i in 1:6){
                  if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  bcp$eps.depth = bcp$eps.depth - 0.01
                  bc = netmensuration::bottom.contact(mergset, bcp, debugrun=FALSE )
                  }
                 }
                }

                eps.depth.final = bcp$eps.depth
                if(!is.null(eps.depth.backup)){bcp$eps.depth = eps.depth.backup}
#browser()
              ##### if bc is not NULL: let user know netmensuration is done, check for error flags, then update RDATA stats file
                if(!is.null(bc)){
                  #### make sure 1st column of output is called "set.no" (crab uses "station")
                  # csv.file <- read.csv(bcp$from.manual.file, as.is = TRUE)
                  # names(csv.file)[1] = "set.no"
                  # write.csv(csv.file,bcp$from.manual.file, row.names = FALSE)
                  ####
                  if(is.na(bc$error.flag)){message(paste("Done"," Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),", Clicktouchdown file updated in: ", pkg.env$manual.archive, sep=""))}
                  if(bc$error.flag %in% 'Too much data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Too much data? Time range may exceed maximum tow length. Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Not enough data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Not enough good data? Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'not enough variability in data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Not enough variability in depth data? Tow may be too shallow or not enough depth data. Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Time track is too short?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Tow may be too short/not enough good data? Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Time track is too long?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Tow may be too long. Netmensuration not completed!"))}

                  if(bc$error.flag %in% 'not enough variability in data?'){message(paste0("can use dummy.depth=",dQuote(paste0(set$Trip[1],":",set$Setno[1]))," to run with dummy depths"))}
                  #### remove depth data from R file and csv file if dummy depths were used
                  if(no.depth){
                    bc$plotdata$depth = NA
                    bc$depth.mean = NA
                    bc$depth.sd = NA
                    bc$depth.n = NA
                    bc$depth.n.bad = NA
                    bc$depth.smoothed.mean = NA
                    bc$depth.smoothed.sd = NA
                    bc$depth.filtered = NA
                    bc$depth.smoothed = NA
                    bc$error.flag = "No Depth data!"

                    csv.file <- read.csv(bcp$from.manual.file, as.is = TRUE)
                    csv.file <- csv.file %>% mutate(depth = ifelse(trip %in% bcp$trip & set.no %in% bcp$set.no,NA, depth))
                    write.csv(csv.file,bcp$from.manual.file, row.names = FALSE)
                  }
                  ##
                  iltsStats[[paste(unique(na.omit(set$Trip)), unique(na.omit(set$Setno)),sep=".")]] = bc
                }
              ##### if bc is still NULL, list warnings with possible reasons
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  warning(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Touchdown metrics could not be calculated. Possible reason: tow may be too shallow or too deep, resulting in too little or too much variation in depth values; if suspect too shallow, check that sd(mergset$depth) > bcp$eps.depth, currently function tries reducing eps.depth as low as ",eps.depth.final," before quitting. If suspect too deep, try increasing bcp$depth.range (currently set at:",bcp$depth.range[1]," to ",bcp$depth.range[2],")", sep = ""), immediate. = TRUE)
                }

              },
              error=function(cond) {
                cont <<- FALSE
                message(cond)
                warning(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), "not completed"), immediate. = TRUE)
              }
            )


          }#END Update clause
        }
      }#END Set subset
    }
  }#END Trip subset
  save(iltsStats, file = file.path(pkg.env$manual.archive, paste("iltsStats_",user, ".RDATA", sep = "")))
  message(paste("iltsStats file saved to: ", pkg.env$manual.archive, sep=""))
}



#' @title  format.lol
#' @description  Take latitude or longitude character vector and convert to decimal degrees.
#' @param x latitude ot longitude values to convert
#' @param lol Specify if you are converting latitude or longitude
#' @import stringr
#' @return list of correct latitude or longitude
#' @export
format.lol = function(x = NULL){
  if(is.null(x))stop("You must specify values: x")
  x = as.character(x)
  x = matrix(unlist(strsplit(x, " ")), ncol = 3, byrow = T)
  rx = as.numeric(x[,1]) + (as.numeric(x[,2])/60)
  if(any(grepl("S", x[1,])) || any(grepl("W", x[1,]))){
    rx = abs(rx)*-1
  }
  return(rx)
}

#####Execute

##click_touch(update = FALSE, user = "geraint", years = "2021" )

