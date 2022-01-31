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
esonar2df = function(esonar = NULL, years=NULL) {
  names(esonar)
  #colnames(esonar) = c("CPUDateTime","GPSDate","GPSTime","Latitude","Longitude","Speed","Heading","Validity","TransducerName","SensorName","SensorValue","ErrorCode","Hydrophone","SignalStrength", "setno", "latedit", "trip", "datetime")

  esonar$primary = NA  #Headline
  #esonar$secondary = NA #Is nothing but may need in file
  esonar$wingspread = NA
  #esonar$depth = NA
  #esonar$temperature = NA
  #esonar$STBDRoll = NA
  #esonar$STBDPitch = NA

  #browser()
  ## headline height
  if(years %in% "2021"){
    esonar$primary[which(esonar$SENSORNAME == 'SENSORDTB' & esonar$TRANSDUCERNAME=="HEADLINE")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "SENSORDTB" & esonar$TRANSDUCERNAME == "HEADLINE")]
    esonar$wingspread[which(esonar$SENSORNAME == 'DISTANCE' & esonar$TRANSDUCERNAME=="WINGSPREAD")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "DISTANCE" & esonar$TRANSDUCERNAME == "WINGSPREAD")]
  }
  if(years %in% c("2020","2019","2018","2017","2014")){
    esonar$primary[which(esonar$SENSORNAME == "Headline" & esonar$TRANSDUCERNAME=="Primary")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "Headline" & esonar$TRANSDUCERNAME == "Primary")]
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
    esonar$primary[which(esonar$SENSORNAME == 'SENSORDTB' & esonar$TRANSDUCERNAME=="HEADLINE")] = esonar$SENSORVALUE[which(esonar$SENSORNAME == "SENSORDTB" & esonar$TRANSDUCERNAME == "HEADLINE")]
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


  esonar <- esonar %>% select(GPSDATE,GPSTIME,LATITUDE,LONGITUDE,SPEED,SET_NO,DDLAT,TRIP_ID,timestamp,primary,wingspread)
  #####NOTE: DDLAT is probably the wrong column but didn't know what "latedit" was supposed to be so used DDLAT to fill that space, but doesn't seem to affect running of function - Geraint E.

  colnames(esonar) = c("Date","Time","Latitude","Longitude","Speed", "Setno", "latedit","Trip","timestamp", "Primary","WingSpread")

  return(esonar)
}

#' @title  get.acoustic.releases
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


#' @title  ilts.format.merge
#' @description  Get all data, format and merge.
#' @param  update TRUE: Add new startion and overwite any previous. FALSE: Skip previously completed stations.
#' @param user Unique user. This is used to keep user files seperate
#' @param years single or vector of years to process
#' @param skiptows skip specific tows specified by TRIP_ID:SET (example: skiptows = '100054289:51')
#' @import netmensuration lubridate
#' @return list of lists. Format (top to bottom) year-set-data
#' @export
click_touch = function(update = TRUE, user = "", years = "", skiptows = NULL){
  #Set up database server, user and password
  init.project.vars()

  cont=TRUE
  if(user == "")stop("You must call this function with a user. exa. ilts.format.merge(update = TRUE, user = 'John'" )

  if(file.exists(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))){
    current = read.csv(file.path(pkg.env$manual.archive, paste("clicktouchdown_", user, ".csv", sep="")))
    current$station = as.character(current$station)
    current$trip = as.character(current$trip)
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
  #esona = get.oracle.table(tn = "LOBSTER.ILTS_SENSORS")
  #esona_fallback <<- esona
  esona <- esona_fallback
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
    years = as.character(years)
    yind = which(as.character(lubridate::year(esona$timestamp)) %in% years)
    if(length(yind)>0)esona = esona[yind,]
    if(length(esona$timestamp)==0)warning("No ILTS_SENSOR data found for your year selection!", immediate. = TRUE)
  }
  # mini = get.oracle.table(tn = "FRAILC.MINILOG_TEMP")
  # #rebuild datetime column as it is incorrect and order
  # mini$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(mini$TDATE)), mini$TIME, sep=" "), tz="UTC" )
  # mini = mini[ order(mini$timestamp , decreasing = FALSE ),]

  #seabf = get.oracle.table(tn = "LOBSTER.ILTS_TEMPERATURE")
  #seabf_fallback <<- seabf
  seabf <- seabf_fallback
   #rebuild datetime column as it is incorrect and order
  seabf$timestamp = lubridate::ymd_hms(paste(as.character(lubridate::date(seabf$UTCDATE)), seabf$UTCTIME, sep=" "), tz="UTC" )
  seabf = seabf[ order(seabf$timestamp , decreasing = FALSE ),]

  ## filter ILTS_TEMPERATURE dataset for desired years also, in case it needs to be used as the set reference
  if(years != ""){
    years = as.character(years)
    yind = which(as.character(lubridate::year(seabf$timestamp)) %in% years)
    if(length(yind)>0)seabf = seabf[yind,]
    if(length(seabf$timestamp)==0)warning("No ILTS_TEMPERATURE data found for your year selection!", immediate. = TRUE)
  }
  ## remove tows selected by user
  esona <- esona %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  esona <- esona %>% filter(!(tow %in% skiptows))
  seabf <- seabf %>% mutate(tow = paste0(TRIP_ID,":",SET_NO))
  seabf <- seabf %>% filter(!(tow %in% skiptows))

 #browser()
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
        }else{setref = unique(trip_eson$SET_NO)}
      for(j in setref){
        if(cont){ #Condition fails if program exited
          set_eson <- esona %>% filter(TRIP_ID %in% i) %>% filter(SET_NO %in% j)
          set_seabf <- seabf %>% filter(TRIP_ID %in% i) %>% filter(SET_NO %in% j)
          if(nrow(set_eson)==0){
            set = set_seabf %>% select(SET_NO,TRIP_ID,timestamp) %>%
            mutate(Date=NA,Time=NA,Latitude=NA,Longitude=NA,Speed=NA,Setno=SET_NO,latedit=NA,Trip=TRIP_ID,
                   timestamp=timestamp,Primary=NA,Secondary=NA,WingSpread=NA,Roll=NA,Pitch=NA) %>%
            select(-SET_NO,-TRIP_ID)
          warning(paste("No ILTS_SENSOR data found for TRIP ID:",i,", SET:",j,"! Netmensuration will use dummy Lat and Long values"), immediate. = TRUE)
          }else{set = esonar2df(set_eson, years)}


          #Dont continue if update is false and station is already complete
#browser()
          if((paste(unique(na.omit(set$Setno)), unique(na.omit(set$Trip)), sep = ".") %in% paste(current$station, current$trip, sep = ".")) & (update==FALSE)){
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
#browser()
            seabsub = NULL
            #Get seabird indicies and extend mins on either side so that depth profile isn't cut off
            # OR reference start and end of seabf directly when set = seabf (because set_eson has no data)

            #OR skip making seasub entirely and make dummy table if there's no seabf data
            if(nrow(set_seabf)==0 | nrow(set_seabf %>% filter(SOURCE %in% "NO_DATA")) == nrow(set_seabf)){
              seabsub = data.frame(temperature = rep(NA,nrow(set)), depth = rep(NA,nrow(set)), timestamp = set$timestamp)
            }else{

            if(nrow(set_eson)==0){
              seab.ind.0 = which(seabf$timestamp==set$timestamp[1])
              seab.ind.1 = which(seabf$timestamp==set$timestamp[length(set$timestamp)])
              }else{
                    seab.ind.0 = which(seabf$timestamp>set$timestamp[1]-lubridate::minutes(15))[1]

                    ## in case it is last tow of ILTS_TEMPERATURE
                    if(max(which(seabf$tow == set_eson$tow[1])) == max(as.numeric(rownames(seabf)))){
                    seab.ind.1 = max(as.numeric(rownames(seabf)))
                    print(paste("Last tow of ILTS_TEMPERATURE data set for",years,"!"))
                    }else{
                    seab.ind.1 = which(seabf$timestamp>set$timestamp[length(set$timestamp)]+lubridate::minutes(15))[1]-1
                    }
            }

            if(is.na(seab.ind.0) | is.na(seab.ind.1))stop(paste0("No Date/Time alignment between ILTS_SENSOR and ILTS_TEMPERATURE tables for Trip:",set$Trip[1]," Set:",set$Setno[1]))


            if(!(is.na(seab.ind.0) | is.na(seab.ind.1))){

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


#browser()
            #### make a dummy depth parabola if there are no depth data
            no.depth = FALSE
            if(all(is.na(seabsub$depth))){
              seabsub$dum_time = as.numeric(rownames(seabsub)) - 0.5*length(seabsub$depth)
              seabsub$depth = 0.001*(-seabsub$dum_time^2)
              seabsub$depth = seabsub$depth + max(-seabsub$depth) + 3
              ### if sd(depth) is too small (because of small seabf dataset length, make a bigger parabola)
              if(sd(seabsub$depth)<1){
                seabsub$depth = 0.1*(-seabsub$dum_time^2)
                seabsub$depth = seabsub$depth + max(-seabsub$depth) + 3
                }

             # ggplot(seabsub,aes(x=dum_time,y=depth))+
             #      geom_point()
              warning("No Depth data; netmensuration will use dummy depth parabola", immediate. = TRUE)
              no.depth = TRUE
            }



            if(is.null(seabsub) && is.null(minisub))stop(paste("No temperature/depth file found for trip - set:  ", unique(na.omit(set$Trip)), " - ", unique(na.omit(set$Setno)), sep=""))
        #    if(is.null(seabsub)) seabsub = minisub

            #Remove depths = <0 #Not sure why but came accross stations with low depth values mixed in with real bottom depths.
            seabsub$depth[which(seabsub$depth <= 2)] = NA


#browser()
            #Merge sensor data.
            mergset = merge(seabsub, set, "timestamp", all = TRUE)
            #Build the full, unbroken timeseries and merge
            timestamp = data.frame(seq(min(mergset$timestamp), max(mergset$timestamp), 1))
            names(timestamp) = c("timestamp")
            mergset = merge(mergset, timestamp, "timestamp", all = TRUE)
            mergset$timestamp = lubridate::ymd_hms(as.character(mergset$timestamp), tz="UTC" )
            #Find deepest point and extend possible data from that out to 20min on either side
            aredown = mergset$timestamp[which(mergset$depth == max(mergset$depth, na.rm = T))]
            time.gate =  list( t0=as.POSIXct(aredown)-lubridate::dminutes(20), t1=as.POSIXct(aredown)+lubridate::dminutes(20) )

            #### make a dummy depth parabola if there are no depth data
            # no.depth = FALSE
            # if(all(is.na(mergset$depth))){
            #   mergset$dum_time = as.numeric(rownames(mergset)) - 0.5*length(mergset$depth)
            #   mergset$depth = 0.001*(-mergset$dum_time^2)
            #   mergset$depth = mergset$depth + max(-mergset$depth) + 2
            #   # ggplot(seabsub,aes(x=dum_time,y=depth))+
            #   #   geom_point()
            #   warning("No Depth data; netmensuration will use dummy depth parabola", immediate. = TRUE)
            #   no.depth = TRUE
            # }

            # Build the variables need for the proper execution of the bottom contact function from
            # the netmensuration package
            bcp = list(
              station = unique(na.omit(set$Setno)),
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
              depth.range=c(-20,30),
              depthproportion=0.6,
              eps.depth=.5, ##Must set this low due to shallow tows resulting in small depth variability
              smooth.windowsize=5,
              modal.windowsize=5,
              noisefilter.trim=0.025,
              noisefilter.target.r2=0.85,
              noisefilter.quants=c(0.025, 0.975))

            bcp = netmensuration::bottom.contact.parameters( bcp ) # add other default parameters .. not specified above

            names(mergset) = tolower(names(mergset))
            mergset$opening = mergset$primary

            #Fix missing position data by repeating the last know position. No NA positions allowed in bottom.contact function
            #BUT, running tows with no esonar data means no coordinates, so in these cases insert a dummy coordinate
            #AND if depth data are too sparse, use loess smoothing instead to avoid many duplicate values
#browser()
             if(nrow(set_seabf)/nrow(set_eson) < 0.05){
              merg_loess = loess(depth~as.numeric(timestamp), data=mergset)
              merg_depth = predict(merg_loess, data.frame(timestamp = mergset$timestamp))
              mergset$depth = merg_depth
              }else{

              if(nrow(set_eson)==0){
                mergset$latitude[1] = 00.00000
                mergset$longitude[1] = -00.00000
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



            ### filter Doorspread values for minimum and maximum spread
            mergset <- mergset %>% mutate(wingspread = ifelse(wingspread>20,20,
                                                              ifelse(wingspread<5,5,wingspread)))


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

              ##### if bc is not NULL: let user know netmensuration is done, check for error flags, then update RDATA stats file
                if(!is.null(bc)){
                  message(paste("Done"," Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),", Clicktouchdown file updated in: ", pkg.env$manual.archive, sep=""))
                  if(bc$error.flag %in% 'Too much data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Too much data? Time range may exceed maximum tow length. Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Not enough data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Not enough good data? Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'not enough variability in data?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Not enough variability data? Tow may be too shallow. Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Time track is too short?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Tow may be too short/not enough good data? Netmensuration not completed!"))}
                  if(bc$error.flag %in% 'Time track is too long?'){message(paste("error.flag for:","Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)),"Tow may be too long. Netmensuration not completed!"))}
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
                    csv.file <- csv.file %>% mutate(depth = ifelse(trip %in% bcp$trip & station %in% bcp$station,NA, depth))
                    write.csv(csv.file,bcp$from.manual.file, row.names = FALSE)
                  }
                  ##
                  iltsStats[[paste(unique(na.omit(set$Trip)), unique(na.omit(set$Setno)),sep=".")]] = bc
                }
              ##### if bc is still NULL, list warnings with possible reasons
                if ( is.null(bc) || ( !is.null(bc$res) && ( ( !is.finite(bc$res$t0 ) || !is.finite(bc$res$t1 ) ) ) )) {
                  warning(paste("Set:",unique(na.omit(set$Setno))," Trip:", unique(na.omit(set$Trip)), " Touchdown metrics could not be calculated. Possible reason: tow may be too shallow, resulting in too little variation in depth values; check that sd(mergset$depth) > bcp$eps.depth, currently function tries reducing eps.depth as low as ",eps.depth.final," before quitting", sep = ""), immediate. = TRUE)
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

##click_touch(update = TRUE, user = "geraint", years = "2021" )

