#' LobsterMarport: Reads data from non-tabular Marport reports and converts this into two separate tables containing GPS and sensor data
#' @param file is the marport file obtained during the 2015 ILTS survey which contains the net sensor information as well as the gps data feeds
#' @author Adam Cook
#' @return returns a list containing two elements: 1) the trawl sensor information 2) the gps track of the file
#' @export
LobsterMarport <- function(file,station=NULL) {
	#reading in and manipulating the marport data from lobster survey
	con = file(description=file, open="r")
	options(stringsAsFactors=F)	
	OSinfo = Sys.info()
	OS = OSinfo["sysname"]


if(OS=='Linux')	{com = paste("wc -l ", file, " | awk '{ print $1 }'", sep=""); n = system(command=com, intern=TRUE)}
if(OS=='Windows') {n =  length(readLines(file))}
	  

		jj=0
		out.sensors = NULL
		out.gps = NULL
		out.gps1 = NULL
		out.date = NULL
		out.gps.time = NULL
		out.gps.time1 = NULL
		Collector = NULL


for(j in 1:n) {
		  tmp <- scan(file=file, skip=jj, nlines=1, quiet=TRUE,what='complex')
	
  		if(!is.na(tmp[4]) & length(tmp)>0) {
  				if(tmp[4]=='INFO' & tmp[6] %in% c('M4O_DATA')) {
  					if(tmp[10] %in% c('CAPACITY_VOLTA','PITCH','BEAM','TEMPERATURE','DEPTH','DISTANCE',"ROLL")) {
  						if(tmp[10]=='BEAM') {tmp[10] <- paste(tmp[10],tmp[11],sep="."); tmp = tmp[-11]}
  						out.sensors = rbind(out.sensors,tmp)
  			}		
  					if(!tmp[10] %in% c('CAPACITY_VOLTA','PITCH','BEAM','TEMPERATURE','DEPTH','DISTANCE',"ROLL")) {
  						Collector = c(Collector,tmp[10])
  						}
	  			}	
			  		
  		  

  		     if(any(strsplit(tmp[4],",")[[1]] %in% c('$GPGLL'))) {
  		     	out.gps1 = rbind(out.gps1,c(tmp[1],strsplit(tmp[4],",")[[1]][c(2,3,4,5,6)]))
  		     	
  		  	} 
  		  
  		  if(any(strsplit(tmp[4],",")[[1]] %in% c('$GPZDA'))) {
  		    ooo = c(tmp[1],strsplit(tmp[4],",")[[1]][2],paste(strsplit(tmp[4],",")[[1]][c(3,4,5)], collapse =""))
  		    out.date = rbind(out.date,ooo)
  		    
  		  }
  		  
  		   if(strsplit(tmp[4],",")[[1]] %in% c('$GPGGA')){
  		     out.gps = rbind(out.gps,c(tmp[1:3],strsplit(tmp[4],",")[[1]][c(2,3,4,5,6)],NA,NA,NA))
  		     
  		     
  		   }
	  					
	  					
	  		if(strsplit(tmp[4],",")[[1]] %in% c('$GPRMC')){
	  			out.gps = rbind(out.gps,c(tmp[1:3],strsplit(tmp[4],",")[[1]][c(2,4,5,6,7,8,9,10)]))
	  			ooo = c(tmp[1],strsplit(tmp[4],",")[[1]][2],strsplit(tmp[4],",")[[1]][10])
	  			# if(length(ooo)==3) ooo = ooo[c(1,3)] 
	  			out.date = rbind(out.date,ooo)
	  					
	  				
	  					}
	  					
	  
 								
  			
  	
	  			}  								
  			
  				jj=jj+1  
				
			}	
		
		ose = NULL
		da = NA
		ogp = NULL
		ogd = NULL
		

		if(!is.null(out.date)){
		da= as.Date(unique(out.date[,3])[1], format("%d%m%Y"))
		
		}
		
		
		if(!is.null(out.sensors)) {
				ose = data.frame(out.sensors)
				# ose$X1 = do.call(rbind,strsplit(ose$X1,"\\."))[,1]
				ose$Date = da
				ose = ose[,c(21,1,6,9,10,11,16,19,20)]
				names(ose) = c('Date','CPU_Time','Info','Info2','Measure','Validity','X2','Info3', 'Info4')
				#ose$X1 = as.numeric(ose$X1)
				ose$X2 = as.numeric(ose$X2)
				
				if(is.null(station)){
						 sdt = strsplit(strsplit(file,"/")[[1]],"\\.")
						 i = length(sdt)
				 		ose$Set_no = sdt[[i]][1]
						}
				if(!is.null(station)) ose$Set_no = station
			}

		
		if(!is.null(out.gps1)) {
		  ogp = data.frame(out.gps1)
		  ogp$Date = da
		  ogp$X2 = paste(ogp$X2," ",ogp$X3)
		  ogp$X4 = paste(ogp$X4," ",ogp$X5)
		  ogp = ogp[,c(7,1,6,2,4)]
		  ogp$Speed = NA
		  ogp$Heading = NA
		  names(ogp) = c('Date','CPU.Time','GPS.Time','Y','X','Speed','Heading')
		  #ogp$X = convert.dd.dddd(as.numeric(ogp$X))*-1
		  #ogp$Y = convert.dd.dddd(as.numeric(ogp$Y))
		  if(is.null(station)){
		    sdt = strsplit(strsplit(file,"/")[[1]],"\\.")
		    i = length(sdt)
		    ogp$Set_no= sdt[[i]][1]
		  }
		  
		  if(!is.null(station)) ogp$Set_no = station
		} 
		
		
		
		
		if(!is.null(out.gps)) {
				ogp = data.frame(out.gps)
				ogp$Date = da
				#ogp$X1 = do.call(rbind,strsplit(ogp$X1,"\\."))[,1]
				#ogp$X1 = strptime(ogp$X1,"%H:%M:%S")
				ogp$X5 = paste(ogp$X5," ",ogp$X6)
				ogp$X7 = paste(ogp$X7," ",ogp$X8)
				ogp = ogp[,c(grep("Date", colnames(ogp)),1,4,5,7,9,10)]
				names(ogp) = c('Date','CPU.Time','GPS.Time','Y','X','Speed','Heading')
				#ogp$X = convert.dd.dddd(as.numeric(ogp$X))*-1
				#ogp$Y = convert.dd.dddd(as.numeric(ogp$Y))
				if(is.null(station)){
						 sdt = strsplit(strsplit(file,"/")[[1]],"\\.")
						 i = length(sdt)
				 		ogp$Set_no = sdt[[i]][1]
				}
		
		if(!is.null(station)) ogp$Set_no = station
		}
	
	 	

		return(list(ose,ogp))
		
}
