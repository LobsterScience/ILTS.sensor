#' @title smartfill
#' @description autofills NA cells of a specified variable in a time series data set with nearby (in time) non-NA values.
#' @param variable The variable with NAs that the user wishes to fill with nearby values.
#' @param reftime specifies the time variable used to make sure that cells are not filled with values from across large time gaps
#' @param timeflex specifies (in seconds) how far away in time a nearby row can be to use its value.
#' @param rowflex species number of rows the function will search to fill an NA before it gives up (default=30)
#' @author Geraint Element
#' @export

smartfill <- function(variable="",reftime="",timeflex=4,rowflex = 30){
  ### function needs to be smart enough to fill from down values if the above value is across a large time gap (different tow, set, etc.)
  ### also needs to fill from down for initial rows if these are missing.
  ### First "block" ensures that first row has a value by searching downwards until
  ### it finds one within number of seconds (reftime) specified by timeflex, second block fills all empty rows with previous
  ### row's value, provided it's within allowed timeflex. Then, if there are still NAs due to
  ### large time gaps (resulting from different tows, sets, etc), the code searches down rows until it finds
  ### a value within allowed timeflex.

  ##First block
  for (j in 1:length(variable)){
    if(j==1){
      for(k in 1:rowflex){
        if(is.na(variable[j])){
          if(difftime(as_datetime(reftime[j+1]),as_datetime(reftime[(j)]),units = "secs")<=timeflex){variable[j] = as.character(variable[(k+1)])}
          if(!is.na(variable[k+1])){break}
        }
      }
    }
    ### second block
    if(is.na(variable[j])){
      if(difftime(as_datetime(reftime[j]),as_datetime(reftime[(j-1)]),units = "secs")<=timeflex){variable[j] = as.character(variable[(j-1)])}
      if(is.na(variable[j])){
        for (k in 1:rowflex){
          if(!is.na(variable[j])){break}
          if(!is.na(variable[(j+k)])){
            if(difftime(as_datetime(reftime[(j+k)]),as_datetime(reftime[j]),units = "secs")<=timeflex){
              variable[j] = as.character(variable[(j+k)])
            }
          }
        }
      }
    }
  }

  return(variable)
  }


