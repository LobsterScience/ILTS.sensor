## Installation


	Dependencies:

	install.packages( "devtools", ask=F, dependencies=TRUE )
	require("devtools")
	install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
	install_github("LobsterScience/netmensuration")
	install.packages("dplyr")
	install.packages("tidyr")
	install_github("LobsterScience/ILTS.sensor", INSTALL_opts=c("--no-multiarch"))
	library(ILTS.sensor)


	Requires Lobster (or snowcrab) .Rprofile setup with Oracle credentials to run functions.


FOR original ilts.operations.R script written by Brent Cameron, see brent0/ILTS.sensor


##Running

click_touch()

This function allows lobster ILTS data to run with netmensuration package to create interactive graphs for selecting bottom contact times. Function relies on a number of functions also within the ilts.operations.R script.

Required arguments:
user = "name of user" ### this can be anything, just creates a name for the output file
years = year of data to work with #### (must be numeric, not string, example: 2019). Currently function can only handle one year at a time.

Optional arguments:
Update = T ### default is F causing the function to skip the tows in the output file that have already been click touched. T will cause the function to start from the 			first tow and overwrite the output file. 

skiptows = "character vector of tows to be skipped" #### format is TRIP_ID:SET (example: skiptows = c("100057841:2","100057841:3"))

dummy.depth = "specific tow for which to use a dummy depth parabola" #### the function will always insert a dummy depth parabola if there is no or too little 			real depth data to make a curve. However, even if there is enough depth data but is for some reason insufficient/causes issues with the 			function, the user can use dummy.depth to replace these depths with a parabola so they can still view opening, headline, etc. Format is TRIP_ID:SET 		    (example: dummy.depth = "100057841:2").

divert.messages = T  ### default is F. If T, causes warning messages to be diverted to the output text file (allows these to be saved for record + reduces screen 			    clutter while running function).

fixed_times = if you want to specify specific start and end times for a specific tow; this will default to not run netmensuration analyses but will produce clickable plot. These times need to be of the form c('15:00:00','15:20:00') and in AST.

use_local = will default to bio.lobster::lobster.db('survey') data if T

skip.analyses if T will skip the analyses in netmensuration::bottom_contact and just produce the clickable plot


When you run the function, a pop-up window will immeditaely open to choose a directory for the output files. If you run the function and nothing is happening, check that the pop-up window has not just opened behind your current window.
