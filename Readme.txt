To Install:

install_github("LobsterScience/ILTS.sensor, INSTALL_opts=c("--no-multiarch"))

	Dependencies:

	install.packages( "devtools", ask=F, dependencies=TRUE )
	require( "devtools")
	remotes::install_version("INLA", version="20.06.29",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)  ##change version number to get version of INLA built for whatever R version you're running (version list here: https://groups.google.com/g/r-inla-discussion-group/c/c0__KycX1j4)
	install_github("jakeelement/netmensuration")
	install.packages("dplyr")
	install.packages("tidyr")


Requires Lobster (or snowcrab) .Rprofile setup to run functions.


FOR original ilts.operations.R script written by Brent Cameron, see brent0/ILTS.sensor


########### Functions in this package:

##### click_touch() ####

(Found within ilts.operations.R script) This function allows lobster ILTS data to run with netmensuration package to create interactive graphs for selecting bottom contact times. Function relies on a number of functions also within the ilts.operations.R script.

Required arguments:
user = "name of user" ### this can be anything, just creates a name for the output file
years = year of data to work with #### (must be numeric, not string, example: 2019). Currently function can only handle one year at a time.

Optional arguments:
Update = T ### default is F causing the function to skip the tows in the output file that have already been click touched. T will cause the function to start from the 			first tow and overwrite the output file. 
skiptows = "character vector of tows to be skipped" #### format is TRIP_ID:SET (example: skiptows = c("100057841:2","100057841:3"))
dummy.depth = "specific tow for which to use a dummy depth parabola" #### the function will always insert a dummy depth parabola if there is no or too little 			real depth data to make a curve. However, even if there is enough depth data but is for some reason insufficient/causes issues with the 			function, the user can use dummy.depth to replace these depths with a parabola so they can still view opening, headline, etc. Format is TRIP_ID:SET 		    (example: dummy.depth = "100057841:2").
divert.messages = T  ### default is F. If T, causes warning messages to be diverted to the output text file (allows these to be saved for record + reduces screen 			    clutter while running function).

