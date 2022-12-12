To Install

	Dependencies:

	install.packages( "devtools", ask=F, dependencies=TRUE )
	require( "devtools")
	remotes::install_version("INLA", version="20.06.29",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)  ##change version number to get version of INLA built for whatever R version you're running (version list here: https://groups.google.com/g/r-inla-discussion-group/c/c0__KycX1j4)
	install_github("jakeelement/netmensuration")
	install_github("LobsterScience/ILTS.sensor, INSTALL_opts=c("--no-multiarch"))
	install.packages("dplyr")
	install.packages("tidyr")



FOR original ilts.operations.R script written by Brent Cameron, see brent0/ILTS.sensor


