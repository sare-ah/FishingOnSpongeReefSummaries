####################################################################################################################
# Fisheries within the Strait of Georgia Sponge Reef Complexes
# 
# Objective:  Select fishing events that cross over the sponge reef polygon   
#
# Summary:    For each fishery of interest - access logbook data, read fishing events, build shapefile
# 
# Fisheries:  Crab by trap, Prawn & shrimp by trap, Scallop by trawl, Shrimp trawl, Groundfish 
#
# Note:       32 bit R needed to read MS Access database
#             Working directory and location of databases are hard coded into the script
#             Script builds SHP, currently R cannot write to GDB
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       February, 2018
###################################################################################################################

###################
### Start Fresh ###
###################

rm(list=ls())

getwd()
setwd("~/R/MY_PROJECTS/SoG_Sponge_Reef_2018")

Sys.getenv("R_ARCH")   
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R


cat("THIS SCRIPT USES THE RODBC Pkg AND REQUIRES 32 bit R")

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, update=FALSE, locn="http://cran.rstudio.com/" ) {
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- pkgs[!(pkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(pkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("library(", pkgs[i], ")", sep="")) )
  }  # End i loop over package names
  # Update packages if requested
  if( update ) update.packages( ask=FALSE )
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("RODBC","rgdal","sp", "dplyr", "maptools", "data.table", "stringr") ) 

#################
### Functions ###
#################

# Grab the fishing events
GrabFishingDat <- function( db, logs, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the logbook data
  fe <- sqlFetch( channel=dataBase, sqtable=logs )
  # Message re logservation data
  cat( "Imported logbook table with", nrow(fe), 
       "rows and", ncol(fe), "columns" )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( fe )
}  # End GrablogsDat function

# Remove rows with NA values in specific columns within a dataframe
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#########################################
#### Build Fisheries with Point data ####
#########################################

# Parameters for each fishery
name <- c( "Crab","Prawn","Shrimp_trawl" )
# sf_log_db$ is mapped to the L:/ drive
# sf_bio_db$ is mapped to the K:/ drive
database <- c( "L:/CrabLogs.mdb","L:/PrawnLogs.mdb","L:/ShrimpTrawlLogs.mdb" ) 
records <- c( "Logbook","Catch","Logbook" )
degree.lower <- c( TRUE,TRUE,TRUE )
tdate <- c( FALSE,TRUE,FALSE ) 
degree.upper <- c( FALSE,FALSE,FALSE )
degree.long <- c( FALSE,FALSE,FALSE )

# Create a dataframe to hold the unique set of parameters for each fishery
fisheries <- data.frame( name, database, records, degree.lower, tdate, degree.upper, degree.long )
fisheries$name <- as.character(fisheries$name)
fisheries$database <- as.character(fisheries$database)
fisheries$records <- as.character(fisheries$records)

# Loop through dataframe, read in fishing events, correct formatting, write shapefile
for (i in 1:nrow(fisheries)){
  # Assign parameters
  fishery <- fisheries[i,1]
  locDB <- fisheries[i,2]
  fishingEvents <- fisheries[i,3]
  cat(fishery, " fishery ...")
  # Read in fishing events
  fe <- GrabFishingDat( db=locDB, logs=fishingEvents )
  # headers <- "Headers"  # code used to add the CFV number to Prawn data
  # hdrs <- GrabFishingDat( db=locDB, logs=headers )
  # hdrs <- dplyr::select( hdrs, Key, year, cfv )
  # hdrs$H_Key <- hdrs$Key
  # fe <- dplyr::left_join( fe, hdrs, by="H_Key")
  # #head(fe)
  # Correct for difference in lat/long/year formatting
  if ( fisheries$degree.lower[i] == TRUE ){
    fe$lon <- ( (fe$long_deg) + (fe$long_min/60) )
    fe$lon <- ( fe$lon*-1 )
    fe$lat <- ( (fe$lat_deg) + (fe$lat_min/60) )
  }
  if ( fisheries$tdate[i] == TRUE ){
    fe$year <- format(fe$tdate, "%Y")
  }
  if ( fisheries$degree.upper[i] == TRUE ){
    fe$lon <- fe$Lon
    fe$lat <- fe$Lat
    fe$Lon <- NULL
    fe$Lat <- NULL
  }
  if ( fisheries$degree.long[i] == TRUE ){
    fe$lon <- ( (fe$Long_deg) + (fe$Long_min/60) )
    fe$lon <- ( fe$lon*-1 )
    fe$lat <- ( (fe$Lat_deg) + (fe$Lat_min/60) )
  }
  # Remove rows without lat/lon
  fe <- completeFun( fe,c("lon", "lat") )
  # If fishery is Prawn - year is hidden in tdate field, need to extract
  if (fishery=="Prawn"){
    fe$year <- as.character(fe$tdate)
    fe$year <- str_sub(fe$year, end=-7)
    fe$year <- as.integer(fe$year)
  }
  # Remove records before 2000
  fe <- dplyr::filter(fe, fe$year>1999)
  cat(" ...")
  # Assign coordinate fields and create Large SpatialPoints DataFrame
  coordinates(fe) <- c( "lon", "lat" )  # set spatial coordinates
  # Define projection
  crs.geo <- CRS( "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs" ) # geographical, datum NAD83
  proj4string(fe) <- crs.geo  # define projection system of our data
  # Write shapefile using writeOGR function
  # Field names will be abbreviated for ESRI Shapefile driver
  dsn <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE"
  writeOGR( fe, dsn=dsn, layer=fishery, driver="ESRI Shapefile", overwrite_layer = TRUE )
  # Save as RDS for easy loading in R
  filename <- paste(dsn,"/",fishery,".rds", sep="")
  saveRDS(object=fe, file=filename )
  rm(fe)
  cat(" Created shapefile ...\n" )
}

odbcCloseAll( )
