####################################################################################################################
# Fisheries within the Strait of Georgia Sponge Reef Complexes
# 
# Objective:  Select fishing events that cross over the sponge reef polygon   
#
# Summary:    For each fishery of interest - select fishing events inside reef polygon & build pivot table to 
#             summarise results by reef and year
# 
# Fisheries:  Crab by trap, Prawn & shrimp by trap, Shrimp trawl
#
# Note:       Working directory and location of databases are hard coded into the script
#             Script read RDS files because they load quicker than SHP, currently R cannot write to GDB
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
UsePackages( pkgs=c("rgdal","sp","rgeos","dplyr", "maptools","spatialEco","data.table") ) 

# Read in South Coast water shapefile
reefs <- readOGR("F:/GIS/Requests 2018/SoG Sponge reefs/SHP/Background_lyrs","SoG_all_reefs")
# Check projection
reef.crs <- proj4string(reefs)

# Parameters for each fishery
fisheries <- c( "Crab","Prawn","Shrimp_trawl" )
path <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE"
#dsn <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE_byReef"

i <- 2

for (i in 1:length(fisheries)){
  # Select fishery
  fishery <- fisheries[i]
  cat(fishery)
  # Read in data and determine projection
  filename <- paste(path,"/",fishery, ".rds",sep="")
  fishingEvents <- readRDS(filename)
  cat(proj4string(fishingEvents))
  # Reproject FE to reefs
  new.pts <- spTransform(fishingEvents, CRS(reef.crs))
  # Determine which FE are on reefs
  new.pts$Reef <- over(new.pts, reefs)$Reef
  # Select only fishing events witin the reef 
  new.pts <- new.pts[!is.na(new.pts$Reef),]
  plot(reefs)
  points(new.pts, col="blue")
  # Buffer the point to represent the area of a fishing event
  new.poly <- gBuffer( new.pts, width=5.6, byid=FALSE )
  # Clip
  new.poly <- gIntersection(new.poly, reefs)
  # Save work as shp, rds, & csv
  lyrName <- paste(fishery, "_reefs", sep="")
  # Save as a new shapefile
  writeOGR( new.pts, dsn=dsn, layer=lyrName, driver="ESRI Shapefile", overwrite_layer = TRUE )
  # Save as RDS for easy loading in R
  filename <- paste(dsn,"/",lyrName,".rds", sep="")
  saveRDS(object=new.pts, file=filename )
  # Save as csv
  filename <- paste(dsn,"/",lyrName,".csv", sep="")
  write.csv(new.pts@data,file=filename, row.names=FALSE)
  # Create pivot table of results
  pvTbl <- dplyr::select(new.pts@data, Reef, year)
  pvTbl$ID <- row.names(pvTbl)
  pvTbl <- xtabs( ~ Reef+year, data=pvTbl , na.action=na.exclude)
  filename <- paste(dsn,"/",lyrName,"_pivotTbl.csv", sep="")
  write.csv(pvTbl, file=filename, row.names=TRUE ) 
}


gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

zones_clipped <- gClip(new.poly, reefs)
