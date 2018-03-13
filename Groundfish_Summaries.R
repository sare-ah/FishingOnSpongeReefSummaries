####################################################################################################################
# Groundfish Fisheries within the Strait of Georgia Sponge Reef Complexes
# 
# Objective:    Select fishing events that cross over the sponge reef polygon   
#
# Summary:      For each fishery of interest - read fishing events, build shapefile & pivot table
# 
# MajStatAreas: 14, 15, 16, 17, 18, 28, 29  
#
# Author:       Sarah Davies
#               Sarah.Davies@dfo-mpo.gc.ca
#               250-756-7124
# Date:         March, 2018
###################################################################################################################

###################
### Start Fresh ###
###################

rm(list=ls())

getwd()
setwd("C:/Users/daviessa/Documents/R/MY_PROJECTS/SoG Sponge Reef CSAS")

Sys.getenv("R_ARCH")   
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R

##### Functions #####

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
UsePackages( pkgs=c("rgdal","sp", "dplyr", "maptools", "data.table", "stringr") ) 

# Remove rows with NA values in specific columns within a dataframe
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

##### Manipulate Groundfish data #####
# 1. FOS
# 2. PacHarvHL
# 3. PacHarvTrawl

########### FOS ##################################
fos <- read.csv("F:/GIS/Requests 2018/SoG Sponge reefs/GF_data/GFFOS.csv", header=TRUE)
summary(fos)

# Create a year field
fos$BEST_DATE <- as.Date(fos$BEST_DATE, format="%d/%m/%Y")
fos$Year <- format(as.Date(fos$BEST_DATE, format="%d/%m/%Y"),"%Y")
  
# Negate Longitude values
fos$START_LONGITUDE <- (-1*fos$START_LONGITUDE)
fos$END_LONGITUDE <- (-1*fos$END_LONGITUDE)
 
# Remove boogus values, Longitude greater than -180 degrees
fos <- dplyr::filter( fos, END_LONGITUDE>= -125.2)
fos <- dplyr::filter( fos, END_LONGITUDE<= -123)
fos <- dplyr::filter( fos, START_LONGITUDE<= -123)
fos <- dplyr::filter( fos, START_LONGITUDE>= -125.2)
fos <- dplyr::filter( fos, END_LATITUDE>= 48.5)
fos <- dplyr::filter( fos, END_LATITUDE<= 50.5)
fos <- dplyr::filter( fos, START_LATITUDE>= 48.5)
fos <- dplyr::filter( fos, START_LATITUDE<= 50.5)
summary(fos)

# Extract start & end coordinates
begin.coord <- data.frame(lon=fos$START_LONGITUDE, lat=fos$START_LATITUDE)
end.coord <- data.frame(lon=fos$END_LONGITUDE, lat=fos$END_LATITUDE)

# Build list of coordinate pairs
sl <- vector("list", nrow(fos))
for (i in seq_along(sl)) {
  sl[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

# Build a spatial line object
set.lines <- SpatialLines(sl)

# Build a SpatialLinesDataFrame to attach the attribute data to
Sldf <- SpatialLinesDataFrame( set.lines, data=fos)

# Define projection
crs.geo <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
proj4string(Sldf) <- crs.geo  # define projection system of our data

# Transform to Albers
albers <- paste( "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126",
                 "+x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep=" " )
Sldf <- spTransform( Sldf, CRS(albers))

# Write shapefile using writeOGR function
# Field names will be abbreviated for ESRI Shapefile driver
dsn <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE"
writeOGR( Sldf, dsn=dsn, layer="FOS_FE", driver="ESRI Shapefile", overwrite_layer=TRUE )
# Save as RDS for easy loading in R
filename <- paste(dsn,"/","FOS_FE",".rds", sep="")
saveRDS(object=Sldf, file=filename )
cat(" ... Created shapefile ...\n" )

# TO DO:
# Calculate the length of a polyline without using ArcMap

folder <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE_byReef/"

fos.reef <- read.csv(paste(folder,"Groundfish_reefs.csv", sep=""), header = TRUE)
trawl <- dplyr::filter( fos.reef, FISHERY=="GROUNDFISH TRAWL" )
dogfish <- dplyr::filter( fos.reef, FISHERY=="SPINY DOGFISH" )

# Parameters for each fishery
fisheries <- list( GrndfshTrawl=trawl,Dogfish=dogfish )

for (i in 1:length(fisheries)){
  # Select fishery
  fishery <- fisheries[[i]]
  file <- names(fisheries[i])
  pvTbl <- dplyr::select(fishery, Reef, Year)
  pvTbl$ID <- row.names(pvTbl)
  pvTbl <- xtabs( ~ Reef+Year, data=pvTbl , na.action=na.exclude)
  filename <- paste(folder,file,"_pivotTbl.csv", sep="")
  write.csv(pvTbl, file=filename, row.names=TRUE ) 
}


##### PacHarvHL ########################### 
pacHL <- read.csv("F:/GIS/Requests 2018/SoG Sponge reefs/GF_data/PacHarvHL.csv", header=TRUE)
summary(pacHL)

# Negate Longitude values
pacHL$OBFL_START_LONGITUDE <- (-1*pacHL$OBFL_START_LONGITUDE)
pacHL$OBFL_END_LONGITUDE <- (-1*pacHL$OBFL_END_LONGITUDE)

# Remove boogus values, Longitude greater than -180 degrees
pacHL <- dplyr::filter( pacHL, OBFL_END_LONGITUDE>= -125.2)
pacHL <- dplyr::filter( pacHL, OBFL_END_LONGITUDE<= -123)
pacHL <- dplyr::filter( pacHL, OBFL_START_LONGITUDE<= -123)
pacHL <- dplyr::filter( pacHL, OBFL_START_LONGITUDE>= -125.2)
pacHL <- dplyr::filter( pacHL, OBFL_END_LATITUDE>= 48.5)
pacHL <- dplyr::filter( pacHL, OBFL_END_LATITUDE<= 50.5)
pacHL <- dplyr::filter( pacHL, OBFL_START_LATITUDE>= 48.5)
pacHL <- dplyr::filter( pacHL, OBFL_START_LATITUDE<= 50.5)
summary(pacHL)

# Extract start & end coordinates
begin.coord <- data.frame(lon=pacHL$OBFL_START_LONGITUDE, lat=pacHL$OBFL_START_LATITUDE)
end.coord <- data.frame(lon=pacHL$OBFL_END_LONGITUDE, lat=pacHL$OBFL_END_LATITUDE)

# Build list of coordinate pairs
sl <- vector("list", nrow(pacHL))
for (i in seq_along(sl)) {
  sl[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

# Build a spatial line object
set.lines <- SpatialLines(sl)

# Build a SpatialLinesDataFrame to attach the attribute data to
Sldf <- SpatialLinesDataFrame( set.lines, data=pacHL)

# Define projection
crs.geo <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
proj4string(Sldf) <- crs.geo  # define projection system of our data

# Transform to Albers
albers <- paste( "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126",
                 "+x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep=" " )
Sldf <- spTransform( Sldf, CRS(albers))

# Write shapefile using writeOGR function
# Field names will be abbreviated for ESRI Shapefile driver
dsn <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE"
writeOGR( Sldf, dsn=dsn, layer="pacHL_FE", driver="ESRI Shapefile", overwrite_layer=TRUE )
# Save as RDS for easy loading in R
filename <- paste(dsn,"/","pacHL_FE",".rds", sep="")
saveRDS(object=Sldf, file=filename )
cat(" ... Created shapefile ...\n" )

# TO DO:
# Calculate the length of a polyline without using ArcMap

folder <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE_byReef/"

hl.reef <- read.csv(paste(folder,"PacHarvHL_reefs.csv", sep=""), dec=".", header = TRUE)
# Build the year field when R adds , to the year during the file import
hl.reef$Year <- as.numeric(gsub(",", "", as.character(hl.reef$Fsh_Y)))

pvTbl <- dplyr::select(hl.reef, Reef, Year)
pvTbl$ID <- row.names(pvTbl)
pvTbl <- xtabs( ~ Reef+Year, data=pvTbl , na.action=na.exclude)
filename <- paste(folder,"pacHarvHL_pivotTbl.csv", sep="")
write.csv(pvTbl, file=filename, row.names=TRUE )


##### PacHarvTrawl ################ 
pacTrawl <- read.csv("F:/GIS/Requests 2018/SoG Sponge reefs/GF_data/PacHarvTrawl.csv", header=TRUE)
summary(pacTrawl)

# Create a year field, filter for 2000+
pacTrawl$OBFL_START_DT <- as.Date(pacTrawl$OBFL_START_DT, format="%d/%m/%Y")
pacTrawl$Year <- format(as.Date(pacTrawl$OBFL_START_DT, format="%d/%m/%Y"),"%Y")
pacTrawl$Year <- as.numeric(pacTrawl$Year)
pacTrawl <- dplyr::filter( pacTrawl, Year>1999)

# Negate Longitude values
pacTrawl$OBFL_START_LONGITUDE <- (-1*pacTrawl$OBFL_START_LONGITUDE)
pacTrawl$OBFL_END_LONGITUDE <- (-1*pacTrawl$OBFL_END_LONGITUDE)

# Remove boogus values, Longitude greater than -180 degrees
pacTrawl <- dplyr::filter( pacTrawl, OBFL_END_LONGITUDE>= -125.2)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_END_LONGITUDE<= -123)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_START_LONGITUDE<= -123)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_START_LONGITUDE>= -125.2)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_END_LATITUDE>= 48.5)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_END_LATITUDE<= 50.5)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_START_LATITUDE>= 48.5)
pacTrawl <- dplyr::filter( pacTrawl, OBFL_START_LATITUDE<= 50.5)
summary(pacTrawl)

# Extract start & end coordinates
begin.coord <- data.frame(lon=pacTrawl$OBFL_START_LONGITUDE, lat=pacTrawl$OBFL_START_LATITUDE)
end.coord <- data.frame(lon=pacTrawl$OBFL_END_LONGITUDE, lat=pacTrawl$OBFL_END_LATITUDE)

# Build list of coordinate pairs
sl <- vector("list", nrow(pacTrawl))
for (i in seq_along(sl)) {
  sl[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

# Build a spatial line object
set.lines <- SpatialLines(sl)

# Build a SpatialLinesDataFrame to attach the attribute data to
Sldf <- SpatialLinesDataFrame( set.lines, data=pacTrawl)

# Define projection
crs.geo <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" )
proj4string(Sldf) <- crs.geo  # define projection system of our data

# Transform to Albers
albers <- paste( "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126",
                 "+x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep=" " )
Sldf <- spTransform( Sldf, CRS(albers))

# Write shapefile using writeOGR function
# Field names will be abbreviated for ESRI Shapefile driver
dsn <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE"
writeOGR( Sldf, dsn=dsn, layer="pacTrawl_FE", driver="ESRI Shapefile", overwrite_layer=TRUE )
# Save as RDS for easy loading in R
filename <- paste(dsn,"/","pacTrawl_FE",".rds", sep="")
saveRDS(object=Sldf, file=filename )
cat(" ... Created shapefile ...\n" )

# TO DO:
# Calculate the length of a polyline without using ArcMap

folder <- "F:/GIS/Requests 2018/SoG Sponge reefs/SHP/FE_byReef/"

trawl.reef <- read.csv(paste(folder,"PacHarvtrawl_reefs.csv", sep=""), dec=".", header = TRUE)
# Build the year field when R adds , to the year during the file import
trawl.reef$Year <- as.numeric(gsub(",", "", as.character(trawl.reef$Year)))

pvTbl <- dplyr::select(trawl.reef, Reef, Year)
pvTbl$ID <- row.names(pvTbl)
pvTbl <- xtabs( ~ Reef+Year, data=pvTbl , na.action=na.exclude)
filename <- paste(folder,"pacHarvtrawl_pivotTbl.csv", sep="")
write.csv(pvTbl, file=filename, row.names=TRUE )
