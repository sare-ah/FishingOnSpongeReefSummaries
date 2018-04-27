
# start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   
# The message returned will tell you which version of R is being used
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R
# To reset: Select Tools menu | Global Options... | R Version: | Change
# Then you will have to open and close R for the changes to take effect

# Set working directory
setwd("F:/GIS/Requests 2018/SoG Sponge reefs/Tables")


################ Functions #####################################
################################################################

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


# Remove rows with NA values in specific columns within a dataframe
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


# Make packages available
UsePackages( pkgs=c("dplyr","reshape", "vegan", "stringr","rgdal","sp","geoR","ggplot2","tidyr") ) 

#############
# Psuedocode
# * Read in bathy derivatives tables
# * Build one dataframe of range values for each derivative and reef
# * Plot all range values with different colours
# https://www.sixhat.net/how-to-plot-multpile-data-series-with-ggplot.html

# TO DO
# * look at list of reef complexes and pick 6 complexes with dramatic different health status or fishing events
# * plot facets by reef?


# Read in csv files
arcRug.df <- read.csv("arcRug.txt", header=TRUE, stringsAsFactors = FALSE)
bathy.df <- read.csv("bathy.txt", header=TRUE, stringsAsFactors = FALSE)
fbpi.df <- read.csv("fbpi.txt", header=TRUE, stringsAsFactors = FALSE)
# mcurrent.df <- read.csv("mcurrent.txt", header=TRUE, stringsAsFactors = FALSE)
slope.df <- read.csv("slope.txt", header=TRUE, stringsAsFactors = FALSE)


arcRug <- dplyr::select( arcRug.df, c(REEF,RANGE) )
colnames(arcRug) <- c("REEF","arcRug")
bathy <- dplyr::select( bathy.df, c(REEF,RANGE) )
colnames(bathy) <- c("REEF","bathy")
fbpi <- dplyr::select( fbpi.df, c(REEF,RANGE) )
colnames(fbpi) <- c("REEF","fbpi")
# mcurrent <- dplyr::select( mcurrent.df, c(REEF,RANGE) )
# colnames(mcurrent) <- c("REEF","mcurrent")
slope <- dplyr::select( slope.df, c(REEF,RANGE) )
colnames(slope) <- c("REEF","slope")

bbpi <- read.csv("bbp.txt", header=TRUE, stringsAsFactors = FALSE)
bbpi <- dplyr::select( bbpi, c(REEF,RANGE) )
colnames(bbpi) <- c("REEF","bbpi")

curvature <- read.csv("curvature.txt", header=TRUE, stringsAsFactors = FALSE)
curvature <- dplyr::select( curvature, c(REEF,RANGE) )
colnames(curvature) <- c("REEF","curvature")

mbpi <- read.csv("mbpi.txt", header=TRUE, stringsAsFactors = FALSE)
mbpi <- dplyr::select( mbpi, c(REEF,RANGE) )
colnames(mbpi) <- c("REEF","mbpi")

slopeSD <- read.csv("slopeSD.txt", header=TRUE, stringsAsFactors = FALSE)
slopeSD <- dplyr::select( slopeSD, c(REEF,RANGE) )
colnames(slopeSD) <- c("REEF","slopeSD")

botcrnt <- read.csv("botcurrent_SalishSea.txt", header=TRUE, stringsAsFactors = FALSE)
botcrnt <- dplyr::select( botcrnt, c(REEF,RANGE))
colnames(botcrnt) <- c("REEF","bot_current")

# tidebot <- read.csv("tidebot.txt", header=TRUE, stringsAsFactors = FALSE)
# tidebot <- dplyr::select( tidebot, c(REEF,RANGE) )
# colnames(tidebot) <- c("REEF","tidebot")



df <- data.frame(bind_cols(arcRug, bathy, bbpi, fbpi, curvature, slope, slopeSD, botcrnt))
df$REEF1 <- NULL
df$REEF2 <- NULL
df$REEF3 <- NULL
df$REEF4 <- NULL
df$REEF5 <- NULL
df$REEF6 <- NULL
df$REEF7 <- NULL
#df$REEF8 <- NULL


# Different variable on same plot
# Problem is the y-axis is different for the different variables
# ggplot(df, aes(x = REEF, y = value, color=variable)) + 
#   geom_point(aes(y=fbpi, col="fbpi")) +
#   geom_point(aes(y=bathy, col="bathy")) +
#   theme(axis.text.x = element_text(size=5, face="italic", angle=30, hjust=0.9))
#   

# # Plot of just one variable
# ggplot(arcRug.df, aes(x = REEF, y = RANGE)) + geom_point() +
#   theme(axis.text.x = element_text(size=5, face="italic", angle=30, hjust=0.9)) +
#   annotate("text", x=4, y=0.4, label="Arc Rugosity")

# # Search for key
# df %>%
#   gather() %>% head()


# Plot bathy derivatives against reef complex
df %>%
  gather(-REEF, key="var", value="value") %>% 
  ggplot(aes(x = value, y = REEF)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

write.csv(df, "./DerivativeRangeByReef.csv" )
