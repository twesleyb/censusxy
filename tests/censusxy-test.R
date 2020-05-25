#!/usr/bin/env Rscript

# The censusxy package provides easy access to the U.S. Census Bureau's
# Geocoding Tools: https://geocoding.geo.census.gov/geocoder/ in R.
# These tools allow for unlimited, free geocoding.

# Installation
# install.packages("censusxy")
# devtools::install_github("slu-openGIS/censusxy")

# NOTE: Installation failed because of units dependency.
# The 'sf' option in the central cxy_geocode function utilzes the
# 'sf' package, which in turn relies upon the units library.
# I've forked the repo and just removed any calls to 'sf'.

# Try using my fork:
#devtools::install_github("twesleyb/census_xy")

# Load renv.
renv::load(getrd())

# Load twesleyb/censusxy.
library(censusxy)

# Load the test data.
data(stl_homicides)

# NOTE: Parsing Addresses
# Dataframe or alike object should contain seperate columns for:
# * street address - required; the rest are optional.
# * city
# * state 
# * zipcode

#--------------------------------------------------------------------
# Try batch Geocoding
start <- Sys.time()
homicide_sf <- cxy_geocode(stl_homicides, street = 'street_address', 
			   city = 'city', state = 'state')
end <- Sys.time()

# Status.
n_addrs <- nrow(stl_homicides)
delta_t <- difftime(end,start,units="secs")
message(paste("Time to geocode", formatC(n_addrs,big.mark=","), 
	      "addresses:", round(delta_t,3),"seconds."))
message(paste("Average time per row:",round(as.numeric(delta_t/n_addrs),3),
	"seconds."))

# Print the result:
knitr::kable(head(homicide_sf))

# Worked BUT, very slow.

#---------------------------------------------------------------------
# How long does the single address function take?

# Add column with complete addresses.
df <- homicide_sf
df$address <- paste(df$street_address,df$city,
		    df$state,sep=" ")

# Get a random address.
library(dplyr)
library(microbenchmark)

set.seed(as.numeric(Sys.time()))
df <- df %>% filter(!duplicated(address))
address <- df %>% filter(address == sample(address,1)) %>% 
	select(street_address,city,state) %>% as.list()
message(paste("\nGeocoding a single address:", 
	      paste(unlist(address), collapse=" ")))
names(address)[1] <- "street"

# A function to perform geocoding with cxy_single:
geocode <- function(addr) { suppressWarnings(do.call(cxy_single, addr)) } 

# A function to do an equivalent operation with tidygeocoder.
geocode_tidy <- function(address) {
	df <- data.frame("address"=paste(address,collapse=" "))
	result <- tidygeocoder::geocode(df,"address")
	return(result)
}

# Run experiment:
message("\nAnalyzing time to encode 100 adderesses with Censusxy...")
result <- microbenchmark(geocode(address),geocode_tidy(address),times=100)

