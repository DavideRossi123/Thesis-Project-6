#Bivariate spatial plots


#Poisson model (need to state convergence issues)


#NB shape file as well necessary file to run the code!!!!!!!!

#change  shooting_longitude, shooting_latitude everywhere 


#refit models


#create 3 codes

#NB at this poin t try to fit a model without surface temperature and slope in 
#expert 1 and expert 2 to enhance interpretability at the maximum.!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#TO include in the thesis stuff about prediction error!!!!!!!!
#The formula given in the ZImoE is prediction variance not mean variance


#Suggest in the thesis the use of simulation studies
#to assess godness of fit of zinbmoe algorithm !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Add part in which you verufy that hakes in expert 2 are indeed lower in length



#Think about reparametrizing the model so that w2 not w1 is the response variable.


#Do Kolmogrov smirnov test for residuals


#gratia::appraise(model14)  !!!!!!!!!!


# For data with many zeroes clustered together in the covariate space it is quite easy to set up GAMs
# which suffer from identifiability problems, particularly when using Poisson or binomial families.
# The problem is that with e.g. log or logit links, mean value zero corresponds to an infinite range on
# the linear predictor scale.


#2   Check if nursery areas probailities are constant trough time 


#REDUCE THE NUMBER OF BASIS IN SHOOTING DEPTH in the mixing 
#model AS THE ESTIMATE IS TOO WIGLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################# Libraries ####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


library(tidyverse)
library(dplyr)     
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(gratia::draw)
conflicts_prefer(graphics::layout)
conflicts_prefer(gamlss::Rsq)
library(viridis)   
library(scales)    
library(GGally)     
library(gridExtra) 
library(patchwork) 
library(statmod)   
library(mgcv)      
library(gamlss)    
library(gamlss.add)
library(gamlss2)
library(evgam)
library(evd)
library(magrittr)  
library(Metrics)   
library(corrplot)  
library(car)       
library(caret)     
library(knitr)     
library(spdep)     
library(plotly)    
library(moments)   
library(terra)     
library(geosphere) 
library(sf)
library(sp)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(gratia)
library(emodnet.wfs)
library(ncdf4)
library(reticulate)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################### DATA WRANGLING #################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

################################## DATASET TA ##################################

#TA constains haul specific data
TA <- read_csv("https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/TA.csv")
table(TA$month)
#MEDITS TA - Haul Operations Description
#This dataset contains operational and environmental data for each trawl haul

#Let's inspect each variable now:

#country: 3A (3-character alphabetic) - Country conducting the survey (ISO code)
#area: 2N (2-digit numeric) - Geographic area code (GFCM coding system)
#vessel: 3A (3-character alphabetic) - Research vessel identifier 
#gear: 5A (5-character alphabetic) - Type of fishing gear used 
#rigging: 4AN (4-character alphanumeric) - Trawl gear configuration
#door: 4AN (4-character alphanumeric) - Trawl door type and specification 
#year: 4N (4-digit numeric) - Survey year
#month: 2N (2-digit numeric) - Survey month 
#day: 2N (2-digit numeric) - Survey day 
#haul_number: 3N (3-digit numeric) - Unique haul identifier within vessel/year
#codend_closing: 1A (1-character alphabetic) - Codend closure type: "S"=without, "C"=controlled
#Explenation about this covariate:
#CODEND DEFINITION:
#The codend is the terminal, bag-like section of the trawl net where captured organisms accumulate.
#It functions as the collection chamber at the posterior end of the net system.
#2 levels:
#S = Without Closing (Standard)
#- The codend remains permanently open throughout the entire trawl operation
#C = Controlled Closing
#- The codend is equipped with a remote-opening/closing mechanism

#Start of the trawl covariates:
#shooting_quadrant 1N (1-digit numeric)
#This is a location variable indicating in which quadrant of the hemisphere
#the trawl was conducted. See page 43 of the MEDITS Handbook 2017 (version 9_5-60417r).
#This variable will be used to adjust the sign of the longitude coordinate for
#stations located West of the Greenwich meridian(prime meridian).

#shooting_time: 4N (4-digit numeric) 0 to 2400     In UT Ex: 7 h 25 min > 725
#Note that the haul start and end times are recorded in UT time (GMT) and not in
#the local time, but for our analysis the local time is needed instead.
#shooting_latitude: 7N (7-digit numeric) - Latitude at trawl deployment (DDMM.MMM format, e.g., 4338.057 = 43°38.057'N)
#shooting_longitude: 7N (7-digit numeric) - Longitude at trawl deployment (DDMM.MMM format, e.g., 2843.600 = 28°43.600'E)
#Note that locations West of the Greenwich meridian are still stored with positive
#longitude values. We will need to use the quadrant variable to correctly
#assign their sign (negative for west, positive for east).

#Note on coordinates (referencing meridian and parallel):
#- Latitude (shooting_latitude) measures distance north or south from the Equator (the 0° parallel)
#  and must be between -90° and +90°. Positive values indicate north, negative south.
#- Longitude (shooting_longitude) measures distance east or west from the Prime Meridian (0° meridian)
#  and must be between -180° and +180°. Positive values indicate east, negative west.
#- In the dataset, coordinates are stored in DDMM.MMM format (degrees + decimal minutes),
#  e.g., 4338.057 = 43°38.057'N. 

#shooting_depth: 3N (3-digit numeric) - Water depth at shooting location (meters)

#End of the trawl covariates:
#hauling_time: 4N (4-digit numeric) - Time when trawl was retrieved (decimal hours)
#hauling_quadrant: 1N (1-digit numeric) - Geographic quadrant for hauling position (1-4) (end time position)
#hauling_latitude: 7N (7-digit numeric) - Latitude at trawl retrieval (DDMM.MMM format)
#hauling_longitude: 7N (7-digit numeric) - Longitude at trawl retrieval (DDMM.MMM format)
#hauling_depth: 3N (3-digit numeric) - Water depth at hauling location (meters)

#hauling_duration: 2N (2-digit numeric) - Duration of trawl operation (5-90 minutes)
#NB Haul duration is determined by depth: 30 minutes on the continental shelf (10-200m) 
#and 60 minutes on the slope (201-800m).
#validity: 1A (1-character alphabetic) - Haul validity: "V"=valid, "I"=invalid
#course: 1A (1-character alphabetic) - Vessel course during trawl: "R"=rectilinear, "N"=not rectilinear
#Explanation of the covariate:
#course describes the vessel's steering pattern during trawling:
#"R" = Rectilinear (Straight-line course)
#- Vessel maintains a constant, straight heading
#- Ideal for standardized sampling and area calculation
#"N" = Not Rectilinear (Non-straight course)   #Consider deliting obs with course="N"
#- Vessel changes direction during trawling

#recorded_species: 1N (1-digit numeric) Information about species caught
#0  No standard species recorded   
#1  Only the species of the reference list are recorded 
#2  The species of the reference list plus some others are 
#   recorded 
#3  All the caught species are recorded
#4  Species from a national list

#distance: 4N (4-digit numeric) - Distance traveled over ground during trawl (1000-9999 meters)
#vertical_opening: 3N (3-digit numeric) - Vertical opening of trawl net (10-100 decimeters)
#It is the height from seafloor to top of net mouth
#wing_opening: 3N (3-digit numeric) - Horizontal wing opening of trawl (50-250 decimeters)
#It is the width of the net mouth.
#geometrical_precision: 1A (1-character alphabetic) - Precision of opening measurements: "M"=measured, "E"=estimated

#bridles_length: 3N (3-digit numeric) - Length of bridles(100, 150, or 200 meters)
#warp_length: 4N (4-digit numeric) - Length of warp cables (100-2200 meters)
#warp_diameter: 2N (2-digit numeric) - Diameter of warp cables (10-30 millimeters)
#Bridles
#These are the shorter cables that connect the trawl doors to the mouth of the 
#net.   #See thesis images word file
#Warps:
#These are the main towing cables that run from the vessel down to the trawl doors.
#They're usually very long—often hundreds or even thousands of meters depending on the fishing depth.
#hydrological_station: 5A/2A (5 or 2-character alphabetic) - hydrological station identifier or 0 (if 
#no further oceanographic measurement has been conducted)

#observations_flag: 1N (1-digit numeric) - Additional observations code
#0  No problem  
#1  Slight plugging of the net  
#2  Heavy plugging of the net  
#3  High abundance of jellyfish  
#4  High abundance of plants in the net  
#5  Tears of the net  
#6  High abundance of benthos  
#7   
#8   
#9  Other  
#type_of_file: 2A (2-character alphabetic) - File type identifier: "TA" (fixed value) 
#it will be TA for all the observations in this dataset

#bottom_temperature_beginning: 5N/2A (5-digit numeric or 2A) - Bottom(at the seabed level) temp at start (0-30°C, 2 decimals) or "NA"
#bottom_temperature_end: 5N/2A (5-digit numeric or 2A) - Bottom temp at end (0-30°C, 2 decimals) or "NA"
#measuring_system_temp: 2A (2-character alphabetic) - Temperature measurement system (see Annex X) or "NA"

#number_of_the_stratum: 6AN (6-character alphanumeric) - Sampling stratum identifier (see Annex II)
#Unclear how they are computed
#Example:
## FORMAT: [Country][Area][Sub-area][DepthBand]
# 1 1 1 05
# │ │ │  │
# │ │ │  └── Depth Band (05 = 500-800m)
# │ │ └───── Sub-area (1 = "a" in your data)
# | └─────── Geographic Area (1 = Alboran Sea region)
# └───────── Country (1 = Spain)
#part_of_the_codend: 1A (1-character alphabetic) - Codend sampling section: "A"=anterior, "M"=middle, "P"=posterior, "S"=sum
#it's unclear whether multiple observations for the same haul for different
#part_of_the_codend are present in the dataset.

#A rapid check confirms that this is not the case:
keys<-c("country","area","vessel","year","month","day","haul_number","name_of_survey") 
#This set of keys uniquely identify a haul.
TA_grouped <- group_by(TA, across(all_of(keys)))
TA_summary <- summarise(TA_grouped,n_parts = n_distinct(part_of_the_codend), .groups = "drop")
nrow(filter(TA_summary, n_parts > 1)) 
#0 confirming that there are no multiple observations for the same haul, for 
#different part_of_the_codend are present in the dataset.

#name_of_survey: 10A (10-character alphabetic) - Survey identifier

#bottom_salinity_beginning: 5N (5-digit numeric) - Bottom(at seabed level) salinity at start
#bottom_salinity_end: 5N (5-digit numeric) - Bottom salinity at end 
#measuring_system_sal: 2A (2-character alphabetic) - Salinity measurement system (see Annex X)

#Note: Missing values are typically coded as -1 or "NA" in this dataset.
#Coordinates are in DDMM.MMM format (Degrees, Decimal Minutes).
#All depth measurements are in meters, distances in meters, openings in decimeters.







################################## DATASET TB ##################################

#TB contains haul-species specific data
TB <- read_csv("https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/TB.csv")


#MEDITS TB (Trawl Biology) Dataset - Species Catch Data Description
#This dataset contains species-specific biological catch data for each trawl haul

#Let's inspect each variable now:

#country: 3A - Country conducting the survey (ISO code) [ALSO IN TA]
#area: 2N - Geographic area code (GFCM coding system) [ALSO IN TA] 
#vessel: 3A - Research vessel identifier [ALSO IN TA]
#year: 4N - Survey year [ALSO IN TA]
#haul_number: 3N - Unique haul identifier within vessel/year [ALSO IN TA]
#month: 2N - Survey month [ALSO IN TA]
#day: 2N - Survey day [ALSO IN TA]
#name_of_survey: - Survey identifier [ALSO IN TA]
#codend_closing: 1A - Codend closure type: "S"=without, "C"=controlled [ALSO IN TA]
#partit: 1A - Part of codend sampled: "A"=anterior, "M"=middle, "P"=posterior, "S"=sum  #Equivalent to part_of_the_codend in TA
#Explanation of the covariate:
#specifies which part of the codend (the end section of the trawl net where the catch accumulates) was sampled or recorded.
#A = anterior (front part of the codend)
#M = middle section
#P = posterior (rear end, where the catch accumulates)
#S = sum or total codend — the entire codend sampled as one unit, not divided into sections.
#catfau: 3A - taxonomic group to which the species belong to
#genus: 4A - Genus code following MEDITS reference list (e.g., "MERL" = Merluccius)
#species: 3A - Species code following MEDITS reference list (e.g., "MER" = merluccius)
#lref: 2A - Name of reference list used: "FM" = FishMed, "NCC" = National Coding, "MEDITS"
#This doesn't impact how genus was collected.
#ptot: 7N - Total weight of species in the haul (grams)
#nbtot: 7N - Total number of individuals in the haul for the species
#nbfem: 7N - Number of female individuals
#nbmal: 7N - Number of male individuals
#nbind: 7N - Number of undetermined sex individuals
#tf: 2A - Type of file identifier: "TB" (fixed value). The value will be TB for all
#the observations.


#Let's subset TA and TB recalling that our period of interest is 2000-2023 
#and the survey of interest is the MEDITS one
TA<-filter(TA,year>=2000 & year<=2023,name_of_survey=="MEDITS")
TB<-filter(TB,year>=2000 & year<=2023,name_of_survey=="MEDITS")

#In years 2022 and 2023 many countries have zero hauls, so 2000–2021 is a more
#appropriate study period
table(TA$country,TA$year)

TA<-filter(TA,year>=2000 & year<=2021,name_of_survey=="MEDITS") #24217
nrow(TA)
TB<-filter(TB,year>=2000 & year<=2021,name_of_survey=="MEDITS")

#We aim to create a final merged dataset where, for each haul, a row with 
#genus = "MERL" and ptot=0, nbtot=0, nbfem=0, nbmal=0, nbind=0 is added whenever
#no MERL individuals were caught.

keys <- c("country","area","vessel","year","month","day","haul_number")
#Let's check that TA and TB actually have a unique observation for each set of 
#vaues of keys
dup_TA <- TA %>% count(across(all_of(keys))) %>% filter(n > 1)
nrow(dup_TA) #0 
#Note that this implies that no multiple observations for different sections of the codend 
#(anterior, middle, posterior) are present in the dataset.

TB_MER<-TB%>%filter(genus=="MERL",species=="MER")
dup_TB_MER <- TB_MER %>% count(across(all_of(keys))) %>% filter(n > 1)
nrow(dup_TB_MER) #0
#No duplicate records for Merluccius merluccius are present within the same haul 
#in dataset TB.



########################### MERGE OF DATASET TA AND TB #########################
keys <- c("country","area","vessel","year","month","day","haul_number")

#Right join TA and TB
MEDITS <- right_join(x=TA, y=TB, by=keys)

# Get all unique hauls from TA
unique_hauls <- TA %>% distinct(across(all_of(keys)))
#Note that TA unlike TB includes hauls with zero hake catch and hauls with zero 
#catch of any species.
#These two cases are structurally different and were handled explicitly during 
#the join with TB.

#Create tibble to store rows to add
rows_to_add <- tibble()

start <- Sys.time()

for(i in 1:nrow(unique_hauls)) {
  
  current_haul <- unique_hauls[i, ]
  
  #Subset data for this haul in MEDITS
  #Note that this row will be empty when no fishes at all were caught
  haul_data <- MEDITS %>%
    filter(country == current_haul$country,
           area == current_haul$area,
           vessel == current_haul$vessel,
           year == current_haul$year,
           month == current_haul$month,
           day == current_haul$day,
           haul_number == current_haul$haul_number)
  
  #Check if hake was caught in this haul
  has_merl <- any(haul_data$genus=="MERL" & haul_data$species=="MER")
  
  #Case 1: Haul has other species but no hake
  if(!has_merl & nrow(haul_data) > 0) {
    new_row <- haul_data[1, ]
    new_row$genus <- "MERL"
    new_row$species <- "MER"
    new_row$ptot <- 0
    new_row$nbtot <- 0
    new_row$nbfem <- 0
    new_row$nbmal <- 0
    new_row$nbind <- 0
    rows_to_add <- bind_rows(rows_to_add, new_row)
  }
  
  #Case 2: Haul caught zero fish entirely
  if(nrow(haul_data) == 0) {
    #Get the full TA row for this haul to preserve all TA columns
    TA_haul <- TA %>%
      filter(country == current_haul$country,
             area == current_haul$area,
             vessel == current_haul$vessel,
             year == current_haul$year,
             month == current_haul$month,
             day == current_haul$day,
             haul_number == current_haul$haul_number)
    
    #Create a row with TA data plus hake TB data (all zeros)
    new_row <- TA_haul %>%
      mutate(
        genus = "MERL",
        species = "MER",
        ptot = 0,
        nbtot = 0,
        nbfem = 0,
        nbmal = 0,
        nbind = 0)
    rows_to_add <- bind_rows(rows_to_add, new_row)
  }
}

#Add the new rows to the MEDITS dataset
MEDITS_join <- bind_rows(MEDITS, rows_to_add)

end <- Sys.time()
print(end - start)

#Filter for hake only
MEDITS_MERL <- filter(MEDITS_join, genus=="MERL", species=="MER")

#Arrange in standard order
MEDITS_MERL <- MEDITS_MERL %>% 
  arrange(country, area, vessel, year, month, day, haul_number)

nrow(TA) # 24217
nrow(MEDITS_MERL) #24527
#This means that there are hauls in TB with no correspondence in TA
#These inconsistencies are unexpected and originates from data storage issues on 
#the provider's side. Those errors are discussed as well in:
#Methods for supporting stock assessment in the Mediterranean (STECF-21-02)
#https://publications.jrc.ec.europa.eu/repository/handle/JRC126125
#doi: 10.2760/457201

#Let's inspect data about these hauls present in TB but not in TA
TB_not_in_TA <- TB %>% distinct(across(all_of(keys))) %>%
  anti_join(TA %>% distinct(across(all_of(keys))), by = keys)

MEDITS_missing_TA <- MEDITS_MERL %>%
  semi_join(TB_not_in_TA, by = keys)

#Clearly those rows has to be removed from MEDITS_MERL
n<-nrow(MEDITS_MERL)
MEDITS_MERL <- MEDITS_MERL %>%
  anti_join(TB_not_in_TA, by = keys)
n1<-nrow(MEDITS_MERL)
n-n1 #310 


nrow(TA) #24217
nrow(MEDITS_MERL) #24217 as expected



############################ VARIABLES INSPECTION ##############################
#Let's inspect some of the variables, following the order in which they appear 
#in the tibble
#Let's create the tibble that will be used at the modelling stage
MEDITS_MERL_MOD<-MEDITS_MERL 
sort(colSums(is.na(MEDITS_MERL_MOD)),decreasing=TRUE)
nrow(MEDITS_MERL_MOD) #24217

#Country
table(MEDITS_MERL_MOD$country)
#All categories have sufficient representation to allow for reliable estimation.

#Area
table(MEDITS_MERL_MOD$area)
#All categories have sufficient representation to allow for reliable estimation.
#Those 2 variables will likely not be used at spatial modelling level, however they
#are still of interest to interpret the final results.

#Vessel
table(MEDITS_MERL_MOD$vessel)
#All categories have sufficient representation to allow for reliable estimation.

#Year
table(MEDITS_MERL_MOD$year)
#All categories have sufficient representation to allow for reliable estimation.

#Codend_closing
#codend _closing was present in both TA and TB so we have two identical columns in 
#the tibble codend_closing.x and codend_closing.y
MEDITS_MERL_MOD$codend_closing<-MEDITS_MERL_MOD$codend_closing.x
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-codend_closing.x,-codend_closing.y)
table(MEDITS_MERL_MOD$codend_closing) # All the variables have "S" as level
#So this variable can be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-codend_closing)

#Partit
table(MEDITS_MERL_MOD$partit)
#This is the same variable as part_of_the_codend so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-partit)

#Catfau
table(MEDITS_MERL_MOD$catfau)
#This variable is not of interest in our context so remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-catfau)

#Genus
table(MEDITS_MERL_MOD$genus)
#Genus column is redundant as the dataset has already been subset to European hake
#only 
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-genus)

#Lref
table(MEDITS_MERL_MOD$lref)
#This variable is not of interest in our context so remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-lref)

#Ptot
hist(MEDITS_MERL_MOD$ptot,breaks=200)
#This variable will not be included in our model as a covariate, since
#nbtot will be the response variable.
#But it's still of high interest.
#Sanity check:
nrow(MEDITS_MERL_MOD %>%filter(nbtot == 0, ptot > 0))==0 #FALSE! 
#Let's inspect the row(s) with these contradictory values:
MEDITS_error<-MEDITS_MERL_MOD %>%filter(nbtot == 0, ptot > 0)
nrow(MEDITS_error) #1
#The unit has to be removed
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% filter(!(nbtot == 0 & ptot > 0))

#Nbtot
hist(MEDITS_MERL_MOD$nbtot,breaks=200)
#Response variable

#Nbfem
hist(MEDITS_MERL_MOD$nbfem,breaks=200)

#Nbmal
hist(MEDITS_MERL_MOD$nbmal,breaks=200)

MEDITS_MERL_MOD %>%
  filter(nbtot != 0) %>%
  summarise(mean=mean(nbind/nbtot)) 
#this means that sex was not determined for 41% half of the individuals.

#Nbind
hist(MEDITS_MERL_MOD$nbmal,breaks=200)

#Tf
table(MEDITS_MERL_MOD$tf)
#Not of interest, so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-tf)

#Month
table(MEDITS_MERL_MOD$month)
#All categories have sufficient representation to allow for reliable estimation.

#According to the MEDITS handbook, the survey should be conducted between May
#and July; however, this condition is not satisfied across all sampling stations.
#Removing all observations that do not satisfy this condition would result in
#too many observations being removed from the dataset.
#It is strongly recommended to keep the sampling period consistent among different years 
#in order to reduce the time of the survey effect on the time series. 

#Compute the temporal spread of sampling dates within each station
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, day)))

spread_by_station <- MEDITS_MERL_MOD %>%
  group_by(country, area, vessel, year, number_of_the_stratum) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    day_span = as.integer(max_date - min_date),
    .groups = "drop")
spread_by_station

high_spread_stations <- spread_by_station %>%
  filter(day_span > 90)
high_spread_stations

nrow(high_spread_stations)

#Some stations exhibit a wide range of sampling months within different years. 
#This variability can confound the estimation of spatial and temporal effects.
#To minimize bias, we removed observations from stations where the month spread
#exceeded 3 and those observations occurred outside the recommended May–July window.

to_remove <- MEDITS_MERL_MOD %>%
  semi_join(high_spread_stations, by=c("country","area","vessel","year","number_of_the_stratum"))  %>% 
  filter(!(month %in% c(5:8)))
to_remove
nrow(to_remove)
#29 observations has been removed in the end

MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  anti_join(to_remove, by=c("country","area","vessel","year","number_of_the_stratum",
                            "month","day","haul_number"))
nrow(MEDITS_MERL_MOD) #24187

#Day
table(MEDITS_MERL_MOD$day)

#Gear
table(MEDITS_MERL_MOD$gear)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-gear)

#Rigging
table(MEDITS_MERL_MOD$rigging)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-rigging)

#Door
table(MEDITS_MERL_MOD$door)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-door)

#Start of the trawl covariates:
# Shooting_time
hist(MEDITS_MERL_MOD$shooting_time)
#The haul start is recorded in UT time (GMT) and not in the local time.
#We have to conevert in local time to be useful for our analysis.
table(MEDITS_MERL_MOD$country)
#All those countries fall in the +1 time zone
#So we just have to sum 100 to the every entry, it's not necessary to take into
#account possible date changes as the maximum shooting_time is 2124 (21:24)
MEDITS_MERL_MOD$shooting_time<-MEDITS_MERL_MOD$shooting_time+100

#Shooting_longitude
hist(MEDITS_MERL_MOD$shooting_longitude)
#In some Spanish areas west of the Greenwich meridian, the survey was conducted
#in regions with negative longitude values. However, in this dataset all
#longitude values are stored as positive. We need to use the shooting_quadrant variable
#to assign the correct sign to the 'shooting_longitude' variable.

MEDITS_MERL_MOD$shooting_longitude<-ifelse(MEDITS_MERL_MOD$shooting_quadrant==7,
                                           -MEDITS_MERL_MOD$shooting_longitude,
                                           MEDITS_MERL_MOD$shooting_longitude)

#Shooting_quadrant
table(MEDITS_MERL_MOD$shooting_quadrant)
#This variable is not of interest anymore so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-shooting_quadrant)

# Shooting_depth(range 10-800)
n1<-nrow(MEDITS_MERL_MOD)
hist(MEDITS_MERL_MOD$shooting_depth)
c(min(MEDITS_MERL_MOD$shooting_depth),max(MEDITS_MERL_MOD$shooting_depth))
#Some observations have a depth that is outside the range so we remove them:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%filter(shooting_depth >= 10, shooting_depth <= 800)
n2<-nrow(MEDITS_MERL_MOD)
n1-n2 
#21 observations were removed


#End of the trawl covariates:
# Hauling_time
hist(MEDITS_MERL_MOD$hauling_time)
#Similarly to how we handled shooting_time we just need to add 1 to every entry,
#again here is not necessary to take into account possible date changes as maximum 
#MEDITS_MERL_MOD$shooting_time is 2200 (22:00)
MEDITS_MERL_MOD$hauling_time<-MEDITS_MERL_MOD$hauling_time+100


#Hauling_latitude
hist(MEDITS_MERL_MOD$hauling_latitude)

# Hauling_longitude
hist(MEDITS_MERL_MOD$hauling_longitude)
#In some Spanish areas west of the Greenwich meridian, the survey was conducted
#in regions with negative longitude values. However, in this dataset all
#longitude values are stored as positive. We need to use the hauling_quadrant variable
#to assign the correct sign to the 'shooting_longitude' variable.
MEDITS_MERL_MOD$hauling_longitude<-ifelse(MEDITS_MERL_MOD$hauling_quadrant==7,
                                          -MEDITS_MERL_MOD$hauling_longitude,
                                          MEDITS_MERL_MOD$hauling_longitude)

#Hauling_quadrant
table(MEDITS_MERL_MOD$hauling_quadrant)
#This variable is not of interest anymore so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-hauling_quadrant)


#Hauling_depth (range 10-800)
n1<-nrow(MEDITS_MERL_MOD)
hist(MEDITS_MERL_MOD$hauling_depth)
c(min(MEDITS_MERL_MOD$hauling_depth),max(MEDITS_MERL_MOD$hauling_depth))
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%filter(hauling_depth >= 10, hauling_depth <= 800)
n2<-nrow(MEDITS_MERL_MOD)
n1-n2 
#16 observations were removed


#Hauling_duration (in minutes) (range 5-90)
hist(MEDITS_MERL_MOD$hauling_duration)
c(min(MEDITS_MERL_MOD$hauling_duration),max(MEDITS_MERL_MOD$hauling_duration)) 
#No observation is outside the range


#Conversion of coordinates to decimal degrees
#R functions requires coordinates to be in decimal degrees so i convert
#the shooting_latitude and shooting_longitude that are now stored in
#Degrees and Decimal Minutes (DDMM.MMM) in decimal degrees (DD).

#Let's define a function to convert from Degrees and Decimal Minutes (DDMM.MMM)
#to decimal degrees (DD)
ddmm.mmm_to_dd <- function(coord) {
  # Extract degrees
  degrees <- floor(coord / 100)
  # Extract minutes
  minutes <- coord - (degrees * 100)
  # Convert to decimal degrees
  decimal_degrees <- degrees + minutes / 60
  return(decimal_degrees)
}

#Let's convert the coordinates in decimal degreess
MEDITS_MERL_MOD$shooting_latitude<-ddmm.mmm_to_dd(MEDITS_MERL_MOD$shooting_latitude)
MEDITS_MERL_MOD$shooting_longitude<-ddmm.mmm_to_dd(MEDITS_MERL_MOD$shooting_longitude)
MEDITS_MERL_MOD$hauling_latitude<-ddmm.mmm_to_dd(MEDITS_MERL_MOD$hauling_latitude)
MEDITS_MERL_MOD$hauling_longitude<-ddmm.mmm_to_dd(MEDITS_MERL_MOD$hauling_longitude)


#Validity
table(MEDITS_MERL_MOD$validity)
n1<-nrow(MEDITS_MERL_MOD)
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[-which(MEDITS_MERL_MOD$validity=="I"),]
n2<-nrow(MEDITS_MERL_MOD)
n1-n2 
#72 observations were removed
#Validity should then be removed since of no interest anymore:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-validity)
nrow(MEDITS_MERL_MOD)

#Course
table(MEDITS_MERL_MOD$course)
boxplot(nbtot~course,data=MEDITS_MERL_MOD,ylim=c(0,100))
#Rectilinear hauls appear to have an higher nbtot

#Recorded_species
table(MEDITS_MERL_MOD$recorded_species)
#Not of interest here, so it should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-recorded_species)

#Distance (range 1000-9999)
hist(MEDITS_MERL_MOD$distance)
c(min(MEDITS_MERL_MOD$distance),max(MEDITS_MERL_MOD$distance)) 
#no observations outside the range

#Vertical_opening (range 10-100)
hist(MEDITS_MERL_MOD$vertical_opening)
c(min(MEDITS_MERL_MOD$vertical_opening),max(MEDITS_MERL_MOD$vertical_opening)) 
#no observations outside the range

#Wing_opening(range 50-250)
hist(MEDITS_MERL_MOD$wing_opening)
c(min(MEDITS_MERL_MOD$wing_opening),max(MEDITS_MERL_MOD$wing_opening))  
#no observations outside the range

#Geometrical_precision
table(MEDITS_MERL_MOD$geometrical_precision) 
boxplot(nbtot~geometrical_precision,data=MEDITS_MERL_MOD,ylim=c(0,100))
#geometrical_precision doesn't seem to influencite nbtot

#Bridles_length (range={100,150,200})
table(MEDITS_MERL_MOD$bridles_length)


#Warp_length (range 100-2200)
hist(MEDITS_MERL_MOD$warp_length)
c(min(MEDITS_MERL_MOD$warp_length),max(MEDITS_MERL_MOD$warp_length))  
#One observation has warp length outside the range and so it should be removed:
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[-which.max(MEDITS_MERL_MOD$warp_length),]

#Warp_diameter (range 10-30)
hist(MEDITS_MERL_MOD$warp_diameter)
c(min(MEDITS_MERL_MOD$warp_diameter),max(MEDITS_MERL_MOD$warp_diameter))  
#No observations outside the range

#Observations
table(MEDITS_MERL_MOD$observations)
boxplot(nbtot~observations,data=MEDITS_MERL_MOD,ylim=c(0,100))
#Differences in group means are likely driven by unequal sample sizes across levels,
#groups with few observations are less likely to include extreme values, biasing
#mean estimates
boxplot(nbtot~observations,data=MEDITS_MERL_MOD,ylim=c(0,100))

#As some levels have very few observations inside, the best approach is to merge them
#in just 2 categories: no problem (value 0) and problematic(value 1,2,3,4,5,6,9).
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(observations_flag = ifelse(observations == 0, "no_problem", "problem"))
table(MEDITS_MERL_MOD$observations_flag)
boxplot(nbtot~observations_flag,data=MEDITS_MERL_MOD,ylim=c(0,100))
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-observations)

#Type_of_file
table(MEDITS_MERL_MOD$type_of_file) 
#Not of interest so it should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-type_of_file)

#Bottom_temperature_beginning (range 0-30)
hist(MEDITS_MERL_MOD$bottom_temperature_beginning)
c(min(MEDITS_MERL_MOD$bottom_temperature_beginning),max(MEDITS_MERL_MOD$bottom_temperature_beginning))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_temperature_beginning== -1)/nrow(MEDITS_MERL_MOD)
#Around 30% of the variables have  an invalid temperature measurements (-1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to high so the variable cannot be used at
#the modelling stage.
#However the variable is important (See Orsi Relini et al 2002).
plot(MEDITS_MERL_MOD$bottom_temperature_end,MEDITS_MERL_MOD$nbtot)
#Clearly bottom_temperature_end is related to nbtot indeed.

#Some countries like Greece and Croatia have high NA(-1) shares for bottom_temeperature_start.
#Removing the NA values would mean restricting the area analysed. 
#As shown in:
invalid_by_country <- MEDITS_MERL_MOD %>%
  mutate(invalid_temp = bottom_temperature_end == -1) %>%
  group_by(country) %>%
  summarise(
    total_hauls = n(),
    invalid_count = sum(invalid_temp, na.rm = TRUE),
    invalid_prop = invalid_count / total_hauls
  ) %>%
  arrange(desc(invalid_prop))

invalid_by_country
#It's however possible to integrate temperature measurements form external datasets,
#this will be implemented later.


#Bottom_temperature_end
hist(MEDITS_MERL_MOD$bottom_temperature_end)
c(min(MEDITS_MERL_MOD$bottom_temperature_end),max(MEDITS_MERL_MOD$bottom_temperature_end))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_temperature_end==-1)/nrow(MEDITS_MERL_MOD)
#Around 30% of the variables have  an invalid temperature measurment (-1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to variable should be removed:
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-bottom_temperature_end)

#The focus in our model will be on bottom_temperature_start and on regressors
#extracted at the shooting_position in general.
#Indeed shooting regressors are in general more meaningful for modelling abundance
#for the existence of phenomena as the “catch-by-surprise” effect described in 
#works as (Berg et al. 2024). https://doi.org/10.1016/j.fishres.2024.107108

#Number_of_the_stratum
table(MEDITS_MERL_MOD$number_of_the_stratum)
#Not of interest here, so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-number_of_the_stratum)

#Part_of_the_codend
table(MEDITS_MERL_MOD$part_of_the_codend)
boxplot(nbtot~part_of_the_codend,data=MEDITS_MERL_MOD,ylim=c(0,100))
#Level S few observations inside, one option is to merge them in the other category:
MEDITS_MERL_MOD$part_of_codend_grouped <- factor(MEDITS_MERL_MOD$part_of_the_codend)
levels(MEDITS_MERL_MOD$part_of_codend_grouped) <- list(S = "S",P = "P", A_M = c("A", "M"))
boxplot(nbtot~part_of_codend_grouped,data=MEDITS_MERL_MOD,ylim=c(0,100))
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-part_of_the_codend)

#Bottom_salinity_beginning
hist(MEDITS_MERL_MOD$bottom_salinity_beginning)
c(min(MEDITS_MERL_MOD$bottom_salinity_beginning),max(MEDITS_MERL_MOD$bottom_salinity_beginning))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_salinity_beginning==-1)/nrow(MEDITS_MERL_MOD)
#Around 92% of the units have  an invalid temperature measurement (-1 is put to
#indicate the presence of a missing value) 
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_salinity_beginning)

#Bottom_salinity_end
hist(MEDITS_MERL_MOD$bottom_salinity_end)
c(min(MEDITS_MERL_MOD$bottom_salinity_end),max(MEDITS_MERL_MOD$bottom_salinity_end))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_salinity_end==-1)/nrow(MEDITS_MERL_MOD)
#Around 92% of the variables have  an invalid temperature measurment (-1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to high so the variable should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_salinity_end)

#Hydrological station
table(MEDITS_MERL_MOD$hydrological_station)
#As hydrologgial_station is not of interest let's remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-hydrological_station)

#measuring_system_sal
table(MEDITS_MERL_MOD$measuring_system_sal)
#As measuring_system_sal is not of interest let's remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-measuring_system_sal)

#measuring_system_temp
table(MEDITS_MERL_MOD$measuring_system_temp)
#As measuring_system_temp is not of interest let's remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-measuring_system_temp)

#As well name_of_survey.x and name_of_survey.y variables should be removed
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-name_of_survey.x,-name_of_survey.y)


#Make sure each variable is stored in the appropriate format

MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>%mutate(  
  #Categorical variables
  country = as.factor(country),
  area = as.factor(area),
  vessel = as.factor(vessel),
  year = as.factor(year),
  month = as.factor(month),
  day = as.factor(day),
  course = as.factor(course),
  geometrical_precision = as.factor(geometrical_precision),
  observations_flag = as.factor(observations_flag),
  part_of_codend_grouped = as.factor(part_of_codend_grouped),
  
  #Continuous numeric variables
  haul_number = as.numeric(haul_number),
  shooting_time = as.numeric(shooting_time),
  shooting_latitude = as.numeric(shooting_latitude),
  shooting_longitude = as.numeric(shooting_longitude),
  shooting_depth = as.numeric(shooting_depth),
  
  hauling_time = as.numeric(hauling_time),
  hauling_latitude = as.numeric(hauling_latitude),
  hauling_longitude = as.numeric(hauling_longitude),
  hauling_depth = as.numeric(hauling_depth),
  hauling_duration = as.numeric(hauling_duration),
  
  distance = as.numeric(distance),
  vertical_opening = as.numeric(vertical_opening),
  wing_opening = as.numeric(wing_opening),
  warp_length = as.numeric(warp_length),
  warp_diameter = as.numeric(warp_diameter),
  nbtot = as.numeric(nbtot),
  nbfem = as.numeric(nbfem),
  nbmal = as.numeric(nbmal),
  nbind = as.numeric(nbind),
  ptot = as.numeric(ptot))


nrow(MEDITS_MERL_MOD) #24077


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####################  Environmental data integration  #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


############################## CMEMS extraction ################################

#Key environmental covariates (daily average bottom and surface temperature
#at shooting coordinates) were extracted from CMEMS via the Copernicus Python
#API and added to MEDITS_MERL_MOD. Extraction was handled by an external
#Python script called within R using the reticulate package.

#Let's install the required Python libraries
py_require("numpy")
py_require("pandas")
py_require("xarray")
py_require("dask")
py_require("copernicusmarine")
py_require("scipy")

# Download the Python script from GitHub and run it
tmp_py <- tempfile(fileext = ".py")
download.file(
  url = "https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/Copernicus%20API%20code%20final.py",
  destfile = tmp_py,
  mode = "wb"
)
py_run_file(tmp_py)

#Let's initialize MEDITS_MERL_MOD_PY as the dataframe MEDITS_MERL_MOD with the 
#2 additional columns
MEDITS_MERL_MOD_PY<-reticulate::py_to_r(py$MEDITS_MERL_MOD$to_dict("list"))
MEDITS_MERL_MOD_PY<-as_tibble(MEDITS_MERL_MOD_PY)
#Let's make sure that each variable is stored in the appropriate format
MEDITS_MERL_MOD_PY<-MEDITS_MERL_MOD_PY %>% mutate(  
  #Categorical variables
  country = as.factor(country),
  area = as.factor(area),
  vessel = as.factor(vessel),
  year = as.factor(year),
  month = as.factor(month),
  day = as.factor(day),
  course = as.factor(course),
  geometrical_precision = as.factor(geometrical_precision),
  observations_flag = as.factor(observations_flag),
  
  #Continuous numeric variables
  haul_number = as.numeric(haul_number),
  shooting_time = as.numeric(shooting_time),
  shooting_latitude = as.numeric(shooting_latitude),
  shooting_longitude = as.numeric(shooting_longitude),
  shooting_depth = as.numeric(shooting_depth),
  
  hauling_time = as.numeric(hauling_time),
  hauling_latitude = as.numeric(hauling_latitude),
  hauling_longitude = as.numeric(hauling_longitude),
  hauling_depth = as.numeric(hauling_depth),
  hauling_duration = as.numeric(hauling_duration),
  
  distance = as.numeric(distance),
  vertical_opening = as.numeric(vertical_opening),
  wing_opening = as.numeric(wing_opening),
  warp_length = as.numeric(warp_length),
  warp_diameter = as.numeric(warp_diameter),
  nbtot = as.numeric(nbtot),
  nbfem = as.numeric(nbfem),
  nbmal = as.numeric(nbmal),
  nbind = as.numeric(nbind),
  ptot = as.numeric(ptot),
  bottom_temperature = as.numeric(bottom_temperature),
  surface_temperature = as.numeric(surface_temperature))

#Let's subset MEDITS_MERL_MOD_PY
MEDITS_MERL_MOD_PY<-MEDITS_MERL_MOD_PY %>% select(-haul_date)

MEDITS_MERL_MOD<-MEDITS_MERL_MOD_PY

#bottom_temperature contains the value of the bottom_temperature extracted at
#the shooting coordinates this value was originally contained in 
#bottom_temperature_beginning that however contained 30% NAs.
#Let's very that there is correspondence between bottom_temperature_beginning and
#bottom_temperature extracted from Copernicus API.

hist(MEDITS_MERL_MOD$bottom_temperature_beginning-MEDITS_MERL_MOD$bottom_temperature,breaks=100)
#Some discrepancy is expected because Copernicus provides daily averaged
#bottom temperature estimates, whereas MEDITS records temperature at the
#exact time of sampling.

#CMEMS daily average bottom temperature is preferred over bottom_temperature_beginning
#as it provides complete spatial coverage with no missing values (~30% NAs in original).
#The histogram confirms acceptable agreement between the two sources, with residual
#differences attributable to daily averaging in CMEMS.

MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-bottom_temperature_beginning)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################# EMODnet extraction ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Environmental covariates derived from EMODnet are extracted at shooting
#coordinates: seabed substrate type (Folk 5-class scheme), bathymetry, and
#terrain variables (slope, roughness, flow direction) derived from the
#bathymetric raster.

########################### Seabed type extraction #############################

#Convert MEDITS data to sf POINT object using shooting coordinates
medits_sf <- MEDITS_MERL_MOD %>%
  st_as_sf(coords = c("shooting_longitude", "shooting_latitude"),
           crs = 4326)
#Initialize EMODnet Geology WFS client
geo_wfs <- emodnet_init_wfs_client(
  service = "geology_seabed_substrate_maps")

#Inspect available seabed substrate layers
names_seabed<-emodnet_get_wfs_info(geo_wfs)

#Resolve layout conflict before EMODnet extraction
conflicts_prefer(graphics::layout)

#Download seabed substrate map using the Folk 5-class scheme
#Vector polygon layer, coarsest resolution chosen for highest coverage
options(timeout = 300)
folk5 <- emodnet_get_layers(
  wfs = geo_wfs,
  layers = "seabed_substrate_1m",   #coarser preferred for highest data coverage
  outputFormat = "application/json"
)[[1]] %>%
  st_transform(4326)

class(folk5)
st_geometry_type(folk5)
names(folk5)

#Assign seabed type by spatial intersection
#When a point falls in multiple polygons, largest = TRUE retains the dominant match
#Points outside all polygons receive NA
medits_folk5 <- st_join(medits_sf, folk5, left = TRUE, largest = TRUE)

sum(is.na(medits_folk5$folk_5cl))
#222 NAs

#For NA stations, impute using the nearest seabed polygon
na_idx <- is.na(medits_folk5$folk_5cl)
nearest <- st_nearest_feature(medits_sf[na_idx, ], folk5)

#Fill missing Folk 5 classes with nearest polygon value
medits_folk5$folk_5cl[na_idx]     <- folk5$folk_5cl[nearest]
medits_folk5$folk_5cl_txt[na_idx] <- folk5$folk_5cl_txt[nearest]

sum(is.na(medits_folk5$folk_5cl))     #0
sum(is.na(medits_folk5$folk_5cl_txt)) #0

#Store folk 5 class in MEDITS_MERL_MOD
MEDITS_MERL_MOD$folk_5 <- medits_folk5$folk_5cl_txt



########################### Bathymetry extraction ###############################
#Bathymetric data for the Mediterranean Sea were sourced from the EMODnet 2024
#Digital Terrain Model, accessed via the EMODnet geoviewer
#(https://emodnet.ec.europa.eu/geoviewer/). The 14 GeoTIFF tiles covering the
#study area (E4-E8, F4-F8, G5-G8) were downloaded, merged into a single raster
#using terra::merge(), and uploaded to Zenodo to ensure full reproducibility
#and open access (Rossi, 2026; DOI: 10.5281/zenodo.19111208).
#The unified raster is downloaded directly from Zenodo below.

options(timeout = 3600)  # Increase timeout to allow download of large file (1.1 GB)
tmp_tif <- tempfile(fileext = ".tif")
download.file(
  url      = "https://zenodo.org/records/19111208/files/MEDITS_bathy.tif",
  destfile = tmp_tif,
  mode     = "wb"
)
MEDITS_bathy       <- rast(tmp_tif)
MEDITS_bathy_layer <- MEDITS_bathy[[1]]

#Let's extract depth at the shooting coordinates
buffer <- 0.5  # degrees
xrange <- range(MEDITS_MERL_MOD$shooting_longitude) + c(-buffer, buffer)
yrange <- range(MEDITS_MERL_MOD$shooting_latitude) + c(-buffer, buffer)
points_extent<-ext(xrange[1], xrange[2], yrange[1], yrange[2])

#Let's crop the raster
MEDITS_bathy_crop <- crop(MEDITS_bathy_layer, points_extent)

#Let's create spatial points with same CRS as raster
coords<-cbind(MEDITS_MERL_MOD$shooting_longitude, MEDITS_MERL_MOD$shooting_latitude)
points_rast <- vect(coords, crs = crs(MEDITS_bathy_crop))  # Use same CRS as raster

#Verify that the Coordinate Reference System (CRS) of the raster MEDITS_bathy_layer
#is the same of the raster formed by our shooting coordinates
crs(MEDITS_bathy_layer)
crs(points_rast)   #there is correspondence

#Let's extract the value of the bathymetry contained in the MEDITS_bathy_layer 
#raster from the raster that contains the shooting coordinates points, with 2 methods
#simple and bilinear extraction.

#Simple extraction
shooting_extracted_depth_simple<- terra::extract(
  MEDITS_bathy_crop,
  cbind(MEDITS_MERL_MOD$shooting_longitude, MEDITS_MERL_MOD$shooting_latitude),
  method = "simple")

#Bilinear extraction
shooting_extracted_depth_bilinear<- terra::extract(
  MEDITS_bathy_crop,
  cbind(MEDITS_MERL_MOD$shooting_longitude, MEDITS_MERL_MOD$shooting_latitude),
  method = "bilinear")

#Sanity check
print(nrow(shooting_extracted_depth_simple))
print(nrow(shooting_extracted_depth_bilinear))
print(nrow(MEDITS_MERL_MOD))
#They are equal as expected

#Let's add the extracted bathymetry with 2 different exctraction methods to the 
#dataframe
MEDITS_MERL_MOD$shooting_EMODNET_depth_simple <- shooting_extracted_depth_simple[,1]
MEDITS_MERL_MOD$shooting_EMODNET_mean_depth_bilinear <- shooting_extracted_depth_bilinear[,1]  

#There is not much difference so the simplest method that is mean_depth_simple
#should be retained.
#In EMODNET depth values are stored with a negative value for consistency we store them
#with a positive value instead
MEDITS_MERL_MOD$shooting_EMODNET_depth_simple <- -shooting_extracted_depth_simple[,1]
MEDITS_MERL_MOD$shooting_EMODNET_depth_bilinear <- -shooting_extracted_depth_bilinear[,1] 
sort(abs(MEDITS_MERL_MOD$shooting_EMODNET_depth_simple-MEDITS_MERL_MOD$shooting_EMODNET_depth_bilinear),
     decreasing=TRUE)

#No substantial differences is observed between the two extraction methods, so 
#simple extraction is preferred for simplicity.
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%select(-shooting_EMODNET_depth_bilinear)

sum(is.na(MEDITS_MERL_MOD$shooting_EMODNET_depth_simple)) #26
#26 NAs that correspond to trawl shooting coordinates that fall on land or
#coastal cells not covered by the EMODnet bathymetry grid, likely due to
#positional imprecision in the recorded GPS



################ EMODnet and MEDITS bathymetry discrepancy #####################

#Many extreme discrepancies can be observed between the depth values extracted 
#from the EMODnet dataset and the original shooting_depth values recorded in 
#the MEDITS survey
hist(MEDITS_MERL_MOD$shooting_EMODNET_depth_simple-MEDITS_MERL_MOD$shooting_depth,
     main="absolute difference between EMODnet extracted depth and MEDITS survey depth",
     breaks=100)
#Values appears to be normally distributed around zero besides from several
#extreme values on the upper tale.

#Let's inspect the possible reasons behind discrepancies so high:
#Plot of the bathymetry of the Mediterranean sea:
plot(MEDITS_bathy_crop)
#The are areas in which the depth drops drastically in a relatively short distance.

MEDITS_MERL_MOD$depth_diff<-abs(MEDITS_MERL_MOD$shooting_EMODNET_depth_simple-MEDITS_MERL_MOD$shooting_depth)

extreme_diff <- MEDITS_MERL_MOD[MEDITS_MERL_MOD$depth_diff >50, ]
nrow(extreme_diff) 
#For roughly 10% of the observations, the absolute difference between the EMODnet 
#extracted depth and the shooting_depth recorded in the MEDITS survey is very large. 
#Since depth will likely be one of the most important predictors in the modelling 
#stage, having accurate depth measurements is crucial.
#A discrepancy of around 50 m is already considered excessively high.

#Let's now plot such points 
points(extreme_diff$shooting_longitude,
       extreme_diff$shooting_latitude,
       col = "red",
       pch = 20,
       cex = 0.3)
#It's clear that almost all the points with excessive discrepancy are close to 
#those seabed structures in which depth drops drastically

#This is particularly clear if we confront it with the graph in which all the points
#are plotted not only the ones for which abs_diff >50 holds
plot(MEDITS_bathy_crop)
points(MEDITS_MERL_MOD$shooting_longitude,
       MEDITS_MERL_MOD$shooting_latitude,
       col = "red",
       pch = 20,
       cex = 0.3)

#Trawls conducted near steep seabed structures where depth changes drastically in
#few maeters this leads to EMODnet extracted depth values that are very different from the
#MEDITS depth measure computed with a sonar on the vessel.
#This issue is described in a previous works (see Moriarty et al. 2017).
#https://data.marine.gov.scot/dataset/derivation-groundfish-survey-monitoring-and-assessment-data-product-northeast-atlantic-area

#Let's remove variables that are not of interest anymore
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-shooting_EMODNET_depth_simple,
                                            -shooting_EMODNET_mean_depth_bilinear,
                                            -depth_diff)


nrow(MEDITS_MERL_MOD)


# Bathymetric terrain derivatives are computed from the cropped bathymetry raster
# using the terrain() function from the terra package. Three variables are extracted:
# - Slope: steepness of the seabed (degrees).
# - Roughness: local variation in seabed elevation.
# - Flow dir: direction of steepest descent.
# Each variable is extracted at shooting the coordinates.


#############################  Slope extraction ################################
MEDITS_slope_crop<-terrain(MEDITS_bathy_crop,v="slope", unit = "degrees")
plot(MEDITS_slope_crop,main="Slope")
MEDITS_MERL_MOD$shooting_slope <- terra::extract(MEDITS_slope_crop, coords, method="bilinear")[,1]

#############################  Roughness extraction ############################
#Roughness
MEDITS_roughness_crop <- terrain(MEDITS_bathy_crop, v = "roughness")
plot(MEDITS_roughness_crop, main = "Roughness")
MEDITS_MERL_MOD$shooting_roughness <- terra::extract(MEDITS_roughness_crop, coords, method="bilinear")[,1]

#############################  Flow direction extraction #######################
#Flow direction
MEDITS_flowdir_crop <- terrain(MEDITS_bathy_crop, v = "flowdir")
plot(MEDITS_flowdir_crop, main = "Flow Direction")
MEDITS_MERL_MOD$shooting_flowdir <- terra::extract(MEDITS_flowdir_crop, coords, method="bilinear")[,1]






#
#################################### DATASET TC ##################################

TC <- readRDS(url("https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/TC.rds"))

#MEDITS TC haul-species-length-sex-maturity level data.
#This dataset contains detailed biological observations collected during MEDITS hauls.
#Each row represents one unique combination of:
#haul x species x sex x length_class x maturity_stage

#Let's inspect each variable now:

#tf: 2A - fixed value "TC"
#Indicates the type of MEDITS file. Fixed for this dataset.

#country: 3A - ISO code of the country conducting the survey (e.g., "ITA")
#Important for international comparison and reporting.

#area: 2N - GFCM geographic area code
#Identifies the survey subregion. Useful to track regional differences in populations.

#vessel: 3A - research vessel identifier (MEDITS code)
#Records which ship performed the haul, relevant for haul consistency and calibration.

#year: 4N - year of survey (e.g., 2000)
#month: 2N - month of survey (1-12)
#day: 2N - day of survey (1-31)

#haul_number: 3N - sequential haul identifier (1-999) within vessel/year
#Unique ID for a single trawl operation, used to aggregate data at haul level.

#codend_closing: 1A - codend closure type:
#"S" = without closure (open codend)
#"C" = controlled closure

#partit: 1A - section of the codend sampled (equivalent to part_of_the_codend in TA):
#"A" = anterior
#"M" = middle
#"P" = posterior
#"S" = sum of all 3 parts (mandatory if codend is controlled)

#catfau: 3A - MEDITS faunistic group code (Annex V)
#Indicates the taxonomic or ecological group of the species.
#Not used here because the focus is only on Merluccius merluccius.

#genus: 4A - genus code (MEDITS reference list, Annex XV)
#species: 3A - species code (MEDITS reference list, Annex XV)
#Identify the exact species sampled.

#codlon: 1A or 1N - length class code
#Defines size class intervals:
#"m" = 1 mm classes
#"0" = 0.5 cm classes (5 mm)
#"1" = 1 cm classes (10 mm, allowed only until 2012)
#Example: when codlon == 0, each class covers a 5 mm range (e.g., 230, 235, 240 mm, etc.)

#pfrac: 6N - weight of the fraction (grams)
#Weight of the fraction/subsample of the haul selected for measurement.
#This fraction is defined by the combination of length, sex, or other criteria in that row.

#pechan: 6N - weight of the sample actually measured (grams)
#Weight of the portion of the fraction that was actually measured for length, sex, and maturity.
#Usually pechan <= pfrac. If only part of the fraction is measured, pechan represents that measured portion.
#Example: from a 2000g fraction (pfrac), if 500g are measured for biological variables, pechan = 500.
#Together, pfrac and pechan allow computing an expansion factor to estimate the number of individuals
#in the whole haul from the subset actually measured:
#expansion_factor = pfrac / pechan
#n_expanded = nbsex * expansion_factor

#From the fundamental folder:
#The word "fraction" refers to any sub-group of individuals from the total catch of
#a species (males, females, large individuals, small individuals, juveniles, etc.)
#on which a sub-sample may be performed.
#Example: total weight = 1000g divided into 100g of large individuals and 900g of small ones.
#Large individuals are entirely measured (pfrac = 100, pechan = 100).
#Small individuals are sub-sampled at a ratio of 1/10 (pfrac = 900, pechan = 90).

#sex: 1A - biological sex of measured individuals:
#"M" = male
#"F" = female
#"I" = indeterminate (sex cannot be assigned)
#"N" = not determined (sample not sexed)

#nbsex: 6N - number of individuals of the specified sex measured in this length/maturity class.

#length_class: 4N - lower limit of the length class in mm.
#Example: 305 = 30.5-31 cm (when codlon = "0")

#maturity: 1N or 2A - maturity stage (Annex VIIIa-VIIIe)
#0 = immature
#1-4 = successive maturity stages
#ND = not determined (allowed since 2012 for difficult staging)

#matsub: 2A - maturity substage (A-E)
#Provides finer resolution within each maturity stage.
#Must be filled even if maturity = 1 or 2. ND allowed under specific conditions.

#nblon: 6N - number of individuals per length class, sex, and maturity stage.
#Zero-count classes are excluded.
#When maturity = ND, represents total number per length class and sex.

#Note on nbsex vs nblon:
#nbsex = number of individuals per sex and length class (aggregated over maturity stages)
#nblon = number of individuals per sex, length class, and maturity stage

#name_of_survey: 10A - survey campaign identifier
#We are interested in MEDITS surveys only


#The goal of this section is to estimate the number of recruits (individuals
#with length < 150 mm) per haul using the TC (length-class) dataset.
#TC_MERL_MOD and TB_MERL_MOD are first subset to Merluccius observations
#from the MEDITS survey (2000-2021). Individual length measurements are then
#aggregated to haul level, applying a correction factor to account for
#sub-sampling, and the resulting recruit counts are joined back to
#MEDITS_MERL_MOD.

#Let's subset the dataset as we are interested only in observations related to Merluccius 
#Merluccius, from the dataset MEDITS, in the period 2000-2021.
TC_MERL_MOD<-TC %>%filter(genus == "MERL",year >= 2000 & year <= 2021, name_of_survey=="MEDITS")
TB_MERL_MOD<-TB %>%filter(genus == "MERL",year >= 2000 & year <= 2021, name_of_survey=="MEDITS")
#Let's remove  variables that are not of interest, following th same way of thought
#used for the MEDITS_MERL_MOD tibble
TC_MERL_MOD<-TC_MERL_MOD%>%select(-tf,-codend_closing,-partit,-catfau,-name_of_survey)

keys <- c("country","area","vessel","year","month","day","haul_number","genus")
sort(colSums(is.na(MEDITS_MERL_MOD)),decreasing=TRUE)
#Note that the key variables in keys do not contain any NA in MEDITS_MERL_MOD

#Let's build a haul-level table for TB
TB_nbtot<-TB %>%
  filter(genus == "MERL",year >= 2000 & year <= 2021, name_of_survey=="MEDITS") %>%
  select(all_of(keys), nbtot) %>%
  distinct()
TB_nbtot<-TB_nbtot[TB_nbtot$genus=="MERL", ]
nrow(TB_nbtot)
nrow(MEDITS_MERL_MOD)
#The discrepancy is expected as TB only contains data about hauls with a non zero
#number of hakes caught. This fact will be taken into account later.

#Let's remove genus form the keys as we have already subsetted TB_nbtot
keys2<-c("country","area","vessel","year","month","day","haul_number")


#Ensure that all key variables in TC_MERL_MOD have the same data type
#as in MEDITS_MERL_MOD to allow for a consistent join
TC_MERL_MOD <- TC_MERL_MOD %>%mutate(across(all_of(keys2), as.factor))
TC_MERL_MOD <- TC_MERL_MOD %>%mutate(haul_number=as.numeric(haul_number))
#Let's add the corresponding value of nbtot to each row of TC_MERL_MOD
TC_MERL_MOD<- TC_MERL_MOD %>%
  left_join(MEDITS_MERL_MOD %>% 
              select(all_of(keys2), nbtot), by = keys2)

#Let's now estimate the total number of juveninels within each haul
recruits_treshold<-150 #length measurments are in mm not cm

TC_haul_level <- TC_MERL_MOD %>%
  mutate(
    correction_factor = ifelse(pechan > 0, pfrac / pechan, 1),
    n_adjusted = nblon * correction_factor,
    recruits_flag = length_class <= recruits_treshold
  ) %>%
  group_by(across(all_of(keys))) %>%
  summarise(
    recruits_n = sum(n_adjusted[recruits_flag], na.rm = TRUE),
    .groups = "drop")

TC_haul_level <- TC_haul_level %>% mutate(across(all_of(keys2), as.factor))
TC_haul_level <- TC_haul_level %>%mutate(haul_number=as.numeric(haul_number))

#Let's add juvenile_n to MEDITS_MERL_MOD
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  left_join(
    TC_haul_level %>%
      select(all_of(keys2), recruits_n),
    by = keys2)

#If nbtot==0 then substitute the NA value of recruits_n with 0
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(recruits_n = ifelse(nbtot == 0 & is.na(recruits_n),0,recruits_n))

#Add a diagnostic column to MEDITS_MERL_MOD to flag hauls by catch consistency:
#"zero hakes"  → nbtot == 0 and recruits_n is NA (no hakes caught, NA is expected)
#"problems"    → nbtot >  0 and recruits_n is NA (hakes were caught but length data is missing)
#"no problems" → all other cases (nbtot > 0 and recruits_n is available)
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(
    haul_status = case_when(
      nbtot == 0 & is.na(recruits_n) ~ "zero hakes",
      nbtot != 0 & is.na(recruits_n) ~ "problems",
      TRUE  ~ "no problems" ))
table(MEDITS_MERL_MOD$haul_status)
#There were issues in computing recruits_n for 65 hauls.

#Some hauls appear in TC_MERL_MOD but are absent from TB.
#This represents an inconsistency, since biological class-level
#measurements cannot exist if no hakes were caught.
#The same issue is documented in:
#Methods for supporting stock assessment in the Mediterranean (STECF-21-02)
#https://publications.jrc.ec.europa.eu/repository/handle/JRC126125
#doi: 10.2760/457201

MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% ungroup()


MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-name_of_survey)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################## Missing values ######################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

str(MEDITS_MERL_MOD)

#Let's store all categorical variables as factors
MEDITS_MERL_MOD$country <- as.factor(MEDITS_MERL_MOD$country)
MEDITS_MERL_MOD$vessel <- as.factor(MEDITS_MERL_MOD$vessel)
MEDITS_MERL_MOD$course <- as.factor(MEDITS_MERL_MOD$course)
MEDITS_MERL_MOD$geometrical_precision <- as.factor(MEDITS_MERL_MOD$geometrical_precision)
MEDITS_MERL_MOD$species <- as.factor(MEDITS_MERL_MOD$species)
MEDITS_MERL_MOD$observations_flag <- as.factor(MEDITS_MERL_MOD$observations_flag)
MEDITS_MERL_MOD$part_of_codend_grouped <- as.factor(unlist(MEDITS_MERL_MOD$part_of_codend_grouped))
MEDITS_MERL_MOD$folk_5 <- as.factor(MEDITS_MERL_MOD$folk_5)

#Let's exclude from this process variables that are not of interest anymore
MEDITS_MERL_MOD_subset<-MEDITS_MERL_MOD

#Number of NAs by column
sort(colSums(is.na(MEDITS_MERL_MOD_subset)),decreasing=TRUE)

#The analysis is performed on TA and TB only as they are the two datasets that
#will be used at the modelling stage

#Compute the share of NAs for each variable:
share.NA<-sapply(MEDITS_MERL_MOD_subset,function(x) mean(is.na(x)))

#Let's create the corresponding tibble, since it's necessary for plotting in ggplot:
share.NA.tib<- tibble(variable = names(share.NA),share = share.NA)

#Keep only the variables that have a non zero share of NAs:
share.NA.tib<-filter(share.NA.tib,share!=0)

#Let's plot now the share of NAs for each variable:
ggplot(data=share.NA.tib,aes(x=share,y=variable))+
  geom_col(fill="steelblue")+
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Share of missing values",
    y = "Variable",
    title = "Share of Missing Values per Variable")+
  theme_minimal()


MEDITS_MERL_MOD_FULL<-MEDITS_MERL_MOD

n<-nrow(MEDITS_MERL_MOD)
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[complete.cases(MEDITS_MERL_MOD_subset), ]
n1<-nrow(MEDITS_MERL_MOD)
n-n1#91 observations removed

nrow(MEDITS_MERL_MOD) #23986





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##################### Removal of hidden invalid observations ###################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#According to the MEDITS handbook 2017, the following protocol governs haul
#validity:

#Speed:
#The standard trawling speed is 3 knots over the ground. Speeds below 2.8 knots
#risk destabilising the doors, while speeds above 3.2 knots in deep waters may
#lift the gear off the bottom. The lower bound of 2.8 knots is therefore used
#as the reference speed for distance-based validity checks:
#   2.8 knots = 2.8 × 1.852 km/h = 5.1856 km/h = 5,185.6 m/h

#Duration:
#Standard haul duration is 30 minutes at depths ≤ 200 m and 60 minutes at
#depths > 200 m. A haul is considered valid if at least 2/3 of the standard
#duration OR 2/3 of the expected distance has been achieved.

#Expected minimum distances at 2.8 knots:
#   Depths ≤ 200 m: 5,185.6 m/h × 0.5 h × (2/3) = 1,728.5 m
#   Depths > 200 m: 5,185.6 m/h × 1.0 h × (2/3) = 3,457.1 m

#Expected minimum durations:
#   Depths ≤ 200 m: 30 min × (2/3) = 20 minutes
#   Depths > 200 m: 60 min × (2/3) = 40 minutes

#Hauls that satisfy neither the minimum duration criterion (1) nor the minimum
#distance criterion (2) are considered invalid and removed from the dataset.


MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(
    # Expected duration (minutes) depending on depth
    expected_duration = ifelse(shooting_depth < 200, 30, 60),
    
    # Compute expected distance (in meters) from lower bound speed = 2.8 knots
    # 2.8 knots = 2.8 * 1.852 km/h = 5.1856 km/h = 5185.6 m/h
    expected_distance = 5185.6 * (expected_duration / 60),
    
    # 2/3 thresholds for valid haul
    min_distance = (2/3) * expected_distance,
    min_duration = (2/3) * expected_duration,
    
    # A haul is valid if it achieves at least 2/3 of the expected distance OR duration
    valid_haul = ifelse(distance >= min_distance | hauling_duration >= min_duration,TRUE, FALSE))

MEDITS_MERL_MOD<-MEDITS_MERL_MOD%>%
  select(-expected_duration, -expected_distance, -min_distance, -min_duration)

sum(MEDITS_MERL_MOD$valid_haul==FALSE) #299  observations should be removed,as they violate
#the condition expressed above.


#I procede to remove them:
n<-nrow(MEDITS_MERL_MOD) 
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[MEDITS_MERL_MOD$valid_haul==TRUE,]
n1<-nrow(MEDITS_MERL_MOD)
n-n1 #299 as expected
MEDITS_MERL_MOD<-MEDITS_MERL_MOD%>% select(-valid_haul)



nrow(MEDITS_MERL_MOD) #23687



#Shooting coordinates plot #####################################################

# Convert MEDITS data to sf object using shooting coordinates
shooting_sf <- st_as_sf(MEDITS_MERL_MOD,
                        coords = c("shooting_longitude", "shooting_latitude"),
                        crs = 4326)
#Load country polygons for the Mediterranean background
mediterranean_sea<-ne_countries(scale = "medium", returnclass = "sf")

#Plot trawl shooting locations coloured by country
shooting_coordinates_plot <- ggplot() +
  geom_sf(data = mediterranean_sea, fill = "grey62", color = "grey62") +
  geom_sf(
    data = shooting_sf,
    aes(color = country),
    alpha = 0.8,      
    size = 0.5         
  ) +
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(labels = function(x) x) + 
  scale_y_continuous(labels = function(y) y) + 
  coord_sf(xlim = c(-6, 35), ylim = c(30, 46), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    
    axis.title = element_text(size = 16),
    
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  ) +
  labs(x = "Longitude (E°)", y = "Latitude (N°)", color = "Country") +
  guides(color = guide_legend(
    override.aes = list(size = 3) 
  ))

shooting_coordinates_plot





################################################################################
############################ Feature engineering ###############################
################################################################################

################################### OFFSET #####################################

#Fishing effort for demersal species is typically quantified as total swept area,
#since these species remain close to the seabed, making the vertical dimension of
#the net largely irrelevant see Orsi Relini et al. (2002) and  Garofalo et al. (2018).

#Both metrics are computed below for completeness.
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% mutate(total_swept_area=distance*wing_opening,
                                            total_swept_volume=distance*wing_opening*vertical_opening,
                                            NPUE1=nbtot/total_swept_area,
                                            NPUE2=nbtot/total_swept_volume)


hist(MEDITS_MERL_MOD$total_swept_area,breaks=100)
#the bimodal distribution of total sweept area reflects heavily the different duration
#of trawls performed at deepth lower that 200 meters and higher than 200 meters.
hist(MEDITS_MERL_MOD$total_swept_volume,breaks=100)
#Very similar distribution to total_swept_area

#NPUE1
hist(MEDITS_MERL_MOD$NPUE1,breaks=200)
skewness(MEDITS_MERL_MOD$NPUE1) #24.80735
#NPUE2
hist(MEDITS_MERL_MOD$NPUE2,breaks=200)
skewness(MEDITS_MERL_MOD$NPUE2) #24.5917
#nbtot
hist(MEDITS_MERL_MOD$nbtot,breaks=200)
skewness(MEDITS_MERL_MOD$nbtot) #27.78536
#All the distributions have extremly high skewness values.
#nbtot appears to be only slightly more skewed than NPUE1 and NPUE2.

#For consistency with the literature NPUE1 is preferred.

#However in Berg et al. (2024), an alternative offset measure is proposed for bottom-trawl
#surveys. Specifically, they recommend using hauling_duration + 5 minutes for 
#Gadiformes (which include Merluccius merluccius). 
#The additional 5 minutes accounts for the average extra fishing time that 
#occurs outside the nominal tow duration: when the net is being hauled up and 
#brought on board, it can still catch fish.

#NPUE3
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% mutate(NPUE3=nbtot/(hauling_duration+5))
hist(MEDITS_MERL_MOD$NPUE3,breaks=200)
skewness(MEDITS_MERL_MOD$NPUE3) #23.48506

#NPUE3 should be preferred as offset measure as it has lower skewness than both 
#NPUE1 and NPUE2

MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% mutate(NPUE=NPUE3)
MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>% select(-NPUE1,-NPUE2,-NPUE3,-total_swept_area,-total_swept_volume)

#Let's add the offset_mes regressor to MEDITS_MERL_MOD
MEDITS_MERL_MOD$effort_mes<-MEDITS_MERL_MOD$hauling_duration+5


############################# shooting_time ####################################

#shooting_time is converted from HHMM integer format to decimal hours
#(e.g. 1430 → 14.50) for use as a continuous variable at the modelling stage
MEDITS_MERL_MOD$shooting_time <-
  floor(MEDITS_MERL_MOD$shooting_time/100)+(MEDITS_MERL_MOD$shooting_time%%100)/60

############################# hauling_time #####################################

#hauling_time is converted from HHMM integer format to decimal hours
#(e.g. 1430 → 14.50) for use as a continuous variable at the modelling stage
MEDITS_MERL_MOD$hauling_time <-
  floor(MEDITS_MERL_MOD$hauling_time/100)+(MEDITS_MERL_MOD$hauling_time%%100)/60

############################ month #############################################

#month is stored as an ordered factor with abbreviated labels, to use it at the
#modelling stage
MEDITS_MERL_MOD$month <- factor(
  MEDITS_MERL_MOD$month,
  levels = 4:12,
  labels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

########################### year ###############################################

#year is converted from factor to numeric for use as a continuous variable at the 
#modelling stage
MEDITS_MERL_MOD$year <- as.numeric(as.character(MEDITS_MERL_MOD$year))

######################### shooting coordinates #################################

#Coordinates are converted from decimal degrees to metres (EPSG:3035)
#as metric coordinates are better suited for spatial smoothers such as thin plate
#regression splines, which assume isotropic distance
sf_pts <- st_as_sf(
  MEDITS_MERL_MOD,
  coords = c("shooting_longitude", "shooting_latitude"),
  crs = 4326)

sf_pts_m <- st_transform(sf_pts, 3035)

coords_m <- st_coordinates(sf_pts_m)

MEDITS_MERL_MOD$shooting_longitude_m <- coords_m[,1]
MEDITS_MERL_MOD$shooting_latitude_m  <- coords_m[,2]

#Range of the coordinates
range(MEDITS_MERL_MOD$shooting_longitude_m)
range(MEDITS_MERL_MOD$shooting_latitude_m)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###################### Exploratory data analysis (EDA) #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


###############################  Nbtot #########################################

#Let's inspect the distribution of Nbtot
nbtot_freq <- MEDITS_MERL_MOD %>%
  count(nbtot) %>%
  mutate(rel_freq = n/sum(n))

ggplot(nbtot_freq, aes(x = nbtot, y = rel_freq)) +
  geom_segment(aes(xend = nbtot, y = 0, yend = rel_freq), color = "steelblue") +
  labs(x = "Count", y = "Relative Frequency", 
       title = "Relative Frequency of nbtot") +
  theme_minimal()

summary(MEDITS_MERL_MOD$nbtot)
mean(MEDITS_MERL_MOD$nbtot)
sd(MEDITS_MERL_MOD$nbtot)
quantile(MEDITS_MERL_MOD$nbtot,probs=c(0.01, 0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99))
skewness(MEDITS_MERL_MOD$nbtot)
kurtosis(MEDITS_MERL_MOD$nbtot)
#As expected both skewness and Kurtosis are extremly high for nbtot

#Dispersion index (see Puig and Valero 2006)
DI<-var(MEDITS_MERL_MOD$nbtot)/mean(MEDITS_MERL_MOD$nbtot)
DI

#Zero inflation index  (see Puig and Valero 2006)
ZI<-1+log(sum(MEDITS_MERL_MOD$nbtot==0)/length(MEDITS_MERL_MOD$nbtot))/mean(MEDITS_MERL_MOD$nbtot)
ZI

#Density plot of nbtot
ggplot(MEDITS_MERL_MOD, aes(x = nbtot)) +
  geom_density(alpha = 0.3, fill = "steelblue") +
  labs(title = "Histogram of nbtot", x = "Count", y = "Frequency") +
  theme_minimal()

#Let's plot the empirical CDF of nbtot
ggplot(nbtot_freq, aes(x = nbtot, y = cumsum(rel_freq))) +
  geom_line(color = "steelblue") +
  labs(title = "Empirical CDF of nbtot", x = "Count", y = "Cumulative Probability")



################################  NPUE #########################################

#Let's inspect the distribution of NPUE
summary(MEDITS_MERL_MOD$NPUE)
mean(MEDITS_MERL_MOD$NPUE)
sd(MEDITS_MERL_MOD$NPUE)
quantile(MEDITS_MERL_MOD$NPUE,probs=c(0.01, 0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99))
skewness(MEDITS_MERL_MOD$NPUE)
kurtosis(MEDITS_MERL_MOD$NPUE)
#As expected both skewness and Kurtosis are extremly high for NPUE

#Density plot of NPUE
ggplot(MEDITS_MERL_MOD, aes(x=NPUE)) +
  geom_density(alpha = 0.3, fill = "steelblue") +
  labs(title = "Histogram of NPUE", x = "Count", y = "Frequency") +
  theme_minimal()




################ Quantile plot Nbtot and quantile plot NPUE ####################

#Shared quantile probabilities
q_probs <- c(seq(0, 0.9, 0.1), 0.99, 1)

#Helper function to build the x-axis labels (shifts "100%" down a line to avoid overlap)
make_q_labels <- function(probs) {
  labs <- percent(probs)
  labs[probs == 1] <- paste0("\n", labs[probs == 1])
  labs
}

#Nbtot quantile data
qs_nbtot <- quantile(MEDITS_MERL_MOD$nbtot, probs = q_probs)
df_nbtot  <- data.frame(Quantile = q_probs, Value = as.numeric(qs_nbtot))

#Nbtot quantile plot
p_nbtot <- ggplot(df_nbtot, aes(x = Quantile, y = Value)) +
  geom_point(size = 1, col = "steelblue") +
  geom_text(
    aes(label = round(Value, 0)),
    size  = 3,
    vjust = -0.5,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = q_probs,
    labels = make_q_labels(q_probs),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  labs(x = "Quantile", y = "Number of hakes caught (Nbtot)") +
  theme(
    panel.background = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12, color = "black")
  )

#Npue quantile data
qs_npue <- quantile(MEDITS_MERL_MOD$NPUE, probs = q_probs)
df_npue  <- data.frame(Quantile = q_probs, Value = as.numeric(qs_npue))

#Npue quantile plot
p_npue <- ggplot(df_npue, aes(x = Quantile, y = Value)) +
  geom_point(size = 1, col = "steelblue") +
  geom_text(
    aes(label = round(Value, 2)),   # 2 decimals
    size  = 3,
    vjust = -0.5,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = q_probs,
    labels = make_q_labels(q_probs),
    expand = expansion(mult = c(0.03, 0.05))
  ) +
  labs(x = "Quantile", y = "NPUE") +
  theme(
    panel.background = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12, color = "black")
  )

#Combine
p_nbtot + p_npue



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################## EDA — Continuous Regressors #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Models will use effort_mes as an offset.
#NPUE (Number Per Unit Effort) is used as the response for visualisation
#because its range is far narrower than raw counts.
#The y-axis is capped at 100: the 99.9th percentile of NPUE is ≈ 76.54,
#so virtually no information is lost.
quantile(MEDITS_MERL_MOD$NPUE, probs = 0.999)

#Shared theme 
theme_eda <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank()
    )
}

#Helper function scatter plot with smooth
scatter_smooth <- function(data, x_var, title_label) {
  ggplot(data, aes(x = .data[[x_var]], y = NPUE)) +
    geom_point(alpha = 0.3, size = 0.15, colour = "grey40") +
    geom_smooth(colour = "steelblue", linewidth = 1, se = TRUE) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(title = title_label, x = x_var, y = "NPUE") +
    theme_eda()
}

#Helper function violin plot
violin_plot <- function(data, x_var, title_label) {
  ggplot(data, aes(x = factor(.data[[x_var]]), y = NPUE)) +
    geom_violin(fill = "steelblue", alpha = 0.5, trim = TRUE) +
    geom_boxplot(width = 0.08, outlier.size = 0.3, colour = "grey30") +
    coord_cartesian(ylim = c(0, 100)) +
    labs(title = title_label, x = x_var, y = "NPUE") +
    theme_eda()
}


#shooting_time 
scatter_smooth(MEDITS_MERL_MOD, "shooting_time", "NPUE vs. Shooting Time")

#month 
scatter_smooth(MEDITS_MERL_MOD, "month", "NPUE vs. Month")
violin_plot(MEDITS_MERL_MOD, "month", "NPUE by Month")

#year 
scatter_smooth(MEDITS_MERL_MOD, "year", "NPUE vs. Year")
violin_plot(MEDITS_MERL_MOD, "year", "NPUE by Year")

#shooting_latitude 
scatter_smooth(MEDITS_MERL_MOD, "shooting_latitude", "NPUE vs. Shooting Latitude")

#shooting_longitude 
scatter_smooth(MEDITS_MERL_MOD, "shooting_longitude", "NPUE vs. Shooting Longitude")

#shooting coordinates — 3-D scatter 
plot_ly(MEDITS_MERL_MOD,
        x = ~shooting_longitude, y = ~shooting_latitude, z = ~NPUE,
        type   = "scatter3d", mode = "markers",
        marker = list(size = 1.5, color = ~nbtot, colorscale = "Viridis",
                      showscale = TRUE)) %>%
  plotly::layout(title = "NPUE in Shooting-Coordinate Space",
                 scene = list(zaxis = list(range = c(0, 100))))

#shooting coordinates — 2-D density map 
ggplot(MEDITS_MERL_MOD, aes(x = shooting_longitude, y = shooting_latitude)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title  = "Spatial Density of Trawls (Shooting Coordinates)",
       x = "Longitude", y = "Latitude", fill = "Density") +
  theme_eda()

#shooting_depth 
scatter_smooth(MEDITS_MERL_MOD, "shooting_depth", "NPUE vs. Shooting Depth")

#hauling_latitude 
scatter_smooth(MEDITS_MERL_MOD, "hauling_latitude", "NPUE vs. Hauling Latitude")

#hauling_longitude 
scatter_smooth(MEDITS_MERL_MOD, "hauling_longitude", "NPUE vs. Hauling Longitude")

#hauling coordinates — 3-D scatter 
plot_ly(MEDITS_MERL_MOD,
        x = ~hauling_longitude, y = ~hauling_latitude, z = ~NPUE,
        type   = "scatter3d", mode = "markers",
        marker = list(size = 1.5, color = ~nbtot, colorscale = "Viridis",
                      showscale = TRUE)) %>%
  plotly::layout(title = "NPUE in Hauling-Coordinate Space",
                 scene = list(zaxis = list(range = c(0, 100))))

#hauling coordinates — 2-D density map 
ggplot(MEDITS_MERL_MOD, aes(x = hauling_longitude, y = hauling_latitude)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title  = "Spatial Density of Trawls (Hauling Coordinates)",
       x = "Longitude", y = "Latitude", fill = "Density") +
  theme_eda()

#hauling_depth 
scatter_smooth(MEDITS_MERL_MOD, "hauling_depth", "NPUE vs. Hauling Depth")

#hauling_duration 
scatter_smooth(MEDITS_MERL_MOD, "hauling_duration", "NPUE vs. Hauling Duration")

#distance 
scatter_smooth(MEDITS_MERL_MOD, "distance", "NPUE vs. Distance")

#wing_opening 
scatter_smooth(MEDITS_MERL_MOD, "wing_opening", "NPUE vs. Wing Opening")

#warp_length 
scatter_smooth(MEDITS_MERL_MOD, "warp_length", "NPUE vs. Warp Length")

#bottom_temperature 
scatter_smooth(MEDITS_MERL_MOD, "bottom_temperature",
               "NPUE vs. Bottom Temperature")

#surface_temperature 
scatter_smooth(MEDITS_MERL_MOD, "surface_temperature", "NPUE vs. Surface Temperature")

#shooting_slope 
scatter_smooth(MEDITS_MERL_MOD, "shooting_slope", "NPUE vs. Shooting Slope")

#shooting_roughness 
scatter_smooth(MEDITS_MERL_MOD, "shooting_roughness", "NPUE vs. Shooting Roughness")

#shooting_flowdir 
scatter_smooth(MEDITS_MERL_MOD, "shooting_flowdir", "NPUE vs. Shooting Flow Direction")

#effort_mes 
scatter_smooth(MEDITS_MERL_MOD, "effort_mes", "NPUE vs. Effort (effort_mes)")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###################### EDA — Categorical Regressors  ###########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

categorical_vars <- c(
  "country", "area", "vessel", "year", "month", "day",
  "course", "part_of_codend_grouped", "folk_5")

#y-axis again capped at 100
quantile(MEDITS_MERL_MOD$NPUE, probs = 0.99)

#Helper function density plot by group
density_facet <- function(data, group_var, title_label) {
  ggplot(data, aes(x = NPUE, fill = factor(.data[[group_var]]))) +
    geom_density(alpha = 0.6) +
    coord_cartesian(xlim = c(0, 100)) +
    facet_wrap(as.formula(paste("~", group_var))) +
    scale_fill_viridis_d(option = "turbo") +
    labs(title = title_label, x = "NPUE", y = "Density") +
    theme_eda() +
    theme(legend.position = "none")
}

#Helper boxplot by group
box_by_group <- function(data, group_var, title_label) {
  ggplot(data, aes(x = factor(.data[[group_var]]), y = NPUE)) +
    geom_boxplot(fill = "steelblue", alpha = 0.55, outlier.size = 0.4,
                 outlier.alpha = 0.3) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(title = title_label, x = group_var, y = "NPUE") +
    theme_eda()
}


#country
density_facet(MEDITS_MERL_MOD, "country", "NPUE Distribution by Country")
box_by_group(MEDITS_MERL_MOD, "country", "NPUE by Country")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = country)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = country)

#area 
density_facet(MEDITS_MERL_MOD, "area", "NPUE Distribution by Area")
box_by_group(MEDITS_MERL_MOD, "area", "NPUE by Area")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = area)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = area)

#vessel
density_facet(MEDITS_MERL_MOD, "vessel", "NPUE Distribution by Vessel")
box_by_group(MEDITS_MERL_MOD, "vessel", "NPUE by Vessel")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = vessel)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = vessel)

#year
density_facet(MEDITS_MERL_MOD, "year", "NPUE Distribution by Year")
box_by_group(MEDITS_MERL_MOD, "year", "NPUE by Year")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = year)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = year)

#month 
density_facet(MEDITS_MERL_MOD, "month", "NPUE Distribution by Month")
box_by_group(MEDITS_MERL_MOD, "month", "NPUE by Month")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = month)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = month)

#day 
density_facet(MEDITS_MERL_MOD, "day", "NPUE Distribution by Day")
box_by_group(MEDITS_MERL_MOD, "day", "NPUE by Day")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = day)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = day)

#course 
density_facet(MEDITS_MERL_MOD, "course", "NPUE Distribution by Course")
box_by_group(MEDITS_MERL_MOD, "course", "NPUE by Course")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = course)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = course)

#part_of_codend_grouped
density_facet(MEDITS_MERL_MOD, "part_of_codend_grouped",
              "NPUE Distribution by Part of Codend (Grouped)")
box_by_group(MEDITS_MERL_MOD, "part_of_codend_grouped",
             "NPUE by Part of Codend (Grouped)")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = part_of_codend_grouped)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = part_of_codend_grouped)

#folk_5
density_facet(MEDITS_MERL_MOD, "folk_5", "NPUE Distribution by FOLK-5 Class")
box_by_group(MEDITS_MERL_MOD, "folk_5", "NPUE by FOLK-5 Class")
summarise(MEDITS_MERL_MOD, mean   = mean(nbtot),   .by = folk_5)
summarise(MEDITS_MERL_MOD, median = median(nbtot), .by = folk_5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################# Correlation Analysis #############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

continuous_vars <- c(
  "nbtot", "shooting_time", "shooting_latitude", "shooting_longitude",
  "shooting_depth", "distance", "wing_opening", "warp_length",
  "bottom_temperature", "surface_temperature",
  "shooting_slope", "year", "shooting_roughness", "shooting_flowdir")

#Correlation matrix
cor_matrix <- cor(MEDITS_MERL_MOD[, continuous_vars], use = "pairwise.complete.obs")

var_labels <- c(
  nbtot                   = "Nbtot",
  shooting_time           = "Shooting Time",
  shooting_latitude       = "Shooting Lat",
  shooting_longitude      = "Shooting Lon",
  shooting_depth          = "Shooting Depth",
  distance                = "Distance",
  warp_length             = "Warp Length",
  wing_opening            = "Wing Opening",
  bottom_temperature      = "Bottom Temp",
  surface_temperature     = "Surface Temp",
  shooting_slope          = "Slope",
  year                    = "Year",
  month                   = "Month",
  shooting_roughness      = "Roughness",
  shooting_flowdir        = "Flow Direction")

colnames(cor_matrix) <- var_labels[colnames(cor_matrix)]
rownames(cor_matrix) <- var_labels[rownames(cor_matrix)]

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.col      = "black",
         tl.srt      = 45,
         tl.cex      = 0.8,
         diag        = TRUE,
         col         = COL2("RdBu", 200))

#Flag pairs with (> 0.7)
high_corr_pairs <- which(abs(cor_matrix) > 0.7 & lower.tri(cor_matrix), arr.ind = TRUE)
if (nrow(high_corr_pairs) > 0) {
  cat("High correlations (|r| > 0.7):\n")
  for (i in seq_len(nrow(high_corr_pairs))) {
    r <- high_corr_pairs[i, 1]; c <- high_corr_pairs[i, 2]
    cat(sprintf("  %s  <-->  %s : %.3f\n",
                continuous_vars[r], continuous_vars[c], cor_matrix[r, c]))
  }
}

# Key findings:
# 1 warp_length is a near-perfect proxy for shooting_depth and consequently distance (by design)
# 2 shooting_roughness and shooting_slope are highly correlated; slope is
#    preferred following (Garofalo et al. 2018), as roughness is often considered
#    a coarser substrate descriptor.
# 3 distance and shooting_depth have elevated correlation due to the survey design. 

#NB High correlation does not imply concurvity in a GAM context.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###################### Training and Validation Split ##########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#The dataset is split temporally: data from 2000-2020 are used for model
#fitting, while 2021 is held out as an independent validation set (~5% of
#the data). A temporal split is preferred over random splitting to reflect
#a realistic forecasting scenario and avoid data leakage.

MEDITS_MERL_fit<- MEDITS_MERL_MOD %>% filter(year>=2000, year<=2020)
nrow(MEDITS_MERL_fit) #22349

MEDITS_MERL_val<- MEDITS_MERL_MOD %>% filter(year==2021)
nrow(MEDITS_MERL_val) #1338

nrow(MEDITS_MERL_val)/(nrow(MEDITS_MERL_fit)+nrow(MEDITS_MERL_val)) #0.056
#Around 5% of the data left for validation





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################# Modelling ####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Poisson ######################################################################


#Let's fit a simple model containing only basic essential environmental covariates:
model_pois<-gam(
  nbtot ~ offset(log(effort_mes)) +
    s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
    s(shooting_depth, bs = "cr") +
    s(bottom_temperature, bs = "cr") +
    s(surface_temperature, bs = "cr") +
    s(year, bs= "cr") +
    month,
  data = MEDITS_MERL_MOD,
  family = poisson(link = "log"),
  method = "REML")
summary(model_pois) #Deviance explained = 52.7%
#All the terms appears to be significant
plot(model_pois)
par(mfrow = c(2, 2))
gam.check(model_pois)
par(mfrow = c(1, 1))
#All the residual plots apprars to be  extremly bad, suggesting that a Poisson
#model is not appropriate.
#Histogram of residuals particularly indicative.




#Zero inflated Poisson (ZIP) GAM models ########################################


model_zip<- gam(
  nbtot ~ offset(effort_mes) +
    s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
    s(shooting_depth, bs = "cr") +
    s(bottom_temperature, bs = "cr") +
    s(surface_temperature, bs = "cr") +
    s(year, bs= "cr") +
    month+
    s(vessel,bs="re")+
    part_of_codend_grouped+
    course,
  data = MEDITS_MERL_MOD,
  family = ziP(link="identity"),    #only link available
  method = "REML")
summary(model_zip) #Deviance explained =  99.9% 
#This strongly indicates issues during the fitting process.
#In the mgcv documentation it is explained how problem during the fitting process
#often arise due to the model poorly fitting the data.
#Suggesting problems during the fitting process.

#Let's try to fit a simpler ZIP model:
model_zip_simple<- gam(
  nbtot ~ offset(effort_mes) +
    s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
    s(shooting_depth, bs = "cr") +
    s(year, bs= "cr") +
    month+
    part_of_codend_grouped+
    course,
  data = MEDITS_MERL_MOD,
  family = ziP(link="identity"),
  method = "REML")
summary(model_zip_simple) #Deviance explained = 99.9%
#Same issue here
#This indicates mispecification of the model




#Ziplss models #################################################################


#Ziplls family allow the zero inflation and the count process to dependent on 
#different set of covariates
model_ziplss<- gam(
  list(#count process
    nbtot ~ offset(effort_mes) +
      s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
      s(shooting_depth, bs = "cr") +
      s(bottom_temperature, bs = "cr") +
      s(surface_temperature, bs = "cr") +
      s(year, bs= "cr") +
      month+
      s(vessel,bs="re")+
      part_of_codend_grouped+
      course,
    #zero inflation process 
    ~ s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
      s(shooting_depth, bs = "cr") +
      s(bottom_temperature, bs = "cr") +
      s(year, bs= "cr") +
      month),
  data = MEDITS_MERL_MOD,
  family = ziplss(link=list("identity","identity")),    #only link available
  method = "REML")
summary(model_ziplss) #Deviance explained = -1.35e+03%


#Let's try to fit a simpler ZIP model:
model_ziplss_simple<- gam(
  list(#count process
    nbtot ~ offset(effort_mes) +
      s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
      s(shooting_depth, bs = "cr") +
      s(bottom_temperature, bs = "cr") +
      s(surface_temperature, bs = "cr") +
      s(year, bs= "cr") +
      part_of_codend_grouped+
      course,
    #zero inflation process 
    ~ s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=50) +
      s(shooting_depth, bs = "cr") +
      s(bottom_temperature, bs = "cr")),
  data = MEDITS_MERL_MOD,
  family = ziplss(link=list("identity","identity")),
  method = "REML")
summary(model_ziplss_simple) #Deviance explained = -1.52e+03%
#Same issue here



# Negative binomial ############################################################

model_nb<- gam(
  nbtot ~ offset(log(effort_mes)) +
    s(shooting_longitude_m,shooting_latitude_m, bs = "tp",k=150) +
    s(shooting_depth, bs = "cr",k=30) +
    s(bottom_temperature, bs = "cr") +
    s(surface_temperature, bs = "cr",k=20) +
    s(year, bs= "cr") +
    month+
    s(shooting_slope, bs="cr")+
    folk_5,
  data = MEDITS_MERL_fit,
  family = nb(link="log"),
  method = "REML")
summary(model_nb) #Deviance explained = 55.7%

par(mfrow = c(2, 2))
gam.check(model_nb)   
par(mfrow = c(1, 1))
#The model still seems to struggle to capture the excessive overdispersion

plot.gam(model_nb,pages=1,rug=TRUE)
draw(model_nb)

#Concurvity check
concurvity_values<-concurvity(model_nb, full = FALSE)
concurvity_est<-as.data.frame(concurvity_values$estimate)

# Find high concurvity pairs (>0.7)
high_concurv_pairs<- which(abs(concurvity_est) > 0.7 & lower.tri(concurvity_est), arr.ind = TRUE)

if (nrow(high_concurv_pairs) > 0) {
  cat("High concurvity pairs (>0.7):\n")
  for (i in 1:nrow(high_concurv_pairs)) {
    row_idx <- high_concurv_pairs[i, 1]
    col_idx <- high_concurv_pairs[i, 2]
    var1 <- rownames(concurvity_est)[row_idx]
    var2 <- rownames(concurvity_est)[col_idx]
    concurv_val<- concurvity_est[row_idx, col_idx]
    cat(sprintf("%s <-> %s: %.3f\n", var1, var2, concurv_val))
  }
}else {
  cat("No high concurvity pairs detected (all < 0.7)\n")
}
#No high concurvity pairs detected (all < 0.7)

predict_nb<-predict(model_nb,type="response")
predict_nb 
check_nb<-data.frame(observed=MEDITS_MERL_fit$nbtot,predicted=predict_nb)
check_nb


MED_AE_nb<-median(abs(check_nb$predicted - check_nb$observed))
MED_AE_nb #14.67327

MED_AE_baseline<-median(abs(mean(MEDITS_MERL_fit$nbtot)- MEDITS_MERL_fit$nbtot))
MED_AE_baseline  #65.97709 


#NB randomized quantile residuals (training 2000–2020)
theta_nb      <- model_nb$family$getTheta(TRUE)
mu_nb         <- predict(model_nb, type = "response")
y             <- MEDITS_MERL_fit$nbtot

F_y_minus_1_nb <- ifelse(y == 0, 0,
                         pnbinom(y - 1, mu = mu_nb, size = theta_nb))
F_y_nb         <- pnbinom(y, mu = mu_nb, size = theta_nb)

set.seed(2026)
u_nb <- runif(length(y), min = F_y_minus_1_nb, max = F_y_nb)
u_nb[u_nb >= 1] <- 1 - 1e-12
u_nb[u_nb <= 0] <- 1e-12
q_res_nb <- qnorm(u_nb)
q_res_nb <- q_res_nb[is.finite(q_res_nb)]


# ZINB #########################################################################

#Initialization

##Initialize the Zero-Inflation Gating Network
#Fit logistic regression on zero non-zero label (zero_num)
MEDITS_MERL_fit$zero_num<- as.numeric(MEDITS_MERL_fit$nbtot== 0)

model_zi_init<- gam(zero_num ~ s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                      s(shooting_depth, bs = "cr",k=30) +
                      s(bottom_temperature, bs = "cr") +
                      s(shooting_slope, bs="cr")+
                      folk_5, 
                    family = binomial(link="logit"), 
                    data = MEDITS_MERL_fit)
phi_i_init <- predict(model_zi_init, type = "response")
# Quick check
summary(model_zi_init) 
head(phi_i_init)


#Parameters initialization for expert 1
model_expert1<- gam(nbtot ~ offset(log(effort_mes)) +
                      s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                      s(shooting_depth, bs = "cr",k=30) +
                      s(bottom_temperature, bs = "cr") +
                      s(surface_temperature, bs = "cr",k=20) +
                      s(year, bs= "cr") +
                      month+
                      s(shooting_slope, bs="cr")+
                      folk_5,
                    family = nb(link="log"), 
                    data = MEDITS_MERL_fit)
summary(model_expert1) 
mu_i_init<-predict(model_expert1, type = "response")
theta_init<-model_expert1$family$getTheta(TRUE)
head(mu_i_init)



k <- 50
phi_i <- phi_i_init
mu_i <- mu_i_init
theta <- theta_init
log_likelihood_history <- numeric(k) 

for(t in 1:k){
  
  #E STEP
  #Calculate probability of structural zero
  denom <- phi_i + (1 - phi_i) * dnbinom(0, mu = mu_i, size = theta)
  vi_t <- ifelse(MEDITS_MERL_fit$nbtot == 0,
                 phi_i / denom,
                 0)
  #Calculate weight for the count expert that is equal to 1 - vi_t
  w1i_t <- 1 - vi_t
  
  #M STEP
  #Update Zero-inflation Gating Network
  model_zi <- gam(vi_t ~ s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                    s(shooting_depth, bs = "cr",k=30) +
                    s(bottom_temperature, bs = "cr") +
                    s(shooting_slope, bs="cr")+
                    folk_5, 
                  family = quasibinomial(link = "logit"), 
                  data = MEDITS_MERL_fit)
  
  #Update Count Expert (Negative Binomial)
  model_expert1 <- gam(nbtot ~ offset(log(effort_mes)) +
                         s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                         s(shooting_depth, bs = "cr",k=30) +
                         s(bottom_temperature, bs = "cr") +
                         s(surface_temperature, bs = "cr",k=20) +
                         s(year, bs= "cr") +
                         month+
                         s(shooting_slope, bs="cr")+
                         folk_5,
                       family = nb(link = "log"),
                       weights = w1i_t,   
                       data = MEDITS_MERL_fit)
  
  #Update parameters for the next iteration
  phi_i <- predict(model_zi, type = "response")
  mu_i <- predict(model_expert1, type = "response")
  theta <- model_expert1$family$getTheta(TRUE)
  
  #Convergence check
  f_i <- dnbinom(MEDITS_MERL_fit$nbtot, mu = mu_i, size = theta)
  
  log_lik_i <- ifelse(
    MEDITS_MERL_fit$nbtot == 0,
    log(phi_i + (1 - phi_i) * f_i),
    log(1 - phi_i) + log(f_i)
  )
  
  log_likelihood <- sum(log_lik_i)
  log_likelihood_history[t] <- log_likelihood
  
  cat("Iteration:", t,
      "| Log-Likelihood:", round(log_likelihood, 2),
      "| Theta:",  round(theta, 3), "\n")
}

#Create a list containing components of the fitted model
final_results_zinb<- list(
  vi_t = vi_t,                #The latent probability of being a structural zero
  model_zi = model_zi,         #The Zero-Inflation model
  model_expert = model_expert1, #The Expert  model
  theta_final = theta  ,         #Theta parameter of the final model
  log_likelihood_history=log_likelihood_history
)         
#Let's inspect the smooth terms of the models


#Zero-inflated model
summary(final_results_zinb$model_zi)

plot.gam(final_results_zinb$model_zi,pages=1,rug=TRUE)
draw(final_results_zinb$model_zi)
#Count model
summary(final_results_zinb$model_expert)

plot.gam(final_results_zinb$model_expert,pages=1,rug=TRUE)
draw(final_results_zinb$model_expert)

#Get the probability of being a structural zero (Gating Network)
prob_structural_zero <- predict(final_results_zinb$model_zi, type = "response")

#Get the mean of the Negative Binomial part (Expert Network)
mean_nb <- predict(final_results_zinb$model_expert, type = "response")

#Calculate the fitted values of the ZINB model
fitted_values <- (1 - prob_structural_zero) * mean_nb

#Compare
check_zinb <- data.frame(
  observed = MEDITS_MERL_fit$nbtot, 
  fitted = fitted_values)

MED_AE_zinb<-median(abs(check_zinb$observed - check_zinb$fitted))
MED_AE_zinb #14.43685
MED_AE_baseline #65.97709

#AIC
#Compute AIC using definition of Wood and Pya (2016) (implemented using edf of a
#gam object). 

phi <- predict(final_results_zinb$model_zi, type = "response")
mu  <- predict(final_results_zinb$model_expert, type = "response")
theta <- final_results_zinb$theta_final
p_nb_zero <- dnbinom(0, mu = mu, size = theta)
y<-MEDITS_MERL_fit$nbtot
#Likelihood for each observation
lik <- ifelse(y == 0,
              phi + (1 - phi) * p_nb_zero,
              (1 - phi) * dnbinom(y, mu = mu, size = theta))

#Total Log-Likelihood
loglik_total <- sum(log(lik))

#Degrees of Freedom (edf)
df_total<-sum(final_results_zinb$model_zi$edf) + sum(final_results_zinb$model_expert$edf)

AIC_zinb<- -2 * loglik_total + 2 * df_total
AIC_zinb #172264.1

AIC(model_nb) #174811



#ZINB randomized quantile residuals (training 2000–2020)
#Lower bound: F(y-1)
F_y_minus_1 <- ifelse(y == 0, 
                      0, 
                      phi + (1 - phi) * pnbinom(y - 1, mu = mu, size = theta))

#Upper bound: F(y)
F_y <- phi + (1 - phi) * pnbinom(y, mu = mu, size = theta)
set.seed(2026)
u_zinb <- runif(length(y), min = F_y_minus_1, max = F_y)

q_res_zinb<- qnorm(u_zinb)

#Histogram
hist(q_res_zinb, breaks = 50, prob = TRUE, 
     main = "Histogram of Quantile Residuals",
     xlab = "Residuals", col = "steelblue", border = "white")
#The quantile residuals are approximately symmetric and closer to normality
#than model_nb, but a slight right skew persists, suggesting the model
#still under-predicts the frequency of large counts.

#Q-Q Plot
qqnorm(q_res_zinb, main = "Q-Q Plot (ZINB Mixture)")
qqline(q_res_zinb, col = "darkred", lwd = 2)

#Q-Q plot reveals systematic departure in both tails, confirming the model cannot 
#fully account for the extreme overdispersion in haul counts.
#Improvements with respect to model_nb are marginal in terms of the Q-Q plot.


#NB model diagnostic
par(mfrow=c(2,2))
gam.check(model_nb)
par(mfrow=c(1,1))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################# ZINBMoE2 #####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Initialization

#Initialize the Zero-Inflation Gating Network
MEDITS_MERL_fit$zero_num <- as.numeric(MEDITS_MERL_fit$nbtot == 0)

model_zi_init <- gam(zero_num ~ s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                       s(shooting_depth, bs = "cr",k=30) +
                       s(bottom_temperature, bs = "cr") +
                       s(shooting_slope, bs="cr")+
                       folk_5, 
                     family = binomial(link = "logit"), 
                     data = MEDITS_MERL_fit)

phi_i_init <- predict(model_zi_init, type = "response")


#Initialize Mixing Probabilities (p1, p2), equal weights initially (0.5 and 0.5)
p1_i_init <- rep(0.5, nrow(MEDITS_MERL_fit))
p2_i_init <- 1 - p1_i_init


#90th usual threshold for nursery areas  identification (Paradinas et al. 2020; Izquierdo et al. 2021)
threshold <- quantile(MEDITS_MERL_fit$nbtot, 0.90)

#Expert 1: "Low" values expert
#Weights give more importance to the bottom 90% of observations
w_init_exp1 <- ifelse(MEDITS_MERL_fit$nbtot <= threshold, 1, 0.1)
model_expert1_init <- gam(nbtot ~ offset(log(effort_mes)) +
                            s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                            s(shooting_depth, bs = "cr",k=30) +
                            s(bottom_temperature, bs = "cr") +
                            s(surface_temperature, bs = "cr",k=20) +
                            s(year, bs= "cr") +
                            month+
                            s(shooting_slope, bs="cr")+
                            folk_5,
                          family = nb(link="log"),
                          weights = w_init_exp1,
                          data = MEDITS_MERL_fit)

#Expert 2: "High" values expert
#Weights give more importance to the top 10% of observations
w_init_exp2 <- ifelse(MEDITS_MERL_fit$nbtot > threshold, 1, 0.1)
model_expert2_init <- gam(nbtot ~ offset(log(effort_mes)) +
                            s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                            s(shooting_depth, bs = "cr",k=30) +
                            s(bottom_temperature, bs = "cr") +
                            s(surface_temperature, bs = "cr",k=20) +
                            s(year, bs= "cr") +
                            month+
                            s(shooting_slope, bs="cr")+
                            folk_5,
                          family = nb(link="log"), 
                          weights = w_init_exp2, 
                          data = MEDITS_MERL_fit)

mu1_i_init <- predict(model_expert1_init, type = "response")
mu2_i_init <- predict(model_expert2_init, type = "response")
theta1_init <- model_expert1_init$family$getTheta(TRUE)
theta2_init <- model_expert2_init$family$getTheta(TRUE)


#EM Algorithm 

k <- 100
log_likelihood_history <- numeric(k)

#Initialize parameters
phi_i <- phi_i_init
p1_i <- p1_i_init
p2_i <- p2_i_init
mu1_i <- mu1_i_init
mu2_i <- mu2_i_init
theta1 <- theta1_init
theta2 <- theta2_init

for (t in 1:k) {
  
  #E STEP
  
  #Densities at zero for each expert
  f1_0<-dnbinom(0, mu = mu1_i, size = theta1)
  f2_0<-dnbinom(0, mu = mu2_i, size = theta2)
  
  #Densities at every value (zero included) for each expert
  f1_i <- dnbinom(MEDITS_MERL_fit$nbtot, mu = mu1_i, size = theta1) 
  f2_i <- dnbinom(MEDITS_MERL_fit$nbtot, mu = mu2_i, size = theta2)
  
  #Calculate probability of structural zero
  vi_t <- ifelse(MEDITS_MERL_fit$nbtot == 0,
                 phi_i /(phi_i + (1 - phi_i)*(p1_i*f1_0+p2_i*f2_0)),
                 0)
  
  #Calculate weights for each expert
  w1i_t <- ifelse(MEDITS_MERL_fit$nbtot == 0,
                  (f1_0 * (1 - phi_i) * p1_i) / (phi_i + (1 - phi_i)*(p1_i*f1_0+p2_i*f2_0)),
                  (f1_i * p1_i) / (p1_i * f1_i + p2_i * f2_i))
  
  w2i_t <- 1-w1i_t
  
  
  
  #M STEP
  
  #Update Zero-Inflation Gating Network
  model_zi <- gam(vi_t ~ 
                    s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                    s(shooting_depth, bs = "cr",k=30) +
                    s(bottom_temperature, bs = "cr") +
                    s(shooting_slope, bs="cr")+
                    folk_5, 
                  family = quasibinomial(link = "logit"), 
                  data = MEDITS_MERL_fit)
  
  #Update Mixing Gating Network
  model_mixing <- gam(w1i_t ~ 
                        s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                        s(shooting_depth, bs = "cr",k=30) +
                        s(bottom_temperature, bs = "cr") +
                        s(shooting_slope, bs="cr")+
                        folk_5,
                      family = quasibinomial(link = "logit"),
                      data = MEDITS_MERL_fit) 
  
  #Update Expert 1
  model_expert1_zinb2 <- gam(nbtot ~ offset(log(effort_mes)) +
                               s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                               s(shooting_depth, bs = "cr",k=30) +
                               s(bottom_temperature, bs = "cr") +
                               s(surface_temperature, bs = "cr") +
                               s(year, bs= "cr") +
                               month+
                               s(shooting_slope, bs="cr")+
                               folk_5,
                             family = nb(link = "log"),
                             weights = w1i_t,
                             data = MEDITS_MERL_fit)
  
  #Update Expert 2
  model_expert2_zinb2 <- gam(nbtot ~ offset(log(effort_mes)) +
                               s(shooting_latitude_m,shooting_longitude_m, bs = "tp",k=150) +
                               s(shooting_depth, bs = "cr",k=30) +
                               s(bottom_temperature, bs = "cr") +
                               s(surface_temperature, bs = "cr") +
                               s(year, bs= "cr") +
                               month+
                               s(shooting_slope, bs="cr")+
                               folk_5,
                             family = nb(link = "log"),
                             weights = w2i_t,
                             data = MEDITS_MERL_fit)
  
  
  #Update parameters for the next iteration
  phi_i_old <- phi_i
  phi_i <- predict(model_zi, type = "response")
  p1_i <- predict(model_mixing, type = "response")
  p2_i <- 1 - p1_i
  mu1_i <- predict(model_expert1_zinb2, type = "response")
  mu2_i <- predict(model_expert2_zinb2, type = "response")
  theta1 <- model_expert1_zinb2$family$getTheta(TRUE)
  theta2 <- model_expert2_zinb2$family$getTheta(TRUE)
  
  #Calculate log-likelihood for convergence check
  f1_i <- dnbinom(MEDITS_MERL_fit$nbtot, mu = mu1_i, size = theta1)
  f2_i <- dnbinom(MEDITS_MERL_fit$nbtot, mu = mu2_i, size = theta2)
  mixture_density <- p1_i * f1_i + p2_i * f2_i
  log_lik_i <- ifelse(MEDITS_MERL_fit$nbtot == 0,
                      log(phi_i + (1 - phi_i) * mixture_density),
                      log((1 - phi_i) * mixture_density))
  log_likelihood <- sum(log_lik_i)
  log_likelihood_history[t] <- log_likelihood
  
  #Check convergence
  param_change <- mean(abs(phi_i - phi_i_old))
  
  cat("Iteration:", t, 
      "| Log-Likelihood:", round(log_likelihood, 2),
      "| Param Change:", round(param_change, 6),
      "| theta1:", round(theta1, 3), 
      "| theta2:", round(theta2, 3),
      "| Mean p1:", round(mean(p1_i), 3), "\n")
}



final_results_zinb2<- list(
  vi_t = vi_t,                    #Weight for Zero gating network
  w1i_t = w1i_t,                  #Weight for expert 1
  w2i_t = w2i_t,                  #Weight for expert 2
  model_zi = model_zi,            #Zero-inflation model
  model_mixing = model_mixing,    #Mixing gating model
  model_expert1 = model_expert1_zinb2,   #Expert 1 model
  model_expert2 = model_expert2_zinb2,   #Expert 2 model
  theta1_final = theta1,          #Final theta for expert 1
  theta2_final = theta2,          #Final theta for expert 2
  log_likelihood = log_likelihood,
  log_likelihood_history = log_likelihood_history[1:t],
  n_iterations = t)



#Zero-Inflation gate
#Diagnostic plots
summary(final_results_zinb2$model_zi)

par(mfrow=c(2,2))
gam.check(final_results_zinb2$model_zi)
par(mfrow=c(1,1))
draw(final_results_zinb2$model_zi)
draw(final_results_zinb2$model_zi, select=c(2,3,4))

plot.gam(final_results_zinb2$model_zi,pages=1)

#Mixing gate
summary(final_results_zinb2$model_mixing) 

par(mfrow=c(2,2))
gam.check(final_results_zinb2$model_mixing)
par(mfrow=c(1,1))
draw(final_results_zinb2$model_mixing)
draw(final_results_zinb2$model_mixing, select=c(2,3,4))

plot.gam(final_results_zinb2$model_mixing,pages=1)


#Expert 1
summary(final_results_zinb2$model_expert1)

par(mfrow=c(2,2))
gam.check(final_results_zinb2$model_expert1)
par(mfrow=c(1,1))
draw(final_results_zinb2$model_expert1)
draw(final_results_zinb2$model_expert1, select=c(2,3,4,5,6))

plot.gam(final_results_zinb2$model_expert1,pages=1)


#Expert 2
summary(final_results_zinb2$model_expert2)


par(mfrow=c(2,2))
gam.check(final_results_zinb2$model_expert2)
par(mfrow=c(1,1))
draw(final_results_zinb2$model_expert2)
draw(final_results_zinb2$model_expert2, select=c(2,3,4,5,6))

plot.gam(final_results_zinb2$model_expert2,pages=1)




#Partial smooth plots graph of al the 4 models toghether #######################

#Draw each sub-model (non-spatial smooths only)
p_zi <- draw(final_results_zinb2$model_zi,      select = c(2, 3, 4)) +
  plot_annotation(
    title = "Zero-Inflation Gate",
    theme = theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))
  )

p_mixing <- draw(final_results_zinb2$model_mixing, select = c(2, 3, 4)) +
  plot_annotation(
    title = "Mixing Gate",
    theme = theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))
  )

p_expert1 <- draw(final_results_zinb2$model_expert1, select = c(2, 3, 4, 5, 6)) +
  plot_annotation(
    title = "Expert 1",
    theme = theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))
  )

p_expert2 <- draw(final_results_zinb2$model_expert2, select = c(2, 3, 4, 5, 6)) +
  plot_annotation(
    title = "Expert 2",
    theme = theme(plot.title = element_text(size = 23, face = "bold", hjust = 0.5))
  )

#2x2 patchwork
smooths_plot<-(wrap_elements(p_expert1) | wrap_elements(p_expert2))/(wrap_elements(p_zi)      | wrap_elements(p_mixing))
smooths_plot






# Calculate fitted values
prob_structural_zero <- predict(final_results_zinb2$model_zi, type = "response")
prob_expert1 <- predict(final_results_zinb2$model_mixing, type = "response")
prob_expert2 <- 1 - prob_expert1
mean_expert1 <- predict(final_results_zinb2$model_expert1, type = "response")
mean_expert2 <- predict(final_results_zinb2$model_expert2, type = "response")

# Overall fitted mean 
fitted_values <- (1 - prob_structural_zero) * (prob_expert1 * mean_expert1 + prob_expert2 * mean_expert2)

# Evaluation metrics
check_zinb2<- data.frame(
  observed = MEDITS_MERL_fit$nbtot, 
  fitted = fitted_values,
  phi = prob_structural_zero,
  p_expert1 = (1 - prob_structural_zero)* prob_expert1,
  p_expert2 = (1 - prob_structural_zero) * prob_expert2,
  mu1 = mean_expert1,
  mu2 = mean_expert2)

# Calculate metrics
MED_AE_zinb2<-median(abs(check_zinb2$observed - check_zinb2$fitted))
MED_AE_zinb2 #12.21073
MED_AE_baseline #65.9771

#AIC
edf_zi      <- sum(final_results_zinb2$model_zi$edf)
edf_mixing <- sum(final_results_zinb2$model_mixing$edf)
edf_exp1   <- sum(final_results_zinb2$model_expert1$edf)
edf_exp2   <- sum(final_results_zinb2$model_expert2$edf)

AIC_zinb2<- -2*final_results_zinb2$log_likelihood+2*(edf_zi+edf_mixing+edf_exp1+edf_exp2)
AIC_zinb2 #168215.7
AIC_zinb  #172264.1
AIC(model_nb) #174811


# Plot log-likelihood convergence
plot(1:final_results_zinb2$n_iterations, 
     final_results_zinb2$log_likelihood_history,
     type = "b", 
     xlab = "Iteration", 
     ylab = "Log-Likelihood",
     main = "EM Algorithm Convergence")


#ZINBMoE2 randomized quantile residuals (training 2000–2020)
#Extract final parameters from ZINBMoE2 model
phi_zinb2 <- predict(final_results_zinb2$model_zi, type = "response")
p1_zinb2 <- predict(final_results_zinb2$model_mixing, type = "response")
p2_zinb2 <- 1 - p1_zinb2
mu1_zinb2 <- predict(final_results_zinb2$model_expert1, type = "response")
mu2_zinb2 <- predict(final_results_zinb2$model_expert2, type = "response")
theta1_zinb2 <- final_results_zinb2$theta1_final
theta2_zinb2 <- final_results_zinb2$theta2_final
y <- MEDITS_MERL_fit$nbtot

# Calculate mixture density for NB experts
f1_zinb2 <- function(k) dnbinom(k, mu = mu1_zinb2, size = theta1_zinb2)
f2_zinb2 <- function(k) dnbinom(k, mu = mu2_zinb2, size = theta2_zinb2)
mixture_density_zinb2 <- function(k) p1_zinb2 * f1_zinb2(k) + p2_zinb2 * f2_zinb2(k)

#Lower bound: F(y-1)
F_y_minus_1_zinb2 <- ifelse(y == 0,
                            0,
                            phi_zinb2 + (1 - phi_zinb2) * (
                              p1_zinb2 * pnbinom(y - 1, mu = mu1_zinb2, size = theta1_zinb2) +
                                p2_zinb2 * pnbinom(y - 1, mu = mu2_zinb2, size = theta2_zinb2)
                            ))

#Upper bound: F(y)
F_y_zinb2 <- phi_zinb2 + (1 - phi_zinb2) * (
  p1_zinb2 * pnbinom(y, mu = mu1_zinb2, size = theta1_zinb2) +
    p2_zinb2 * pnbinom(y, mu = mu2_zinb2, size = theta2_zinb2)
)

#Generate uniform random variables
set.seed(2026)
u_zinb2 <- runif(length(y), min = F_y_minus_1_zinb2, max = F_y_zinb2)

#Transform to quantile residuals
q_res_zinb2 <- qnorm(u_zinb2)

#Histogram
hist(q_res_zinb2, 
     breaks = 50, 
     prob = TRUE,
     main = "Histogram of Quantile Residuals (ZINB2 - ZIMoE)",
     xlab = "Residuals", 
     col = "steelblue", 
     border = "white")

#Q-Q Plot
qqnorm(q_res_zinb2, 
       main = "Q-Q Plot (ZINB2 - ZIMoE with 2 NB Experts)")
qqline(q_res_zinb2, 
       col = "darkred", 
       lwd = 2)

#Residual diagnostics show a substantial improvement over the previously
#fitted models.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################## Model comparison (training 2000–2020) ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Use Median Absolute Error (MedAE), which is less sensitive to extreme values
#than mean-based metrics and is particularly useful for datasets with skewed
#or heavy-tailed error distributions (Hosamo et al. 2024).

#Compute absolute errors
check_zinb$abs_error  <- abs(check_zinb$observed  - check_zinb$fitted)
check_zinb2$abs_error <- abs(check_zinb2$observed - check_zinb2$fitted)
check_nb$abs_error     <- abs(check_nb$observed     - check_nb$predicted)

#Compute Median Absolute Error
MedAE_zinb  <- median(check_zinb$abs_error,  na.rm = TRUE)
MedAE_zinb2 <- median(check_zinb2$abs_error, na.rm = TRUE)
MedAE_nb    <- median(check_nb$abs_error,     na.rm = TRUE)

#Comparison table with Median Absoute Error and AIC
model_comparison <- data.frame(
  Model = c("NB", "ZINB", "ZINB2"),
  Median_Absolute_Error = c(MedAE_nb, MedAE_zinb, MedAE_zinb2),
  AIC = c(AIC(model_nb), AIC_zinb, AIC_zinb2)
)

model_comparison


#Randomized quantile residuals diagnostic comparison ##########################

#Compute common axis range
all_train_res <- c(q_res_nb, q_res_zinb, q_res_zinb2)
x_lim_train <- range(all_train_res, na.rm = TRUE)
y_lim_train <- range(all_train_res, na.rm = TRUE)

#Let's define make_qq_fixed a function to create resdiual plots with fixed axis
make_qq_fixed <- function(res, title, ylim) {
  df <- data.frame(r = res)
  ggplot(df, aes(sample = r)) +
    stat_qq(colour = "steelblue", alpha = 0.4, size = 0.8) +
    stat_qq_line(colour = "darkred", linewidth = 0.8) +
    coord_cartesian(ylim = ylim) +
    labs(title = title, x = "Theoretical quantiles", y = "Empirical quantiles") +
    theme_classic(base_size = 11) +
    theme(
      plot.title   = element_text(size = 10, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

make_hist_fixed <- function(res, title, xlim) {
  df <- data.frame(r = res)
  ggplot(df, aes(x = r)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 50, fill = "steelblue", colour = "white", linewidth = 0.2) +
    coord_cartesian(xlim = xlim) +
    labs(title = title, x = "Quantile residuals", y = "Density") +
    theme_classic(base_size = 11) +
    theme(
      plot.title       = element_text(size = 10, face = "bold"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#Training residual plots
p1 <- make_hist_fixed(q_res_nb,          "NB — Histogram (2000–2020)",       x_lim_train)
p2 <- make_hist_fixed(q_res_zinb,             "ZINB — Histogram (2000–2020)",     x_lim_train)
p3 <- make_hist_fixed(q_res_zinb2, "ZINBMoE2 — Histogram (2000–2020)", x_lim_train)

p4 <- make_qq_fixed(q_res_nb,          "NB — Q-Q Plot (2000–2020)",       y_lim_train)
p5 <- make_qq_fixed(q_res_zinb,             "ZINB — Q-Q Plot (2000–2020)",     y_lim_train)
p6 <- make_qq_fixed(q_res_zinb2, "ZINBMoE2 — Q-Q Plot (2000–2020)", y_lim_train)

#2 × 3 patchwork
training_plots <- (p1 | p2 | p3) /
  (p4 | p5 | p6) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 13, face = "bold",
                                            hjust = 0.5)))
training_plots




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########### Validation on 2021 holdout set (MEDITS_MERL_val) ####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#NB Model Validation ###########################################################

#Predictions on validation set
pred_nb_val <- predict(model_nb, newdata = MEDITS_MERL_val, type = "response")

check_nb_val <- data.frame(
  observed  = MEDITS_MERL_val$nbtot,
  predicted = pred_nb_val)

#MedAE
MedAE_nb_val <- median(abs(check_nb_val$observed - check_nb_val$predicted), na.rm = TRUE)
MedAE_nb_val

#Randomized quantile residuals 
theta_nb <- model_nb$family$getTheta(TRUE)
y_val    <- MEDITS_MERL_val$nbtot
mu_nb_val <- pred_nb_val

F_y_minus_1_nb_val <- ifelse(y_val == 0, 0,
                             pnbinom(y_val - 1, mu = mu_nb_val, size = theta_nb))
F_y_nb_val <- pnbinom(y_val, mu = mu_nb_val, size = theta_nb)

set.seed(2026)
u_nb_val <- runif(length(y_val), min = F_y_minus_1_nb_val, max = F_y_nb_val)
u_nb_val[u_nb_val >= 1] <- 1 - 1e-12
u_nb_val[u_nb_val <= 0] <- 1e-12
q_res_nb_val <- qnorm(u_nb_val)
q_res_nb_val <- q_res_nb_val[is.finite(q_res_nb_val)]




# ZINB Model Validation ########################################################

phi_zinb_val <- predict(final_results_zinb$model_zi,     newdata = MEDITS_MERL_val, type = "response")
mu_zinb_val  <- predict(final_results_zinb$model_expert, newdata = MEDITS_MERL_val, type = "response")
theta_zinb   <- final_results_zinb$theta_final

fitted_zinb_val <- (1 - phi_zinb_val) * mu_zinb_val

check_zinb_val <- data.frame(
  observed  = y_val,
  predicted = fitted_zinb_val)

MedAE_zinb_val <- median(abs(check_zinb_val$observed - check_zinb_val$predicted), na.rm = TRUE)
cat("ZINB  | MedAE (2021):", round(MedAE_zinb_val, 3), "\n")

#Randomized quantile residuals 
F_y_minus_1_zinb_val <- ifelse(y_val == 0, 0,
                               phi_zinb_val + (1 - phi_zinb_val) *
                                 pnbinom(y_val - 1, mu = mu_zinb_val, size = theta_zinb))
F_y_zinb_val         <- phi_zinb_val + (1 - phi_zinb_val) *
  pnbinom(y_val, mu = mu_zinb_val, size = theta_zinb)

set.seed(2026)
u_zinb_val <- runif(length(y_val), min = F_y_minus_1_zinb_val, max = F_y_zinb_val)
u_zinb_val[u_zinb_val >= 1] <- 1 - 1e-12
u_zinb_val[u_zinb_val <= 0] <- 1e-12
q_res_zinb_val <- qnorm(u_zinb_val)
q_res_zinb_val <- q_res_zinb_val[is.finite(q_res_zinb_val)]





#ZINBMoE2 Model Validation #####################################################

phi_zinb2_val <- predict(final_results_zinb2$model_zi,      newdata = MEDITS_MERL_val, type = "response")
p1_zinb2_val  <- predict(final_results_zinb2$model_mixing,  newdata = MEDITS_MERL_val, type = "response")
p2_zinb2_val  <- 1 - p1_zinb2_val
mu1_zinb2_val <- predict(final_results_zinb2$model_expert1, newdata = MEDITS_MERL_val, type = "response")
mu2_zinb2_val <- predict(final_results_zinb2$model_expert2, newdata = MEDITS_MERL_val, type = "response")
theta1_zinb2  <- final_results_zinb2$theta1_final
theta2_zinb2  <- final_results_zinb2$theta2_final

fitted_zinb2_val <- (1 - phi_zinb2_val) *
  (p1_zinb2_val * mu1_zinb2_val + p2_zinb2_val * mu2_zinb2_val)

check_zinb2_val <- data.frame(
  observed  = y_val,
  predicted = fitted_zinb2_val)

MedAE_zinb2_val <- median(abs(check_zinb2_val$observed - check_zinb2_val$predicted), na.rm = TRUE)
cat("ZINB2 | MedAE (2021):", round(MedAE_zinb2_val, 3), "\n")

#Randomized quantile residuals 
F_y_minus_1_zinb2_val <- ifelse(y_val == 0, 0,
                                phi_zinb2_val + (1 - phi_zinb2_val) * (
                                  p1_zinb2_val * pnbinom(y_val - 1, mu = mu1_zinb2_val, size = theta1_zinb2) +
                                    p2_zinb2_val * pnbinom(y_val - 1, mu = mu2_zinb2_val, size = theta2_zinb2)
                                ))
F_y_zinb2_val         <- phi_zinb2_val + (1 - phi_zinb2_val) * (
  p1_zinb2_val * pnbinom(y_val, mu = mu1_zinb2_val, size = theta1_zinb2) +
    p2_zinb2_val * pnbinom(y_val, mu = mu2_zinb2_val, size = theta2_zinb2)
)

set.seed(2026)
u_zinb2_val <- runif(length(y_val), min = F_y_minus_1_zinb2_val, max = F_y_zinb2_val)
u_zinb2_val[u_zinb2_val >= 1] <- 1 - 1e-12
u_zinb2_val[u_zinb2_val <= 0] <- 1e-12
q_res_zinb2_val <- qnorm(u_zinb2_val)
q_res_zinb2_val <- q_res_zinb2_val[is.finite(q_res_zinb2_val)]





# Summary comparison table #####################################################


validation_comparison <- data.frame(
  Model   = c("NB", "ZINB", "ZINBMoE2"),
  MedAE_train = c(MedAE_nb, MedAE_zinb, MedAE_zinb2),
  MedAE_val   = c(MedAE_nb_val, MedAE_zinb_val, MedAE_zinb2_val))
MED
print(validation_comparison)



#compute common axis ranges 
all_val_res <- c(q_res_nb_val, q_res_zinb_val, q_res_zinb2_val)
y_lim <- range(all_val_res, na.rm = TRUE)
x_lim <- range(all_val_res, na.rm = TRUE)  # same range for histogram x-axis

#Validation residual plots
p1_val <- make_hist_fixed(q_res_nb_val,    "NB — Histogram (2021)",      x_lim)
p2_val <- make_hist_fixed(q_res_zinb_val,  "ZINB — Histogram (2021)",    x_lim)
p3_val <- make_hist_fixed(q_res_zinb2_val, "ZINBMoE2 — Histogram (2021)",x_lim)

p4_val <- make_qq_fixed(q_res_nb_val,    "NB — Q-Q Plot (2021)",       y_lim)
p5_val <- make_qq_fixed(q_res_zinb_val,  "ZINB — Q-Q Plot (2021)",     y_lim)
p6_val <- make_qq_fixed(q_res_zinb2_val, "ZINBMoE2 — Q-Q Plot (2021)", y_lim)

#2 × 3 patchwork 
validation_plots<-(p1_val | p2_val | p3_val) /
  (p4_val | p5_val | p6_val) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 13, face = "bold",
                                            hjust = 0.5)))
validation_plots






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################# Predictive maps ###################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Predictions are restricted to GSAs surveyed by MEDITS and to the observed 
#bathymetric range (0–800 m).
#Following (Paradinas et al. 2022), abundance is expressed as specimens per hour 
#of trawling with standard MEDITS gear.

sf::sf_use_s2(TRUE) 

#Bathymetric data for the Mediterranean Sea were sourced from the EMODnet 2024
#Digital Terrain Model, accessed via the EMODnet geoviewer
#(https://emodnet.ec.europa.eu/geoviewer/). The 14 GeoTIFF tiles covering the
#study area (E4-E8, F4-F8, G5-G8) were downloaded, merged into a single raster
#using terra::merge(), and uploaded to Zenodo to ensure full reproducibility
#and open access (Rossi, 2026; DOI: 10.5281/zenodo.19111208).
#The unified raster is downloaded directly from Zenodo below.

options(timeout = 3600)  # Increase timeout to allow download of large file (1.1 GB)
tmp_tif <- tempfile(fileext = ".tif")
download.file(
  url      = "https://zenodo.org/records/19111208/files/MEDITS_bathy.tif",
  destfile = tmp_tif,
  mode     = "wb"
)
MEDITS_bathy       <- rast(tmp_tif)
MEDITS_bathy_layer <- MEDITS_bathy[[1]]

sf::sf_use_s2(FALSE)

#Read GSA areas shapefile from GitHub
#Shapefile of GSA areas was downloaded from FAO link:
#https://www.fao.org/gfcm/data/maps/gsas/en/
tmp_dir <- tempdir()
base_url <- "https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/GFCM_GSA/gfcm_gsa"

for (ext in c(".shp", ".shx", ".dbf", ".prj", ".CPG", ".sbn", ".sbx", ".shp.xml")) {
  download.file(
    url      = paste0(base_url, ext),
    destfile = file.path(tmp_dir, paste0("gfcm_gsa", ext)),
    mode     = "wb"
  )
}

gsa <- st_read(file.path(tmp_dir, "gfcm_gsa.shp")) |>
  st_make_valid() |>
  st_transform(4326)
#Exclude GSA areas not included in the MEDITS survey programme
exclude_names <- c("Northern Tunisia","Gulf of Hammamet", 
                   "Gulf of Gabès","Algeria","Marmara Sea","Black Sea","Azov Sea",
                   "Southern Levant Sea","Eastern Levant Sea","Southeastern Ionian Sea",
                   "Southwestern Ionian Sea","Southern Ionian Sea","Northern Levant Sea")

med_gsas <- gsa %>%
  filter(!SMU_NAME %in% exclude_names)
med_gsas$SMU_NAME



#Build prediction grid
bbox <- st_bbox(med_gsas)

lon_seq <- seq(bbox["xmin"], bbox["xmax"], by = 0.02)
lat_seq <- seq(bbox["ymin"], bbox["ymax"], by = 0.02)

grid <- expand.grid(lon = lon_seq, lat = lat_seq)

grid_sf <- st_as_sf(grid,
                    coords = c("lon","lat"),
                    crs = 4326)

#Filter points inside GSAs of interest
idx <- st_intersects(grid_sf, med_gsas)
inside <- lengths(idx) > 0

grid_gsa <- grid_sf[inside, ]

grid_gsa$SMU_NAME <- sapply(idx[inside],
                            function(i) med_gsas$SMU_NAME[i[1]])

#Extract bathimetry
grid_vect <- terra::vect(grid_gsa)
depth_vals <- terra::extract(MEDITS_bathy_layer, grid_vect)
grid_gsa$depth <- depth_vals[,2]

#Filter points shallower than 800 m
grid_final <- grid_gsa %>% filter(!is.na(depth) & depth > -800)

#Predictions
#Let's extract all the covariates over the prediction grid

#Transform coordinates to EPSG:3035 (projection used in the models) to compute
#predictions
grid_3035 <- st_transform(grid_final, 3035)
coords_3035 <- st_coordinates(grid_3035)

#Build prediction dataframe
MEDITS_pred <- cbind(
  st_drop_geometry(grid_final),          # keep original attributes
  longitude = st_coordinates(grid_final)[,1],  #decimal degrees
  latitude  = st_coordinates(grid_final)[,2],  #decimal degrees
  longitude_m = coords_3035[,1],         #meters (EPSG 3035)
  latitude_m  = coords_3035[,2]          #meters (EPSG 3035)
)
MEDITS_pred

#Depth was conventionally assumed to be positive in our model so let's change it accordingly
MEDITS_pred$depth<- -MEDITS_pred$depth

#Effort fixed at 65 min per haul (60 min trawl + 5 min), following (Paradinas 2022)
effort_mes_pred<-rep(65,length=nrow(MEDITS_pred))
MEDITS_pred$effort_mes<-effort_mes_pred

#shooting_slope
MEDITS_slope<-terrain(MEDITS_bathy_layer,v="slope", unit = "degrees")
coords_pred<-cbind(MEDITS_pred$longitude, MEDITS_pred$latitude)
MEDITS_pred$slope<- terra::extract(MEDITS_slope, coords_pred, method="bilinear")[,1]

#folk_5
#Initialize EMODnet Geology WFS client
geo_wfs <- emodnet_init_wfs_client(
  service = "geology_seabed_substrate_maps")

names_seabed<-emodnet_get_wfs_info(geo_wfs)

#Download seabed substrate map using the Folk 5-class scheme
folk5 <- emodnet_get_layers(
  wfs = geo_wfs,
  layers = "seabed_substrate_1m",   #coarser preferred for best data coverage
  outputFormat = "application/json"
)[[1]] %>%
  st_transform(4326)

class(folk5)
st_geometry_type(folk5)
names(folk5)


#Attach Folk 5 to prediction grid
# Convert prediction grid to sf (WGS84)
MEDITS_pred_sf <- st_as_sf(
  MEDITS_pred,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

#Aassign each grid point to the nearest polygon
nearest_id <- st_nearest_feature(MEDITS_pred_sf, folk5)
#Transfer Folk 5 class using nearest polygon
MEDITS_pred_sf$folk_5cl_txt <- folk5$folk_5cl_txt[nearest_id]
#Rename to match model variable
MEDITS_pred_sf$folk_5 <- MEDITS_pred_sf$folk_5cl_txt

#Enforce same factor levels as training data
MEDITS_pred_sf$folk_5 <- factor(
  MEDITS_pred_sf$folk_5,
  levels = levels(MEDITS_MERL_MOD$folk_5))

#Drop geometry for prediction
MEDITS_pred <- st_drop_geometry(MEDITS_pred_sf)

#Verify consistency
nrow(MEDITS_pred)
nrow(grid_final) 


#We are interested only in columns 1:8 and column 10
MEDITS_pred<-MEDITS_pred[, c(1:8,10)]
print(nrow(MEDITS_pred))

#Year and month must be fixed to generate a prediction map for a specific
#time point. Predictions are therefore conditional on a chosen year and month.
unique(MEDITS_MERL_MOD$year)
levels(MEDITS_MERL_MOD$month)

set_prediction_time <- function(pred_df=MEDITS_pred, year, month,df=MEDITS_MERL_MOD){
  
  #Allowed values from training data
  allowed_years  <- unique(df$year)
  allowed_months <- levels(df$month)
  
  #Check year validity
  if (!year%in% allowed_years) {
    stop("Selected year not present in training data.")
  }
  
  # Check month validity
  if (!month %in% allowed_months) {
    stop("Selected month not present in training data.")
  }
  
  #Assign year (numeric)
  pred_df$year <- year
  #Assign month (factor)
  pred_df$month <- factor(month,levels = allowed_months)
  return(pred_df)
}



# Download the Copernicus temperature Python script from GitHub once,
# so it can be reused across multiple calls to add_temps_to_pred()
tmp_py <- tempfile(fileext = ".py")
download.file(
  url      = "https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/copernicus_monthly_temps.py",
  destfile = tmp_py,
  mode     = "wb"
)


#This function retrieves monthly average bottom and surface temperatures from
#Copernicus Marine Service for a given year and month, and appends them to the
#prediction grid. Temperature extraction is handled by an external Python script
#called via reticulate, with automatic dependency installation if required.
add_temps_to_pred <- function(pred_df, year, month,
                              py_file = tmp_py,
                              install_deps = TRUE,
                              py_modules = c("numpy","pandas","xarray","dask","scipy","copernicusmarine")) {
  if (install_deps) {
    if ("py_require" %in% getNamespaceExports("reticulate")) {
      reticulate::py_require(py_modules)
    } else {
      missing <- py_modules[!vapply(py_modules, reticulate::py_module_available, logical(1))]
      if (length(missing) > 0) {
        reticulate::py_install(missing, pip = TRUE)
      }
    }
  }
  reticulate::source_python(py_file, convert = TRUE)
  pred_list <- add_monthly_temps_as_dict(pred_df, year = year, month = month)
  pred_df <- as.data.frame(pred_list, stringsAsFactors = FALSE)
  return(pred_df)
}

#Set the prediction time point and append Copernicus temperatures to the grid.
#Choose Jun-2020 as example predictive map (allowed: 2000-2021, Apr-Dec)
MEDITS_pred <- set_prediction_time(
  pred_df = MEDITS_pred,
  year= 2016,
  month= "Jun",
  df = MEDITS_MERL_MOD)

MEDITS_pred <- add_temps_to_pred(
  pred_df = MEDITS_pred,
  year = 2016,
  month = "Jun")

#Let's rename variable  according to MEDITS_MERL_MOD to allow for predict() function
#to be used
names(MEDITS_pred)[names(MEDITS_pred) == "latitude_m"]<-"shooting_latitude_m"
names(MEDITS_pred)[names(MEDITS_pred) == "longitude_m"]<-"shooting_longitude_m"
names(MEDITS_pred)[names(MEDITS_pred) == "depth"]<-"shooting_depth"
names(MEDITS_pred)[names(MEDITS_pred) == "slope"]<-"shooting_slope"


#Ensure consistent factor enconding for folk_5 and month
MEDITS_pred$folk_5 <- factor(
  MEDITS_pred$folk_5,
  levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred$month <- factor(
  MEDITS_pred$month,
  levels = levels(MEDITS_MERL_MOD$month))

MEDITS_pred
nrow(MEDITS_pred)


# ZINBMoE2 predictive map (June 2016) ##########################################

#Work on a fresh copy of the grid
grid_zinb2 <- MEDITS_pred

#Predict zero-inflation probability at each grid location
phi_pred2 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2, type = "response")

#Predict gating network mixing probabilities
p1_pred   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2, type = "response")
p2_pred   <- 1 - p1_pred

#Predict the conditional mean of each NB expert
mu1_pred  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2, type = "response")
mu2_pred  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2, type = "response")

#Compute the ZINBMoE2 expected NPUE as (1 - phi) * (p1*mu1 + p2*mu2)
predicted_NPUE_ZINB2 <- (1 - phi_pred2) * (p1_pred * mu1_pred + p2_pred * mu2_pred)
grid_zinb2$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2

#Probability that a non-zero observation belongs to expert 2
grid_zinb2$nursery_prob <- (1 - phi_pred2) * p2_pred

#Structural zero probability at each grid location
grid_zinb2$structural_zero_prob <- phi_pred2

#Remove the two most extreme predicted values as they are likely caused by measurement
#errors
grid_zinb2 <- grid_zinb2 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)


#Prepare grid data with classification
grid_zinb2 <- grid_zinb2 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1–50", "50–100",
               "100–200", "200–400", ">400")
  ))

grid_df_zinb2 <- grid_zinb2 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

# Plot
map_zinbmoe2_2016<-ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  #Enlarge legend keys for readability
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  #Format axis labels with cardinal directions
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  #Set map theme: light blue sea, no gridlines, black border and axis ticks
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )

map_zinbmoe2_2016




# ZINBMoE2 predictive maps for June 2000, 2005, 2010, 2015, 2020 ###############

# Predictions are generated for June across five years to visualise how the
# spatial distribution of hake has changed over the survey period.

#June 2000 #####

MEDITS_pred_2000 <- MEDITS_pred
MEDITS_pred_2000 <- set_prediction_time(
  pred_df = MEDITS_pred_2000,
  year = 2000,
  month = "Jun",
  df = MEDITS_MERL_MOD)
MEDITS_pred_2000 <- add_temps_to_pred(
  pred_df = MEDITS_pred_2000,
  year = 2000,
  month = "Jun",
  install_deps = FALSE)
MEDITS_pred_2000$folk_5 <- factor(MEDITS_pred_2000$folk_5, levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred_2000$month  <- factor(MEDITS_pred_2000$month,  levels = levels(MEDITS_MERL_MOD$month))

grid_zinb2_2000 <- MEDITS_pred_2000
phi_pred2_2000 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2_2000, type = "response")
p1_pred_2000   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2_2000, type = "response")
p2_pred_2000   <- 1 - p1_pred_2000
mu1_pred_2000  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2_2000, type = "response")
mu2_pred_2000  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2_2000, type = "response")
predicted_NPUE_ZINB2_2000 <- (1 - phi_pred2_2000) * (p1_pred_2000 * mu1_pred_2000 + p2_pred_2000 * mu2_pred_2000)
grid_zinb2_2000$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2_2000
grid_zinb2_2000$nursery_prob <- (1 - phi_pred2_2000) * p2_pred_2000
grid_zinb2_2000$structural_zero_prob <- phi_pred2_2000
grid_zinb2_2000 <- grid_zinb2_2000 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)
grid_zinb2_2000 <- grid_zinb2_2000 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1\u201350", "50\u2013100",
               "100\u2013200", "200\u2013400", ">400")
  ))
grid_df_zinb2_2000 <- grid_zinb2_2000 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

map_zinbmoe2_2000 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2_2000, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )


map_zinbmoe2_2000



#June 2005 #####

MEDITS_pred_2005 <- MEDITS_pred
MEDITS_pred_2005 <- set_prediction_time(
  pred_df = MEDITS_pred_2005,
  year = 2005,
  month = "Jun",
  df = MEDITS_MERL_MOD)
MEDITS_pred_2005 <- add_temps_to_pred(
  pred_df = MEDITS_pred_2005,
  year = 2005,
  month = "Jun",
  install_deps = FALSE)
MEDITS_pred_2005$folk_5 <- factor(MEDITS_pred_2005$folk_5, levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred_2005$month  <- factor(MEDITS_pred_2005$month,  levels = levels(MEDITS_MERL_MOD$month))

grid_zinb2_2005 <- MEDITS_pred_2005
phi_pred2_2005 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2_2005, type = "response")
p1_pred_2005   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2_2005, type = "response")
p2_pred_2005   <- 1 - p1_pred_2005
mu1_pred_2005  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2_2005, type = "response")
mu2_pred_2005  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2_2005, type = "response")
predicted_NPUE_ZINB2_2005 <- (1 - phi_pred2_2005) * (p1_pred_2005 * mu1_pred_2005 + p2_pred_2005 * mu2_pred_2005)
grid_zinb2_2005$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2_2005
grid_zinb2_2005$nursery_prob <- (1 - phi_pred2_2005) * p2_pred_2005
grid_zinb2_2005$structural_zero_prob <- phi_pred2_2005
grid_zinb2_2005 <- grid_zinb2_2005 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)
grid_zinb2_2005 <- grid_zinb2_2005 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1\u201350", "50\u2013100",
               "100\u2013200", "200\u2013400", ">400")
  ))
grid_df_zinb2_2005 <- grid_zinb2_2005 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

map_zinbmoe2_2005 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2_2005, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )


map_zinbmoe2_2005



#June 2010 #####

MEDITS_pred_2010 <- MEDITS_pred
MEDITS_pred_2010 <- set_prediction_time(
  pred_df = MEDITS_pred_2010,
  year = 2010,
  month = "Jun",
  df = MEDITS_MERL_MOD)
MEDITS_pred_2010 <- add_temps_to_pred(
  pred_df = MEDITS_pred_2010,
  year = 2010,
  month = "Jun",
  install_deps = FALSE)
MEDITS_pred_2010$folk_5 <- factor(MEDITS_pred_2010$folk_5, levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred_2010$month  <- factor(MEDITS_pred_2010$month,  levels = levels(MEDITS_MERL_MOD$month))

grid_zinb2_2010 <- MEDITS_pred_2010
phi_pred2_2010 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2_2010, type = "response")
p1_pred_2010   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2_2010, type = "response")
p2_pred_2010   <- 1 - p1_pred_2010
mu1_pred_2010  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2_2010, type = "response")
mu2_pred_2010  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2_2010, type = "response")
predicted_NPUE_ZINB2_2010 <- (1 - phi_pred2_2010) * (p1_pred_2010 * mu1_pred_2010 + p2_pred_2010 * mu2_pred_2010)
grid_zinb2_2010$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2_2010
grid_zinb2_2010$nursery_prob <- (1 - phi_pred2_2010) * p2_pred_2010
grid_zinb2_2010$structural_zero_prob <- phi_pred2_2010
grid_zinb2_2010 <- grid_zinb2_2010 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)
grid_zinb2_2010 <- grid_zinb2_2010 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1\u201350", "50\u2013100",
               "100\u2013200", "200\u2013400", ">400")
  ))
grid_df_zinb2_2010 <- grid_zinb2_2010 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

map_zinbmoe2_2010 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2_2010, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )


map_zinbmoe2_2010



#June 2015 #####

MEDITS_pred_2015 <- MEDITS_pred
MEDITS_pred_2015 <- set_prediction_time(
  pred_df = MEDITS_pred_2015,
  year = 2015,
  month = "Jun",
  df = MEDITS_MERL_MOD)
MEDITS_pred_2015 <- add_temps_to_pred(
  pred_df = MEDITS_pred_2015,
  year = 2015,
  month = "Jun",
  install_deps = FALSE)
MEDITS_pred_2015$folk_5 <- factor(MEDITS_pred_2015$folk_5, levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred_2015$month  <- factor(MEDITS_pred_2015$month,  levels = levels(MEDITS_MERL_MOD$month))

grid_zinb2_2015 <- MEDITS_pred_2015
phi_pred2_2015 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2_2015, type = "response")
p1_pred_2015   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2_2015, type = "response")
p2_pred_2015   <- 1 - p1_pred_2015
mu1_pred_2015  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2_2015, type = "response")
mu2_pred_2015  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2_2015, type = "response")
predicted_NPUE_ZINB2_2015 <- (1 - phi_pred2_2015) * (p1_pred_2015 * mu1_pred_2015 + p2_pred_2015 * mu2_pred_2015)
grid_zinb2_2015$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2_2015
grid_zinb2_2015$nursery_prob <- (1 - phi_pred2_2015) * p2_pred_2015
grid_zinb2_2015$structural_zero_prob <- phi_pred2_2015
grid_zinb2_2015 <- grid_zinb2_2015 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)
grid_zinb2_2015 <- grid_zinb2_2015 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1\u201350", "50\u2013100",
               "100\u2013200", "200\u2013400", ">400")
  ))
grid_df_zinb2_2015 <- grid_zinb2_2015 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

map_zinbmoe2_2015 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2_2015, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )


map_zinbmoe2_2015



#June 2020 #####

MEDITS_pred_2020 <- MEDITS_pred
MEDITS_pred_2020 <- set_prediction_time(
  pred_df = MEDITS_pred_2020,
  year = 2020,
  month = "Jun",
  df = MEDITS_MERL_MOD)
MEDITS_pred_2020 <- add_temps_to_pred(
  pred_df = MEDITS_pred_2020,
  year = 2020,
  month = "Jun",
  install_deps = FALSE)
MEDITS_pred_2020$folk_5 <- factor(MEDITS_pred_2020$folk_5, levels = levels(MEDITS_MERL_MOD$folk_5))
MEDITS_pred_2020$month  <- factor(MEDITS_pred_2020$month,  levels = levels(MEDITS_MERL_MOD$month))

grid_zinb2_2020 <- MEDITS_pred_2020
phi_pred2_2020 <- predict(final_results_zinb2$model_zi,      newdata = grid_zinb2_2020, type = "response")
p1_pred_2020   <- predict(final_results_zinb2$model_mixing,  newdata = grid_zinb2_2020, type = "response")
p2_pred_2020   <- 1 - p1_pred_2020
mu1_pred_2020  <- predict(final_results_zinb2$model_expert1, newdata = grid_zinb2_2020, type = "response")
mu2_pred_2020  <- predict(final_results_zinb2$model_expert2, newdata = grid_zinb2_2020, type = "response")
predicted_NPUE_ZINB2_2020 <- (1 - phi_pred2_2020) * (p1_pred_2020 * mu1_pred_2020 + p2_pred_2020 * mu2_pred_2020)
grid_zinb2_2020$predicted_NPUE_ZINB2 <- predicted_NPUE_ZINB2_2020
grid_zinb2_2020$nursery_prob <- (1 - phi_pred2_2020) * p2_pred_2020
grid_zinb2_2020$structural_zero_prob <- phi_pred2_2020
grid_zinb2_2020 <- grid_zinb2_2020 %>%
  arrange(desc(predicted_NPUE_ZINB2)) %>%
  slice(-(1:2)) %>%
  arrange(predicted_NPUE_ZINB2)
grid_zinb2_2020 <- grid_zinb2_2020 %>%
  mutate(NPUE_class_ZINB2 = cut(
    predicted_NPUE_ZINB2,
    breaks = c(0, 1, 50, 100, 200, 400, Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0-1", "1\u201350", "50\u2013100",
               "100\u2013200", "200\u2013400", ">400")
  ))
grid_df_zinb2_2020 <- grid_zinb2_2020 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, predicted_NPUE_ZINB2, NPUE_class_ZINB2) %>%
  rename(NPUE_class = NPUE_class_ZINB2) %>%
  filter(!is.na(NPUE_class))

map_zinbmoe2_2020 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_zinb2_2020, 
             aes(x = longitude, y = latitude, color = NPUE_class), 
             size = 0.25) +
  scale_color_manual(
    name = "Predicted NPUE",
    values = c(
      "0-1"     = "#440154",
      "1–50"    = "#3b528b",
      "50–100"  = "#21918c",
      "100–200" = "#5ec962",
      "200–400" = "#fde725",
      ">400"    = "#d73027"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm")
  )

map_zinbmoe2_2020





#ZINBMoE2 nursery probability map (June 2016) ##################################

#Classify probabilities
grid_zinb2 <- grid_zinb2 %>%
  mutate(nursery_class = cut(
    nursery_prob,
    breaks = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 1),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0–0.1", "0.1–0.2", "0.2–0.3",
               "0.3–0.5", "0.5–0.7", "0.7–0.8",
               "0.8–0.9", "0.9–1")
  ))

#Retain only columns needed for plotting
grid_df_nursery <- grid_zinb2 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, nursery_prob, nursery_class) %>%
  filter(!is.na(nursery_class))

#Plot
map_nursery_2016 <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_nursery,
             aes(x = longitude, y = latitude, color = nursery_class),
             size = 0.25) +
  scale_color_manual(
    name = "Nursery probability",
    values = c(
      "0–0.1"   = "#440154",
      "0.1–0.2" = "#482878",
      "0.2–0.3" = "#3e4989",
      "0.3–0.5" = "#31688e",
      "0.5–0.7" = "#1f9e89",
      "0.7–0.8" = "#6ece58",
      "0.8–0.9" = "#b5de2b",
      "0.9–1"   = "#fde725"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 12, color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.6),
    legend.position  = "right",
    legend.title     = element_text(size = 14, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.4, "cm")
  )

map_nursery_2016





# ZINBMoE2 structural zero probability map (June 2016) #########################

#Classify probabilities
grid_zinb2 <- grid_zinb2 %>%
  mutate(sz_class = cut(
    structural_zero_prob,
    breaks = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 1),
    include.lowest = TRUE,
    right = FALSE,
    labels = c("0–0.1", "0.1–0.2", "0.2–0.3",
               "0.3–0.5", "0.5–0.7", "0.7–0.8",
               "0.8–0.9", "0.9–1")
  ))

grid_df_sz <- grid_zinb2 %>%
  as.data.frame() %>%
  dplyr::select(longitude, latitude, structural_zero_prob, sz_class) %>%
  filter(!is.na(sz_class))

map_sz <- ggplot() +
  geom_sf(data = countries_med,
          fill = "grey62",
          color = "grey62",
          linewidth = 0.2) +
  geom_point(data = grid_df_sz,
             aes(x = longitude, y = latitude, color = sz_class),
             size = 0.25) +
  scale_color_manual(
    name = "P(structural zero)",
    values = c(
      "0–0.1"   = "#440154",
      "0.1–0.2" = "#3b528b",
      "0.2–0.3" = "#21918c",
      "0.3–0.5" = "#5ec962",
      "0.5–0.7" = "#fde725",
      "0.7–0.8" = "#fd8d3c",
      "0.8–0.9" = "#d73027",
      "0.9–1"   = "#e31a1c"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 5, shape = 15)
  )) +
  scale_x_continuous(
    breaks = seq(-5, 35, by = 5),
    labels = function(x) {
      ifelse(x < 0, paste0(abs(x), "°W"),
             ifelse(x == 0, "0°", paste0(x, "°E")))
    }
  ) +
  scale_y_continuous(labels = function(y) paste0(y, "°N")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightcyan2"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_text(size = 12, color = "black"),
    axis.ticks       = element_line(color = "black", linewidth = 0.6),
    legend.position  = "right",
    legend.title     = element_text(size = 14, face = "bold"),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.4, "cm")
  )

map_sz



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###################### Residual map validation (2021) ##########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#Raw residuals on validation set 
resid_nb_val    <- MEDITS_MERL_val$nbtot - pred_nb_val

resid_zinb_val  <- MEDITS_MERL_val$nbtot - fitted_zinb_val

resid_zinb2_val <- MEDITS_MERL_val$nbtot - fitted_zinb2_val

#Convert EPSG:3035 validation coordinates to WGS84
sample_sf_val <- st_as_sf(
  data.frame(
    x = MEDITS_MERL_val$shooting_longitude_m,
    y = MEDITS_MERL_val$shooting_latitude_m
  ),
  coords = c("x", "y"), crs = 3035
) %>% st_transform(4326)

lonlat_val <- as.data.frame(st_coordinates(sample_sf_val))
names(lonlat_val) <- c("longitude", "latitude")

#Classify residuals (same breaks as training maps)
resid_breaks <- c(-Inf, -200, -100, -50, -10, 10, 50, 100, 200, Inf)
resid_labels <- c("< -200", "-200–-100", "-100–-50", "-50–-10",
                  "-10–10", "10–50", "50–100", "100–200", "> 200")

# Same palette as training residual maps
resid_colors <- c(
  "< -200"    = "#08306b",   
  "-200–-100" = "#2171b5",   
  "-100–-50"  = "#6baed6",   
  "-50–-10"   = "#c6dbef",   
  "-10–10"    = "#ffd600",   
  "10–50"     = "#f4a582",
  "50–100"    = "#d6604d",
  "100–200"   = "#b2182b",
  "> 200"     = "#67001f"
)

resid_df_val <- data.frame(
  longitude       = lonlat_val$longitude,
  latitude        = lonlat_val$latitude,
  resid_nb_val    = resid_nb_val,
  resid_zinb_val  = resid_zinb_val,
  resid_zinb2_val = resid_zinb2_val
) %>%
  mutate(
    class_nb_val    = cut(resid_nb_val,    breaks = resid_breaks,
                          labels = resid_labels, include.lowest = TRUE),
    class_zinb_val  = cut(resid_zinb_val,  breaks = resid_breaks,
                          labels = resid_labels, include.lowest = TRUE),
    class_zinb2_val = cut(resid_zinb2_val, breaks = resid_breaks,
                          labels = resid_labels, include.lowest = TRUE)
  )

#Shared plot function 
plot_resid_map_val <- function(df, class_col) {
  ggplot() +
    geom_sf(data = countries_med,
            fill = "grey62", color = "grey62", linewidth = 0.2) +
    geom_point(data = df,
               aes(x = longitude, y = latitude,
                   color = .data[[class_col]]),
               size = 0.6, alpha = 0.7) +
    scale_color_manual(
      name   = "Raw residual\n(obs – fitted)",
      values = resid_colors,
      drop   = FALSE
    ) +
    guides(color = guide_legend(
      override.aes = list(size = 5, shape = 15)
    )) +
    scale_x_continuous(
      breaks = seq(-5, 35, by = 5),
      labels = function(x) {
        ifelse(x < 0, paste0(abs(x), "°W"),
               ifelse(x == 0, "0°", paste0(x, "°E")))
      }
    ) +
    scale_y_continuous(labels = function(y) paste0(y, "°N")) +
    coord_sf(expand = FALSE) +
    theme_minimal() +
    theme(
      plot.title       = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_text(size = 12, color = "black"),
      axis.ticks       = element_line(color = "black", linewidth = 0.6),
      legend.position  = "right",
      legend.title     = element_text(size = 14, face = "bold"),
      legend.text      = element_text(size = 9),
      legend.key.size  = unit(0.4, "cm")
    ) +
    labs(x = NULL, y = NULL)
}

#individual maps
p_nb_val    <- plot_resid_map_val(resid_df_val, "class_nb_val")
p_zinb_val  <- plot_resid_map_val(resid_df_val, "class_zinb_val")
p_zinb2_val <- plot_resid_map_val(resid_df_val, "class_zinb2_val")

p_nb_val
p_zinb_val
p_zinb2_val

#combined panel
residual_maps_validation<-(p_nb_val / p_zinb_val / p_zinb2_val)



