#Thesis code 


#Ask if its possible to load the dataset of github as it will be more practical
library(tidyverse)
library(dplyr)      #to merge the datasets
library(viridis)   #color palette that works well for colorblind readers  
#As explained on the R documentation:
#'cividis', a corrected version of 'viridis', 'cividis', 
#'developed by Jamie R. Nuñez, Christopher R. Anderton, and 
#'Ryan S. Renslow, and originally ported to R by Marco Sciaini.
#'It is designed to be perceived by readers with all forms of color blindness.
library(scales)    #to format numbers and axis labels  
library(GGally)    #extends some features of ggplot2  
library(gridExtra) #allows combining several plots into one figure
library(patchwork) #allows combining several plots into one figure
library(statmod)   #to compute quantile residuals
library(mgcv)      #used to fit Generalized Additive Models(GAM)  
library(magrittr)  #for pipe operators 
library(Metrics)   #offers functions to calculate model accuracy measures  
library(corrplot)  #for plotting correlation matrices
library(car)       #to compute the variance inflation factors
library(caret)     #to perform cross validation
library(knitr)     #to create tables


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################### DATA WRANGLING #################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#Load the data
hake<-readRDS("C:\\Thesis\\hake.rds")


################################## DATASET TA ##################################

#TA constains haul specific data
TA<-read_csv("https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/TA.csv")


# MEDITS TA (Trawl Acoustic) Dataset - Haul Operations Description
# This dataset contains operational and environmental data for each trawl haul

#Let's inspect each variable now:

# COUNTRY: 3A (3-character alphabetic) - Country conducting the survey (ISO code)
# AREA: 2N (2-digit numeric) - Geographic area code (GFCM coding system)
# VESSEL: 3A (3-character alphabetic) - Research vessel identifier 
# GEAR: 5A (5-character alphabetic) - Type of fishing gear used 
# RIGGING: 4AN (4-character alphanumeric) - Trawl gear configuration
# DOOR: 4AN (4-character alphanumeric) - Trawl door type and specification 
# YEAR: 4N (4-digit numeric) - Survey year
# MONTH: 2N (2-digit numeric) - Survey month 
# DAY: 2N (2-digit numeric) - Survey day 
# HAUL_NUMBER: 3N (3-digit numeric) - Unique haul identifier within vessel/year
# CODEND_CLOSING: 1A (1-character alphabetic) - Codend closure type: "S"=without, "C"=controlled
# Explenation about this covariate:
# CODEND DEFINITION:
# The codend is the terminal, bag-like section of the trawl net where captured organisms accumulate.
# It functions as the collection chamber at the posterior end of the net system.
# 2 levels:
# S = Without Closing (Standard)
# - The codend remains permanently open throughout the entire trawl operation
# C = Controlled Closing
# - The codend is equipped with a remote-opening/closing mechanism

#Start of the trawl covariates:
# SHOOTING_TIME: 4N (4-digit numeric) - Time when trawl was deployed (decimal hours, e.g., 6.46 = 06:28)
# SHOOTING_QUADRANT: 1N (1-digit numeric) - Geographic quadrant for shooting position (1-4) (start time position)
#Explanation of the covariate:
#This is just a location varibale of little interest that tell in whichquadrant of the emisphere
#the trawl was conducted. See pag 43 of Medits_Handbook_2017_version_9_5-60417r
# SHOOTING_LATITUDE: 7N (7-digit numeric) - Latitude at trawl deployment (DDMM.MMM format, e.g., 4338.057 = 43°38.057'N)
# SHOOTING_LONGITUDE: 7N (7-digit numeric) - Longitude at trawl deployment (DDMM.MMM format, e.g., 2843.600 = 28°43.600'E)
# SHOOTING_DEPTH: 3N (3-digit numeric) - Water depth at shooting location (meters)

#End of the trawl covariates:
# HAULING_TIME: 4N (4-digit numeric) - Time when trawl was retrieved (decimal hours)
# HAULING_QUADRANT: 1N (1-digit numeric) - Geographic quadrant for hauling position (1-4) (end time position)
# HAULING_LATITUDE: 7N (7-digit numeric) - Latitude at trawl retrieval (DDMM.MMM format)
# HAULING_LONGITUDE: 7N (7-digit numeric) - Longitude at trawl retrieval (DDMM.MMM format)
# HAULING_DEPTH: 3N (3-digit numeric) - Water depth at hauling location (meters)

# HAULING_DURATION: 2N (2-digit numeric) - Duration of trawl operation (5-90 minutes)
# VALIDITY: 1A (1-character alphabetic) - Haul validity: "V"=valid, "I"=invalid
# COURSE: 1A (1-character alphabetic) - Vessel course during trawl: "R"=rectilinear, "N"=not rectilinear
#Explanation of the covariate:
# COURSE describes the vessel's steering pattern during trawling:
# "R" = Rectilinear (Straight-line course)
# - Vessel maintains a constant, straight heading
# - Ideal for standardized sampling and area calculation
# "N" = Not Rectilinear (Non-straight course)   #Consider deliting obs with Course="N"
# - Vessel changes direction during trawling

# RECORDED_SPECIES: 1N (1-digit numeric) Information about species caught
#0  No standard species recorded   
#1  Only the species of the reference list are recorded 
#2  The species of the reference list plus some others are 
#   recorded 
#3  All the caught species are recorded
#4  Species from a national list

# DISTANCE: 4N (4-digit numeric) - Distance traveled over ground during trawl (1000-9999 meters)
# VERTICAL_OPENING: 3N (3-digit numeric) - Vertical opening of trawl net (10-100 decimeters)
#It is the height from seafloor to top of net mouth
# WING_OPENING: 3N (3-digit numeric) - Horizontal wing opening of trawl (50-250 decimeters)
#It is the width of the net mouth.
# GEOMETRICAL_PRECISION: 1A (1-character alphabetic) - Precision of opening measurements: "M"=measured, "E"=estimated


# BRIDLES_LENGTH: 3N (3-digit numeric) - Length of bridles(100, 150, or 200 meters)
# WARP_LENGTH: 4N (4-digit numeric) - Length of warp cables (100-2200 meters)
# WARP_DIAMETER: 2N (2-digit numeric) - Diameter of warp cables (10-30 millimeters)
#Bridles
#These are the shorter cables that connect the trawl doors to the mouth of the 
#net.   #See thesis images word file
#Warps:
#These are the main towing cables that run from the vessel down to the trawl doors.
#They’re usually very long—often hundreds or even thousands of meters depending on the fishing depth.
# HYDROLOGICAL_STATION: 5A/2A (5 or 2-character alphabetic) - Hydrological station identifier or 0 (if 
# no further oceanographic measurment has been conducted)

# OBSERVATIONS: 1N (1-digit numeric) - Additional observations code
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
# TYPE_OF_FILE: 2A (2-character alphabetic) - File type identifier: "TA" (fixed value) 
#it will be TA for all the observations in this dataset

# BOTTOM_TEMPERATURE_BEGINNING: 5N/2A (5-digit numeric or 2A) - Bottom(at the seabed level) temp at start (0-30°C, 2 decimals) or "NA"
# BOTTOM_TEMPERATURE_END: 5N/2A (5-digit numeric or 2A) - Bottom temp at end (0-30°C, 2 decimals) or "NA"
# MEASURING_SYSTEM_TEMP: 2A (2-character alphabetic) - Temperature measurement system (see Annex X) or "NA"

# NUMBER_OF_THE_STRATUM: 6AN (6-character alphanumeric) - Sampling stratum identifier (see Annex II)
#Unclear how they are computed
#Example:
## FORMAT: [Country][Area][Sub-area][DepthBand]
# 1 1 1 05
# │ │ │  │
# │ │ │  └── Depth Band (05 = 500-800m)
# │ │ └───── Sub-area (1 = "a" in your data)
# │ └─────── Geographic Area (1 = Alboran Sea region)
# └───────── Country (1 = Spain)
# PART_OF_THE_CODEND: 1A (1-character alphabetic) - Codend sampling section: "A"=anterior, "M"=middle, "P"=posterior, "S"=sum
#it's unclear whether multiple observations for the same haul for different
#PART_OF_THE_CODEND are present in the datase.
#A rapid check confirms that this is not the case:
keys<-c("country","area","vessel","year","month","day","haul_number","name_of_survey") 
#This set of keys uniquely  identify a haul.
TA_grouped <- group_by(TA, across(all_of(keys)))
TA_summary <- summarise(TA_grouped,
                        n_parts = n_distinct(part_of_the_codend),
                        .groups = "drop")
nrow(filter(TA_summary, n_parts > 1)) #0 confirming that no multiple observations
#for the same haul, for different PART_OF_THE_CODEND are present in the dataset.


# NAME_OF_SURVEY: 10A (10-character alphabetic) - Survey identifier

# BOTTOM_SALINITY_BEGINNING: 5N/2A (5-digit numeric or 2A) - Bottom(at seabed level) salinity at start (0-50 ppt, 2 decimals) or "NA"
# BOTTOM_SALINITY_END: 5N/2A (5-digit numeric or 2A) - Bottom salinity at end (0-50 ppt, 2 decimals) or "NA"
# MEASURING_SYSTEM_SAL: 2A (2-character alphabetic) - Salinity measurement system (see Annex X) or "NA"

# Note: Missing values are typically coded as -1 or "NA" in this dataset.
# Coordinates are in DDMM.MMM format (Degrees, Decimal Minutes).
# All depth measurements are in meters, distances in meters, openings in decimeters.



################################## DATASET TB ##################################

#TB contains haul-species specific data
TB<-read_csv("https://raw.githubusercontent.com/DavideRossi123/Thesis-Project-6/main/TB.csv")

# MEDITS TB (Trawl Biology) Dataset - Species Catch Data Description
# This dataset contains species-specific biological catch data for each trawl haul

# COUNTRY: 3A - Country conducting the survey (ISO code) [ALSO IN TA]
# AREA: 2N - Geographic area code (GFCM coding system) [ALSO IN TA] 
# VESSEL: 3A - Research vessel identifier [ALSO IN TA]
# YEAR: 4N - Survey year [ALSO IN TA]
# HAUL_NUMBER: 3N - Unique haul identifier within vessel/year [ALSO IN TA]
# MONTH: 2N - Survey month [ALSO IN TA]
# DAY: 2N - Survey day [ALSO IN TA]
# NAME_OF_SURVEY: - Survey identifier [ALSO IN TA]

# CODEND_CLOSING: 1A - Codend closure type: "S"=without, "C"=controlled [ALSO IN TA]
# PARTIT: 1A - Part of codend sampled: "A"=anterior, "M"=middle, "P"=posterior, "S"=sum  #Equivalent to PART_OF_THE_CODEND in TA
#Explanation of the covariate:
#specifies which part of the codend (the end section of the trawl net where the catch accumulates) was sampled or recorded.
#A = anterior (front part of the codend)
#M = middle section
#P = posterior (rear end, where the catch accumulates)
#S = sum or total codend — the entire codend sampled as one unit, not divided into sections.


# CATFAU: 3A - taxonomic group to which the species belong to
# GENUS: 4A - Genus code following MEDITS reference list (e.g., "MERL" = Merluccius)
# SPECIES: 3A - Species code following MEDITS reference list (e.g., "MER" = merluccius)
# LREF: 2A - Name of reference list used: "FM" = FishMed, "NCC" = National Coding, "MEDITS"
# This doesn't impact hoe genus was collected.
# PTOT: 7N - Total weight of species in the haul (grams)
# NBTOT: 7N - Total number of individuals in the haul for the species
# NBFEM: 7N - Number of female individuals
# NBMAL: 7N - Number of male individuals
# NBIND: 7N - Number of undetermined sex individuals
# TF: 2A - Type of file identifier: "TB" (fixed value). The value will be TB for all
# the observations.



#TC contains haul-species-maturity specif data (not of interest for our analysis,for now at least)
#Could be useful to inspect the age and sex composition of the Merluccius Merluccius(MM) .
#In this case in which we would like to estimate which areas are nursery areas ecc..(Paradinas 2015,2022)
#TC<-read_csv("C:\\Thesis\\Demersal\\TC.csv")



#Let's subset TA and TB recalling that our period of interest is 2010-2016 
#and the survey of interest is the MEDITS one
TA<-filter(TA,year>=2000 & year<=2016,name_of_survey=="MEDITS")
TB<-filter(TB,year>=2000 & year<=2016,name_of_survey=="MEDITS")



#Merging code development section####
#Let's use only a limited section of the dataaset to dhceck if the merging 
#algorithm works appropriately:
TA.dev<-TA[10:12,]  #let's consider hauls 10-12 (in Ciprus2005 ecc...), only in haul 11 MERL was caught
TB.dev<-TB[231:305,] #rows of TB related to haul 10-12.

# We aim to create a final merged dataset where, for each haul, 
# a row with genus = "MERL" and ptot=0,nbtot=0,nbfem=0,nbmal=0,nbind=0 is added whenever no MERL 
# individuals were caught. In this example (hauls 10–12), MERL was 
# caught only in haul 11, so new rows should be added for hauls 10 
# and 12, while haul 11 remains unchanged.


keys<-c("country","area","vessel","year","month","day","haul_number")
# These are all the key variables that allow us to match observations 
# between the TA and TB datasets. They represent the common identifiers 
# across both files, except for 'name_of_survey', which is not used as a key.

MEDITS<-right_join(x=TA,y=TB,by=keys)




#Check for missing values####
#Compute the share of NAs for each of the 56 variables:
share.NA<-sapply(MEDITS,function(x) mean(is.na(x)))

#Let's create the corresponding tibble,since it's necessary for plotting 
#in ggplot:
share.NA.tib<- tibble(
  variable = names(share.NA),
  share = share.NA)

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



##########################NA values#############################################

#measuring_system_temp,measuring_system_sal and hydrological_station have extremly high
#share of missig values (NAs) so I remove them from MEDITS
MEDITS<- MEDITS %>%
  select(-measuring_system_temp, -measuring_system_sal, -hydrological_station)
#Almost all the other variables have the exact same share of NAs
#A rapid look at the dataset evidenciate that few units have a very high number of NAs
#Let's try to remove all the rows with at least a NA:
n.row<-nrow(MEDITS)
MEDITS<- MEDITS[complete.cases(MEDITS), ]
n.row.no.na<-nrow(MEDITS)
c(n.row,n.row.no.na)  #Indeed only 12918 (from a total of 546344) observations 
#in total are removed from the model            

#Let's check now that now NAs are present in the Medits dataset:
sum(is.na(MEDITS)) #as required
#NB since NA values are stored as -1 as well this aspect has to be further investigated.



#The MEDITS is still incomplete because it doesn't include data about "MERL" when
#no "MERL" were caught, during a certain haul.
#Those rows have to be manually added in the tibble with.
#I will run a for loop that will check if the set of rows with the same values for the 
#"key variables" (used to merge TA and TB) contain a row about "MERL". 
#If this is the case no rows will be added.
#If this is not the case I will add a row exactly equal to the one above but with 
#"ptot"=0
#"nbtot"=0 
#"nbfem"=0
#"nbmal"=0
#"nbind"=0

#I remove some variables that are "useless" here:
#Species
table(filter(MEDITS,genus=="MERL")$species)
#Clearly at least in our period of interest only the species "MER" from the genus "MERL"
#was caught so the species variable can be remove from the datset
MEDITS<-select(MEDITS,-species)



############################MERGE DATASET TA.dev AND TB.dev#############################

start<-Sys.time()
#Let's create a tibble in which rows to add to MEDITS(rows referred to 
#hauls in which 0 hakes were caught) will be stored
rows_to_add<-tibble()

# Get all unique combinations of hauls
unique_hauls<-MEDITS %>%distinct(across(all_of(keys)))

# Start the loop
for(i in 1:nrow(unique_hauls)) {
  
  # Select one haul (a single combination of key values)
  current_haul<-unique_hauls[i, ]
  
  # Subset all rows corresponding to that haul
  haul_data<-MEDITS %>%
    filter(country == current_haul$country,
           area == current_haul$area,
           vessel == current_haul$vessel,
           year == current_haul$year,
           month == current_haul$month,
           day == current_haul$day,
           haul_number == current_haul$haul_number)
  
  # Check if there is already a row for MERL in this haul
  has_merl<-any(haul_data$genus=="MERL") #has_mer is a Bolean variable
  
  # If not, create a new row identical to the first one, but with zeros for counts
  if(!has_merl & nrow(haul_data) > 0) {
    new_row<-haul_data[1, ]
    new_row$genus<-"MERL"
    new_row$ptot<-0
    new_row$nbtot<-0
    new_row$nbfem<-0
    new_row$nbmal<-0
    new_row$nbind<-0
    # Add it to our collection of rows to add
    rows_to_add <- bind_rows(rows_to_add, new_row)
  }
}

# Add the new rows to the MEDITS dataset
MEDITS_MERL<- bind_rows(MEDITS, rows_to_add)
end<-Sys.time()
end-start #More or less 9 mins of computations

#Now 0 counts of "MERL" will be added at the end.
#We can reorder these rows inside the tibble MEDITS so that we still have the usual
#Position-time-haul number order
MEDITS_MERL<-MEDITS_MERL %>% arrange(country, area, vessel, year, month, day, haul_number)
#An inspection of the MEDITS dataset assures that the merging algorithm has worked properly

MEDITS_MERL<-filter(MEDITS_MERL,genus=="MERL") #17594 observations

#Apparently the count distribtion of the count variable(nbtot) in the MEDITS_MERL dataset
#contains far more outliers than the count variable distribution in the hake dataset.

# MEDITS_MERL
medits_freq <- MEDITS_MERL %>%
  count(nbtot) %>%
  mutate(rel_freq = n / sum(n))

p1<-ggplot(medits_freq, aes(x = nbtot, y = rel_freq)) +
  geom_segment(aes(xend = nbtot, y = 0, yend = rel_freq), color = "steelblue") +
  labs(x = "MERL Count", y = "Relative Frequency", 
       title = "Relative Frequency of MERL Counts") +
  theme_minimal()


# Hake
hake_freq <- hake %>%
  count(count) %>%
  mutate(rel_freq = n / sum(n))

p2<-ggplot(hake_freq, aes(x = count, y = rel_freq)) +
  geom_segment(aes(xend = count, y = 0, yend = rel_freq), color = "darkorange") +
  labs(x = "MERL Count", y = "Relative Frequency", 
       title = "Relative Frequency of MERL Counts") +
  theme_minimal()

p1+p2   #This has to be inspected deeper.





#############################VARIABLES INSPECTION###############################
# Let's inspect some of the variables, following the order in which they appear in the tibble
#Let's create the tibble that will be used at the modelling stage
MEDITS_MERL_MOD<-MEDITS_MERL

# Country
table(MEDITS_MERL_MOD$country)  # HRV is Croatia

# Area
table(MEDITS_MERL_MOD$area)
# Those 2 variables will likely not be used at spatial modelling level, however they
# are still of interest to interpret the final results.

# Vessel
table(MEDITS_MERL_MOD$vessel)

# Year
table(MEDITS_MERL_MOD$year)

# Codend_closing
# codend _closing was present in both TA and TB so we have two identical columns in 
# the tibble codend_closing.x and codend_closing.y
# So we remove codend_closing.y and rename codend_closing.x as codend_closing
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-codend_closing.y)
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% rename(codend_closing = codend_closing.x)
table(MEDITS_MERL_MOD$codend_closing) # All the variables have "S" as level
# So this variable can be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-codend_closing)

# Partit
table(MEDITS_MERL_MOD$partit)
# This is the same variable as part_of_the_codend so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-partit)

# Catfau
table(MEDITS_MERL_MOD$catfau)
# This variable is not of interest in our context so remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-catfau)

# Genus
table(MEDITS_MERL_MOD$genus)
#Of couurse genus is not of interest at the modelling stage so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-genus)

# Lref
table(MEDITS_MERL_MOD$lref)
# This variable is not of interest in our context so remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-lref)

# Ptot
hist(MEDITS_MERL_MOD$ptot,breaks=200)
# This variable will not be included in our modelas a covariate, since
# nbtot will be the response variable.
# But it's still of high interest.
# Sanity check:
nrow(MEDITS_MERL_MOD %>%filter(nbtot == 0, ptot > 0))==0

nrow(MEDITS_MERL_MOD %>%filter(nbtot > 0, ptot == 0))==0


# Nbtot
hist(MEDITS_MERL_MOD$nbtot,breaks=200)
#Response variable

# Nbfem
hist(MEDITS_MERL_MOD$nbfem,breaks=200)

# Nbmal
hist(MEDITS_MERL_MOD$nbmal,breaks=200)

MEDITS_MERL_MOD %>%
  filter(nbtot != 0) %>%
  summarise(mean=mean(nbind/nbtot)) #this means that sex was not determined for almost 
#half of the fishes, so nbfem and nbmal will not have any role at any stage of our analysis.
#Might still be used in some way !!!!!!!!!!!!!!!!!!!!!!!!########

# Nbind
hist(MEDITS_MERL_MOD$nbmal,breaks=200)

# Tf
table(MEDITS_MERL_MOD$tf)
#Not of interest, so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-tf)

# Month
table(MEDITS_MERL_MOD$month)

# Day
table(MEDITS_MERL_MOD$day)

# Gear
table(MEDITS_MERL_MOD$gear)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-gear)

# Rigging
table(MEDITS_MERL_MOD$rigging)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-rigging)

# Door
table(MEDITS_MERL_MOD$door)
#It's the same for all the observations so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-door)

#Start of the trawl covariates:
# Shooting_time
hist(MEDITS_MERL_MOD$shooting_time)

# Shooting_quadrant
table(MEDITS_MERL_MOD$shooting_quadrant)
#This variable is not of interest so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-shooting_quadrant)

# Shooting_latitude
hist(MEDITS_MERL_MOD$shooting_latitude)

# Shooting_longitude
hist(MEDITS_MERL_MOD$shooting_longitude)

# Shooting_depth(range 10-800)
n1<-nrow(MEDITS_MERL_MOD)
hist(MEDITS_MERL_MOD$shooting_depth)
c(min(MEDITS_MERL_MOD$shooting_depth),max(MEDITS_MERL_MOD$shooting_depth))
#Some observations have a depth that is outside the range so we remove them:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%filter(shooting_depth >= 10, hauling_depth <= 800)
n2<-nrow(MEDITS_MERL_MOD)
n1-n2 #number of observations removed

#End of the trawl covariates:
# Hauling_time
hist(MEDITS_MERL_MOD$hauling_time)

# Hauling_quadrant
table(MEDITS_MERL_MOD$hauling_quadrant)
#This variable is not of interest so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-hauling_quadrant)
# Hauling_latitude
hist(MEDITS_MERL_MOD$hauling_latitude)

# Hauling_longitude
hist(MEDITS_MERL_MOD$hauling_longitude)

# Hauling_depth (range 10-800)
n1<-nrow(MEDITS_MERL_MOD)
hist(MEDITS_MERL_MOD$hauling_depth)
c(min(MEDITS_MERL_MOD$hauling_depth),max(MEDITS_MERL_MOD$hauling_depth))
#Some observations have a depth that is outside the range so we remove them:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%filter(hauling_depth >= 10, hauling_depth <= 800)
n2<-nrow(MEDITS_MERL_MOD)
n1-n2 #number of observations removed

# Hauling_duration (in minutes) (range 5-90)
hist(MEDITS_MERL_MOD$hauling_duration)
c(min(MEDITS_MERL_MOD$hauling_duration),max(MEDITS_MERL_MOD$hauling_duration)) 
#No observation is outside the range

# Validity
table(MEDITS_MERL_MOD$validity) #4 observations have been judged as invalid 
#and should be removed from the tibble
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[-which(MEDITS_MERL_MOD$validity=="I"),]
#Validity should then be removed since of no interest here:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-validity)


# Course
table(MEDITS_MERL_MOD$course)

# Recorded_species
table(MEDITS_MERL_MOD$recorded_species)
#Not of interest here, so it should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-recorded_species)

# Distance(range 1000-9999)
hist(MEDITS_MERL_MOD$distance)
c(min(MEDITS_MERL_MOD$distance),max(MEDITS_MERL_MOD$distance)) #no observations outside the range

# Vertical_opening (range 10-100)
hist(MEDITS_MERL_MOD$vertical_opening)
c(min(MEDITS_MERL_MOD$vertical_opening),max(MEDITS_MERL_MOD$vertical_opening))  #no observations outside the range

# Wing_opening(range 50-250)
hist(MEDITS_MERL_MOD$wing_opening)
c(min(MEDITS_MERL_MOD$wing_opening),max(MEDITS_MERL_MOD$wing_opening))  #no observations outside the range

# Geometrical_precision
table(MEDITS_MERL_MOD$geometrical_precision) #MEASURMENT ERROR ISSUES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!########
#Ask professor how to handle this regressor!!!!!!!!!!!!!!!!!######
#NB MEDITS_MERL_MOD$nbtot with estimated have double variance than MEDITS_MERL_MOD$nbtot with measured#######


# Bridles_length (range={100,150,200})
table(MEDITS_MERL_MOD$bridles_length)
#A lot of values do not belong to 100 150 or 2000, the best approach is to 
#round those observations to the nearest valid value####
round_bridles <- function(x) {
  valid_values <- c(100, 150, 200)
  valid_values[which.min(abs(x - valid_values))]}
MEDITS_MERL_MOD$bridles_length_rounded <- sapply(MEDITS_MERL_MOD$bridles_length, 
                                                 round_bridles)
table(MEDITS_MERL_MOD$bridles_length_rounded)
table(MEDITS_MERL_MOD$bridles_length_rounded)

# Warp_length (range 100-2200)
hist(MEDITS_MERL_MOD$warp_length)
c(min(MEDITS_MERL_MOD$warp_length),max(MEDITS_MERL_MOD$warp_length))  #no observations outside the range
#One observation has warp length outside the range and so it should be removed:
MEDITS_MERL_MOD<-MEDITS_MERL_MOD[-which.max(MEDITS_MERL_MOD$warp_length),]

# Warp_diameter (range 10-30)
hist(MEDITS_MERL_MOD$warp_diameter)
c(min(MEDITS_MERL_MOD$warp_diameter),max(MEDITS_MERL_MOD$warp_diameter))  #no observations outside the range

# Observations
table(MEDITS_MERL_MOD$observations)
#Some levels have very few observations inside, the best approach is to merge them
#in just 2 categories: no problem (value 0) and problematic(value 1,2,3,4,5,6,9).
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>%
  mutate(observations_flag = ifelse(observations == 0, "no_problem", "problem"))



# Type_of_file
table(MEDITS_MERL_MOD$type_of_file) 
#Not of interest so it should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-type_of_file)

# Bottom_temperature_beginning (range 0-30)
hist(MEDITS_MERL_MOD$bottom_temperature_beginning)
c(min(MEDITS_MERL_MOD$bottom_temperature_beginning),max(MEDITS_MERL_MOD$bottom_temperature_beginning))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_temperature_beginning==-1)/nrow(MEDITS_MERL_MOD)
#Around 30% of the variables have  an invalid temperature measurment (likely -1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to variable should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_temperature_beginning)


# Bottom_temperature_end
hist(MEDITS_MERL_MOD$bottom_temperature_end)
c(min(MEDITS_MERL_MOD$bottom_temperature_end),max(MEDITS_MERL_MOD$bottom_temperature_end))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_temperature_end==-1)/nrow(MEDITS_MERL_MOD)
#Around 30% of the variables have  an invalid temperature measurment (likely -1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to variable should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_temperature_end)

# Number_of_the_stratum
table(MEDITS_MERL_MOD$number_of_the_stratum)
#Not of interest here, so we remove it:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-number_of_the_stratum)

# Part_of_the_codend
table(MEDITS_MERL_MOD$part_of_the_codend)
#Some levels have very few observations inside, one option is to merge them in the other category#######
MEDITS_MERL_MOD$part_of_codend_grouped <- factor(MEDITS_MERL_MOD$part_of_the_codend)
levels(MEDITS_MERL_MOD$part_of_codend_grouped) <- list(S = "S",
                                                       P = "P", 
                                                       other = c("A", "M"))

# Bottom_salinity_beginning
hist(MEDITS_MERL_MOD$bottom_salinity_beginning)
c(min(MEDITS_MERL_MOD$bottom_salinity_beginning),max(MEDITS_MERL_MOD$bottom_salinity_beginning))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_salinity_beginning==-1)/nrow(MEDITS_MERL_MOD)
#Around 97% of the variables have  an invalid temperature measurment (likely -1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to variable should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_salinity_beginning)

# Bottom_salinity_end
hist(MEDITS_MERL_MOD$bottom_salinity_end)
c(min(MEDITS_MERL_MOD$bottom_salinity_end),max(MEDITS_MERL_MOD$bottom_salinity_end))
#some observations are outside the range:
sum(MEDITS_MERL_MOD$bottom_salinity_end==-1)/nrow(MEDITS_MERL_MOD)
#Around 97% of the variables have  an invalid temperature measurment (likely -1 is put to
#indicate the presence of a missing value) 
#The share of missing /invalid data is to variable should be removed:
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-bottom_salinity_end)


#As well name_of_survey.x and name_of_survey.y variables should be removed
MEDITS_MERL_MOD <- MEDITS_MERL_MOD %>% select(-name_of_survey.x,-name_of_survey.y)


#Make sure each variable is stored in the appropriate format

MEDITS_MERL_MOD<-MEDITS_MERL_MOD %>%mutate(#Categorical variables
  country = as.factor(country),
  area = as.factor(area),
  vessel = as.factor(vessel),
  year = as.factor(year),
  month = as.factor(month),
  day = as.factor(day),
  geometrical_precision = as.factor(geometrical_precision),
  observations_flag = as.factor(observations_flag),
  part_of_codend_grouped = as.factor(part_of_codend_grouped),
  # Continuous numeric variables
  shooting_latitude = as.numeric(shooting_latitude),
  shooting_longitude = as.numeric(shooting_longitude),
  shooting_depth = as.numeric(shooting_depth),
  hauling_latitude = as.numeric(hauling_latitude),
  hauling_longitude = as.numeric(hauling_longitude),
  hauling_depth = as.numeric(hauling_depth),
  hauling_duration = as.numeric(hauling_duration),
  distance = as.numeric(distance),
  vertical_opening = as.numeric(vertical_opening),
  wing_opening = as.numeric(wing_opening),
  bridles_length_rounded = as.numeric(bridles_length_rounded),
  warp_length = as.numeric(warp_length),
  warp_diameter = as.numeric(warp_diameter),
  nbtot = as.numeric(nbtot),
  nbfem = as.numeric(nbfem),
  nbmal = as.numeric(nbmal),
  nbind = as.numeric(nbind),
  ptot = as.numeric(ptot))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################EXPLORATORY ANALYSIS FOR LOGISTIC REGRESSION##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
MEDITS_MERL_MOD_LOG<-MEDITS_MERL_MOD


as.numeric(table(MEDITS_MERL_MOD_LOG$presence))[1]/nrow(MEDITS_MERL_MOD_LOG)
#the proportion of zeros is 0.23


#Let's remove variables that are not useful for logistic regression modelling:
MEDITS_MERL_MOD_LOG<-select(MEDITS_MERL_MOD,-nbfem,-nbmal,
                            -nbind,-ptot,-part_of_the_codend,
                            -bridles_length,-haul_number,-observations)

#Let's store the response variable  as a factor:
MEDITS_MERL_MOD_LOG <- MEDITS_MERL_MOD_LOG %>%
  mutate(presence = factor(ifelse(nbtot > 0, "non-zero", "zero")))

MEDITS_MERL_MOD_LOG<-select(MEDITS_MERL_MOD_LOG,-nbtot)

##########################CORRELATION BETWEEN COVARIATES########################



###########Continuos variables
#Let's compute the Pearson correlation between the numerical variables

vars.num<-c("shooting_time", "shooting_latitude", "shooting_longitude",
            "shooting_depth", "hauling_time", "hauling_latitude",
            "hauling_longitude", "hauling_depth", "hauling_duration",
            "distance", "vertical_opening", "wing_opening",
            "warp_length", "warp_diameter")
MEDITS_LOG_NUM<-MEDITS_MERL_MOD_LOG[,vars.num]

#Compute Spearman correlation matrix:
cor_spearman <- cor(MEDITS_LOG_NUM, method = "spearman")
#Let's plot it:
corrplot(
  cor_spearman,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,      
  addCoef.col = "black",
  number.cex = 1,
  cl.pos = "r",
  cl.ratio=0.2,
  mar = c(0,0,1,0), 
  col = colorRampPalette(c("tomato3", "white", "steelblue"))(200))  

#Looking a the correlation matrix we can obsrve that some of the covariates
#have an extremly high correlation.
#In particcular:
# shooting_time - hauling_time             r=1
# shooting_latitude - hauling_latitude     r=1
# shooting_longitude - hauling_longitude   r=1
# shooting_depth - hauling_depth           r=0.99
# shooting_depth-hauling_duration          r=0.77
# shooting_depth-warp_length               r=0.96 
# hauling_depth-hauling_duration           r=0.77
# warp_length - hauling_depth              r=0.96
# hauling_duration-distance                r=0.85
# shooting_time - hauling_time             r=1.00
# warp_length - hauling_duration           r=0.78


#All these multicollinearity issues can be solved by removing the following
#covariates:
# "hauling_time" 
#"hauling_latitude"   
#"hauling_longitude"  
#"hauling_depth"     
# "hauling_duration"
# "warp_length" 

#So let' remove them from our model
MEDITS_MERL_MOD_LOG <- MEDITS_MERL_MOD_LOG %>%
  select(-hauling_time, -hauling_latitude, -hauling_longitude,
         -hauling_depth, -hauling_duration, -warp_length)




# Bivariate relationships
###########################CONTINUOUS VARIABLES##################################


# shooting_time
# - Density: check if deployment time differs between present vs absent hauls (bimodality/shift).
# - Boxplot: compare medians and spread of shooting_time by presence.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = shooting_time, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Shooting Time by Presence", x = "Shooting time", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = shooting_time, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Shooting Time by Presence (boxplot)", x = "Presence", y = "Shooting time") +
  theme(legend.position = "none")
#1) The zero and non zero distributions are extremly similar suggesting that shooting_time 
#may not be signifcant.
#(The time in which the trawl is conducted does not seems to be relevant)


# shooting_latitude
# - Density: inspect latitudinal distribution differences (presence may cluster).
# - Boxplot: median latitude shift indicates spatial segregation in latitudinal axis.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = shooting_latitude, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Shooting Latitude by Presence", x = "Shooting latitude", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = shooting_latitude, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Shooting Latitude by Presence (boxplot)", x = "Presence", y = "Shooting latitude") +
  theme(legend.position = "none")
#2) The distribution of shooting latitude for non-zero hauls is shifted toward higher
#values, whereas zero hauls show more lower latitudes, suggesting that shooting 
#latitude may be an important predictor of presence. A possible non linear relationship
#should be inspected as well.


# shooting_longitude
# - Density: check for longitudinal clustering of presence.
# - Boxplot: compare central tendencies of longitude for present vs absent.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = shooting_longitude, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Shooting Longitude by Presence", x = "Shooting longitude", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = shooting_longitude, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Shooting Longitude by Presence (boxplot)", x = "Presence", y = "Shooting longitude") +
  theme(legend.position = "none")
#3) The zero and non zero distributions are extremly similar suggesting that shooting_longitude
#may not be siginifcant.

# shooting_depth
# - Density: check if depth distributions differ (depth is often a strong predictor).
# - Boxplot: median/quantile differences indicate depth preference.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = shooting_depth, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Shooting Depth by Presence", x = "Shooting depth (m)", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = shooting_depth, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Shooting Depth by Presence (boxplot)", x = "Presence", y = "Shooting depth (m)") +
  theme(legend.position = "none")
#4) Non zero distribution is more shifted to the left, whereas zero distibution is
#shifted to the extremes of the distribution, suggesting that shooting_depth may
#be a significant covariate a possible linear relationship should be inspected as well.


# distance
# - Density: check distance traveled effect on detection/abundance.
# - Boxplot: compare distance distributions by presence.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = distance, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Distance by Presence", x = "Distance (m)", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = distance, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Distance by Presence (boxplot)", x = "Presence", y = "Distance (m)") +
  theme(legend.position = "none")
#5) Again here the distibutions are bimodal,again counterintuitively the non zero distribution is
#more concentrated around the lowest peak, and the zero distribution around the
#highest peak.

# vertical_opening
# - Density: net opening height may affect catch; inspect distribution by presence.
# - Boxplot: compare vertical opening central tendency.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = vertical_opening, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Vertical Opening by Presence", x = "Vertical opening (dm)", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = vertical_opening, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Vertical Opening by Presence (boxplot)", x = "Presence", y = "Vertical opening (dm)") +
  theme(legend.position = "none")
#6)The zero and non zero distributions are extremly similar suggesting that vertical_opening
#may not be siginifcant.


# wing_opening
# - Density: net width effect on catches (check overlap/separation).
# - Boxplot: compare wing opening medians.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = wing_opening, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Wing Opening by Presence", x = "Wing opening (dm)", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = wing_opening, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Wing Opening by Presence (boxplot)", x = "Presence", y = "Wing opening (dm)") +
  theme(legend.position = "none")
#7) The non zero distribution has a peak around 175dm that the zero distribution
#does not have, suggesting the presence of a non linear relationship for the 
#wing_opening covariate.


# warp_diameter
# - Density: see if diameter differences exist between present/absent hauls.
# - Boxplot: compare central tendency.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = warp_diameter, fill = presence)) +
  geom_density(alpha = 0.5) + theme_minimal() +
  labs(title = "Density of Warp Diameter by Presence", x = "Warp diameter (mm)", y = "Density")
ggplot(MEDITS_MERL_MOD_LOG, aes(x = presence, y = warp_diameter, fill = presence)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Warp Diameter by Presence (boxplot)", x = "Presence", y = "Warp diameter (mm)") +
  theme(legend.position = "none")
#8)It's unclear whether warp_diameter should or not be included in the model.






########################### CATEGORICAL VARIABLES ##############################

as.numeric(table(MEDITS_MERL_MOD_LOG$presence))[1]/nrow(MEDITS_MERL_MOD_LOG)
#the proportion of zeros is 0.23


# country
# - Proportion: fraction present/absent within each country (compare detection rates).
ggplot(MEDITS_MERL_MOD_LOG, aes(x = country, fill = presence)) +
  geom_bar(position = "fill") + coord_flip() + theme_minimal() +
  labs(title = "Proportion Present by Country", x = "Country", y = "Proportion")
#9)For  some of the countries the proportion of zeros is much higher/lower than 0.23
#suggesting that country is a relevant covariate

# area
# - Proportion: compare presence rates across areas.
#Note that areas are (with only one exception are country-specific)
table(MEDITS_MERL_MOD_LOG$country,MEDITS_MERL_MOD_LOG$area)

ggplot(MEDITS_MERL_MOD_LOG, aes(x = area, fill = presence)) +
  geom_bar(position = "fill") + coord_flip() + theme_minimal() +
  labs(title = "Proportion Present by Area", x = "Area", y = "Proportion")
#10) For  some of the countries the proportion of zeros is much higher/lower than 0.23
#suggesting that area should be included in the model (but it's not meaningful to
#incude both country and area in the same model)


# vessel
# - Proportion: vessel-specific detection/catch differences.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = vessel, fill = presence)) +
  geom_bar(position = "fill") + coord_flip() + theme_minimal() +
  labs(title = "Proportion Present by Vessel", x = "Vessel", y = "Proportion")
#11) For  some of the countries the proportion of zeros is much higher/lower than 0.23
#suggesting that vessel is a relevant covariate.

# year
# - Barplot: sample sizes per year and presence counts (temporal coverage).
# - Proportion: year-to-year changes in presence rate.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = year, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Year", x = "Year", y = "Proportion")
#12) Proportion of zeros doesn't seems to change much across different years
#suggesting that the covariate year may be non significant.

# month
# - Barplot: seasonal sampling pattern and counts with presence.
# - Proportion: seasonal variation in presence probability.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = month, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Month", x = "Month", y = "Proportion")
#13) For  some of the countries the proportion of zeros is much higher/lower than 0.23
#suggesting that vessel is a relevant covariate.

# day
# - Barplot: check day-of-month sampling pattern and presence counts (useful for QC).
# - Proportion: presence fraction across days (usually noisy; check for patterns).
ggplot(MEDITS_MERL_MOD_LOG, aes(x = day, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Day", x = "Day", y = "Proportion")
#14) Proportion of zeros doesn't seems to change much across different years
#suggesting that the covariate day may be non significant.


# course
# - Proportion: presence probability by course (check if non-rectilinear tows matter).
ggplot(MEDITS_MERL_MOD_LOG, aes(x = course, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Course", x = "Course", y = "Proportion")
#15) The proportion of zeros differs slightly between rectilinear and non rectilinear routes,
#it's unclear whether course should or not be included in the model.


# geometrical_precision
# - Proportion: check whether estimated precision has different presence rates.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = geometrical_precision, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Geometrical Precision", x = "Geom. precision", y = "Proportion")
#16) The proportion of zeros does not differ between geometrical data collected with
#measurment and estimation, suggesting that geometrical_precision should not be
#included in the model.

# observations_flag (collapsed)
# - Proportion: presence rate difference between clean/problematic hauls.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = observations_flag, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Observations Flag", x = "Observations flag", y = "Proportion")
#17) The proportion of zeros differs slightly between observations with 
#no_problem and problem, suggesting that observations_flag may be included in the 
#model.


# part_of_codend_grouped
# - Proportion: compare presence across grouped codend categories.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = part_of_codend_grouped, fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Part of Codend (grouped)", x = "Part of codend (grouped)", y = "Proportion")
#18) The proportion of zeros differs between S,P and other,suggesting that part_of_codend_grouped
#should be included in the model.


# bridles_length_rounded
# - Proportion: presence fraction for each bridles length category.
ggplot(MEDITS_MERL_MOD_LOG, aes(x = factor(bridles_length_rounded), fill = presence)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(title = "Proportion Present by Bridles Length (rounded)", x = "Bridles length (rounded)", y = "Proportion")
#19) The proportion of zeros differs between largely between bridles length 100,150 and 200 ,
#suggesting that bridles_length_rounded should be included in the model.

