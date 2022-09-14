## --------------- HEADER ------------------------------------------------------
## Script name: 2a_Clean-survey
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-09-14
## Date Last Modified: 2022-09-14
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script cleans the SREL plant survey data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(lubridate)
library(tidylog)
library(styler)

# Clear the decks
rm(list=ls())

# Load in combined pre survey and first post-treatment survey
surv.pre.surv.1 <- read.csv("Plants/Raw-data/4_2021-04-25-Main-survey.csv")

# Load in the second post-treatment survey
surv.2 <- read.csv("Plants/Raw-data/5_2022-04-Main-survey.csv")

# Separate the pre survey from the first post-treatment survey
surv.pre <- surv.pre.surv.1 %>%
	filter(PERIOD == "PRE")
surv.1 <- surv.pre.surv.1 %>%
	filter(PERIOD == "POST")

rm(surv.pre.surv.1) # Remove the combined data

## --------------- CLEAN PRE SURVEY --------------------------------------------

# Add inferred dates to pre data
surv.pre$MONTH <- 4
surv.pre$DAY <- 20

# Drop moss cover and period
surv.pre <- surv.pre %>%
	select(-CLRA, -MOSS, -PERIOD, -LINE, -EXCLUSION, -ADDITION)

# Fill zeroes 
surv.pre <- replace(surv.pre, is.na(surv.pre), 0)

# Add date
surv.pre <- surv.pre %>%
	mutate(DATE = make_date(year = YEAR, month = MONTH, day = DAY)) %>%
	select(MONTH, DAY, YEAR, DATE, everything())

# Get all the species columns on the same playing field
pred <- surv.pre[,1:7]
spec <- surv.pre[,8:115] %>% 
	mutate_if(is.factor,as.character) %>% 
	mutate_if(is.double,as.character)
surv.pre <- cbind(pred,spec)

# Pivot longer
surv.pre <- surv.pre %>%
	pivot_longer(8:115, names_to = "SPECIES", values_to = "STEMS")

## --------------- CLEAN FIRST SURVEY ------------------------------------------

# Drop moss cover and period
surv.1 <- surv.1 %>%
	select(-CLRA, -MOSS, -PERIOD, -LINE, -EXCLUSION, -ADDITION)

# Fill zeroes 
surv.1 <- replace(surv.1, is.na(surv.1), 0)

# Add date
surv.1 <- surv.1 %>%
	mutate(DATE = make_date(year = YEAR, month = MONTH, day = DAY)) %>%
	select(MONTH, DAY, YEAR, DATE, everything())

# Get all the species columns on the same playing field
pred <- surv.1[,1:7]
spec <- surv.1[,8:115] %>% 
	mutate_if(is.factor,as.character) %>% 
	mutate_if(is.double,as.character)
surv.1 <- cbind(pred,spec)

# Pivot longer
surv.1 <- surv.1 %>%
	pivot_longer(8:115, names_to = "SPECIES", values_to = "STEMS")

## --------------- CLEAN SECOND SURVEY -----------------------------------------

# Drop moss cover
surv.2 <- subset(surv.2, !is.na(STEMS))

# Fix some odds and ends
surv.2[69,9] <- 22 # Take the high count
surv.2[7,8] <- "PASP" # Use value from notes

# Convert date
surv.2$DATE <- mdy(surv.2$DATE)

# Drop excess columns
surv.2 <- surv.2 %>% 
	select(-STEMS..HIGH., -COVER, -NOTES) %>%
	select(MONTH, DAY, YEAR, DATE, BLOCK, PLOT, TREATMENT,everything())
	
## --------------- COMBINE DATA AND CLEAN SPECIES  -----------------------------

# Combine data
surv <- rbind(surv.pre, surv.1, surv.2)

# DROP HYGE 
surv <- surv %>%
	filter(SPECIES != "HYGE")

surv$STEMS <- as.numeric(surv$STEMS)

# Get list of species
list <- as.data.frame(unique(surv$SPECIES))
names(list)[1] <- "Species"

# Pivot wider
surv <- surv %>%
	pivot_wider(names_from = SPECIES, values_from = STEMS)

surv <- surv %>%
	mutate(ASTER = ASTER_21 + ASTER2 + ASTER3 + D_ASTER + F_ASTER + L_ASTER + LG.ASTER,
				 AMBROSIA = H_AMPS,
				 AGALINIS = AGALINIS + AGAC,
				 LACTUCA = LACTUCA + LACTUCA2,
				 LESPEDEZA = H_LESPEDEZA + LECU + LEHI + LES + LESPEDEZA,
				 UNK.FORB = BASAL_UNK + FERN_WING + FERNlike + FUZZY + MEAT + MEATY + 
				 		MINT + RED_MIDVEIN + UNK_1 + UNK_2 + WHORL + WHORL2 + WWAC,
				 UNK.GRASS = BIG + BIG.GRASS + BLUE + GRASS2,
				 SEEDLING = COTYLEDON + SEEDLING2,
				 DICHANTH = DIAC + DICH + DICHANTH + DICO,
				 CLMA = CLMA + CLIT,
				 UNK.FAB = FAB1 + FABACEAE + FABVine + H_FAB,
				 CUDWEED = CUD + FAT.CUD,
				 GESE = JESS,
				 MIMOSA = MIM + MIMOSA,
				 PARONCHYIA = PARONCHYIA + PARON,
				 PLANTAGO = PLANTAG + PLVI,
				 QUERCUS = QUERCUS + OAK,
				 RHCO = RHUS + RHCO,
				 RUBUS = BLACKBERRY + DEWBERRY + RUBUS,
				 CYPERACEAE = SEDGE + CYPERACEAE,
				 TORA = TORA + TORA2,
				 WIREWEED = WIRE + WIREWEED,
				 SISYR = SISYR + YELLOW.EYE,
				 OXALIS = OXALIS + YELLOW.CLOVER
) %>%
	select(-ASTER_21, -ASTER2, -ASTER3, -D_ASTER, -F_ASTER, -L_ASTER, -LG.ASTER,
				 -AGAC, -BASAL_UNK, -BIG, -BLUE, -COTYLEDON, -GRASS2, -DIAC, -CLMA,
				 -CLIT, -DIAC, -DICH, -DICO, -FAB1, -FABACEAE, -CUD, -FAT.CUD,
				 -FERN_WING, -FERNlike, -FUZZY, -H_AMPS, -H_FAB, -H_LESPEDEZA, -JESS,
				 -LACTUCA2, -LECU, -LEHI, -LES, -MEAT, -MEATY, -MIM, -MINT, -PARON,
				 -PLANTAG, -PLVI, -OAK, -RED_MIDVEIN, -RHUS, -BLACKBERRY, -DEWBERRY,
				 -SEEDLING2, -SEDGE, -TORA2, -UNK_1, -UNK_2, -WHORL, -WHORL2, -WIRE,
				 -WWAC, -YELLOW.CLOVER
)

