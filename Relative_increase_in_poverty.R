#!/bin/bash

#! Analysis of the increase in the % of people in poverty due to economic shutdown
#------------------------------------------------------------------------

setwd('~/cloudstor/scott_projects/informal_virus/project_results/poverty_increase/')

library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(readxl)
library(data.table)
library(Rilostat)
library(ggrepel)

# Load poverty increase data
IW_stats <- read.csv("~/cloudstor/scott_projects/informal_virus/project_results/poverty_increase/Informal_employment_stats_poverty_increase.csv", header = TRUE)
IW_stats$Continent <- factor(IW_stats$Continent, levels = rev(c("Africa", "Australia", "North America", "South America", "Asia", "Europe", "Oceania")))
IW_stats$Income <- factor(IW_stats$Income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))

# Load current poverty data (% of working population) ILO
# ILO_near_poor <- get_ilostat(id = 'POV_P3T5_NOC_RT_A', segment = 'indicator', quiet = TRUE, filters = list(time=2000:2020))
# colnames(ILO_near_poor) <- c("Alpha3", "Source", "Indicator", "Year", "ILO_near_poor_percent", "Source")
# 
# ILO_poor <- get_ilostat(id = 'POV_P2T3_NOC_RT_A', segment = 'indicator', quiet = TRUE, filters = list(time=2017))
# colnames(ILO_poor) <- c("Alpha3", "Source", "Indicator", "Year", "ILO_poor_percent", "Source")
# 
# ILO_extremely_poor <- get_ilostat(id = 'POV_PLT1_NOC_RT_A', segment = 'indicator', quiet = TRUE, filters = list(time=2017))
# colnames(ILO_extremely_poor) <- c("Alpha3", "Source", "Indicator", "Year", "ILO_extremely_poor_percent", "Source")
# 
# ILO_poverty <- get_ilostat(id = 'SDG_0111_SEX_AGE_RT_A', segment = 'indicator', quiet = TRUE, filters = list(time=2017, sex = "T", classif1 = "AGE_YTHADULT_YGE15"))
# colnames(ILO_poverty) <- c("Alpha3", "Source", "Indicator", "Sex", "Classif", "Year", "ILO_poverty_percent")

# Load current poverty data from world bank (% of population)
WB_poverty_indicators <- read_xlsx("~/cloudstor/scott_projects/informal_virus/data/WorldBank/Data_Extract_From_World_Development_Indicators.xlsx", sheet = 1)
WB_poverty_indicators[WB_poverty_indicators == "NA"] <- NA
WB_poverty_indicators$MRV <- apply(WB_poverty_indicators[, 5:14], 1, function(x) last(x[!is.na(x)], 1))
WB_poverty_indicators$MRV[WB_poverty_indicators$MRV == "character(0)"] <- NA

# Load poverty data from alternative sources
EU_poverty <- read.csv("~/cloudstor/scott_projects/informal_virus/data/eurostat_poverty/Poverty_EU_percentage.csv", header = TRUE)
continents <- read.csv("~/cloudstor/scott_projects/informal_virus/data/Country_by_continent.csv", header = TRUE)

EU_poverty[EU_poverty == ":"] <- NA
EU_poverty$MRV <- apply(EU_poverty[, 2:11], 1, function(x) last(x[!is.na(x)], 1))

idx <- match(EU_poverty$GEO.TIME, continents$Country_Alpha2)
EU_poverty$Country_Alpha3 <- continents$Country_Alpha3 [ idx ]

OECD_poverty <- read.csv("~/cloudstor/scott_projects/informal_virus/data/oecd_poverty/DP_LIVE_27032020224819334.csv", header = TRUE)
OECD_poverty <- OECD_poverty[OECD_poverty$SUBJECT == "TOT",]
OECD_poverty$LOCATION <- as.character(OECD_poverty$LOCATION)
OECD_poverty$TIME <- as.numeric(OECD_poverty$TIME)

OECD_MRV_values <- data.frame(OECD_poverty %>%
  group_by(LOCATION) %>%
  summarise(TIME = max(TIME)))
OECD_MRV_values$LOCATION <- as.character(OECD_MRV_values$LOCATION)
OECD_MRV_values$TIME <- as.numeric(OECD_MRV_values$TIME)

rm(OECD_MRV)
for(i in OECD_MRV_values$LOCATION) {
  if (exists("OECD_MRV")) {
    OECD_MRV <- rbind(OECD_MRV, OECD_poverty[OECD_poverty$LOCATION == i & OECD_poverty$TIME == OECD_MRV_values[OECD_MRV_values$LOCATION == i , "TIME"], ])
  } else {
  OECD_MRV <- OECD_poverty[OECD_poverty$LOCATION == i & OECD_poverty$TIME == OECD_MRV_values[OECD_MRV_values$LOCATION == i , "TIME"], ]
}
}

# WB_near_poor <- data.frame(WB_poverty_indicators[WB_poverty_indicators$`Series Code` == "SI.POV.UMIC", c(1:4, 15)])
# colnames(WB_near_poor) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "WB_near_poor_percent")
# 
# WB_poor <- data.frame(WB_poverty_indicators[WB_poverty_indicators$`Series Code` == "SI.POV.LMIC", c(1:4, 15)])
# colnames(WB_poor) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "WB_poor_percent")
# 
# WB_extreme_poor <- data.frame(WB_poverty_indicators[WB_poverty_indicators$`Series Code` == "SI.POV.DDAY", c(1:4, 15)])
# colnames(WB_extreme_poor) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "WB_extreme_poor_percent")

WB_relative_poor <- WB_poverty_indicators[WB_poverty_indicators$`Series Code` == "SI.POV.NAHC", c(1:4, 15)]
WB_relative_poor <- WB_relative_poor[1:264,] # Removes last 2 rows (null)
WB_relative_poor$MRV <- as.numeric(unlist(WB_relative_poor$MRV))
colnames(WB_relative_poor) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "WB_relative_poor_percent")
write.csv(WB_relative_poor, "WB_estimates_of_relative_poverty.csv", row.names = FALSE)

# Compile current poverty estimates
# idx <- match(IW_stats$Country_Alpha3, ILO_near_poor$Alpha3)
# IW_stats$ILO_near_poor_percent <- ILO_near_poor$ILO_near_poor_percent [idx]
# 
# idx <- match(IW_stats$Country_Alpha3, ILO_poor$Alpha3)
# IW_stats$ILO_poor_percent <- ILO_poor$ILO_poor_percent [idx]
# 
# idx <- match(IW_stats$Country_Alpha3, ILO_extremely_poor$Alpha3)
# IW_stats$ILO_extremely_poor_percent <- ILO_extremely_poor$ILO_extremely_poor_percent [idx]
# 
# idx <- match(IW_stats$Country_Alpha3, ILO_poverty$Alpha3)
# IW_stats$ILO_poverty_percent <- ILO_poverty$ILO_poverty_percent [idx]
# 
# idx <- match(IW_stats$Country_Alpha3, WB_near_poor$Alpha3)
# IW_stats$WB_near_poor_percent <- WB_near_poor$WB_near_poor_percent [idx]
# IW_stats$WB_near_poor_percent <- as.numeric(unlist(IW_stats$WB_near_poor_percent))
# 
# idx <- match(IW_stats$Country_Alpha3, WB_poor$Alpha3)
# IW_stats$WB_poor_percent <- WB_poor$WB_poor_percent [idx]
# IW_stats$WB_poor_percent <- as.numeric(unlist(IW_stats$WB_poor_percent))
# 
# idx <- match(IW_stats$Country_Alpha3, WB_extreme_poor$Alpha3)
# IW_stats$WB_extreme_poor_percent <- WB_extreme_poor$WB_extreme_poor_percent [idx]
# IW_stats$WB_extreme_poor_percent <- as.numeric(unlist(IW_stats$WB_extreme_poor_percent))

idx <- match(IW_stats$Country_Alpha3, WB_relative_poor$Alpha3)
IW_stats$WB_relative_poor_percent <- WB_relative_poor$WB_relative_poor_percent [idx]
IW_stats$WB_relative_poor_percent <- as.numeric(unlist(IW_stats$WB_relative_poor_percent))

idx <- match(IW_stats$Country_Alpha3, OECD_MRV$LOCATION)
IW_stats$OECD_relative_poor_percent <- OECD_MRV$Value [idx]
IW_stats$OECD_relative_poor_percent <- IW_stats$OECD_relative_poor_percent*100

idx <- match(IW_stats$Country_Alpha3, EU_poverty$Country_Alpha3)
IW_stats$EU_relative_poor_percent <- EU_poverty$MRV [idx]
IW_stats$EU_relative_poor_percent <- as.numeric(IW_stats$EU_relative_poor_percent)
IW_stats$All_source_relative_poor_percent <- apply(IW_stats[, c("WB_relative_poor_percent", "EU_relative_poor_percent")], 1, function(x) first(x[!is.na(x)]))
IW_stats$All_source_relative_poor_percent[IW_stats$All_source_relative_poor_percent == "numeric(0)"] <- NA
IW_stats$All_source_relative_poor_percent[IW_stats$Country_Alpha3 == "USA"] <- "11.8"
IW_stats$All_source_relative_poor_percent <- as.numeric(unlist(IW_stats$All_source_relative_poor_percent))

# Calculate potential increase in poverty increase
IW_stats_poverty_totals <- IW_stats %>%
  select(Country, Country_Alpha3, Continent, Income, Total_population, Total_employment, Informal_no_protection, WB_relative_poor_percent, OECD_relative_poor_percent, EU_relative_poor_percent, All_source_relative_poor_percent) %>%
  mutate(All_source_relative_poor_total = All_source_relative_poor_percent*Total_population/100,
         )

IW_stats_poverty_totals <- IW_stats_poverty_totals %>%
  mutate(All_source_relative_poor_increase = Informal_no_protection/All_source_relative_poor_total*100,
  )

write.csv(IW_stats_poverty_totals, "Informal_employment_stats_poverty_increase_relative.csv", row.names = FALSE)
