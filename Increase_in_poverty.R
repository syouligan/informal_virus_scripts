#!/bin/bash

#! Analysis of number of people moving below the poverty line with economic shutdown
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
library(plotly)
library(processx)
library(maps)
library(viridis)


# Load World bank data
WB_jobs <- read_xlsx("~/cloudstor/scott_projects/informal_virus/data/WorldBank/Data_Extract_From_Jobs.xlsx", sheet = 1)
WB_jobs_metadata <- read_xlsx("~/cloudstor/scott_projects/informal_virus/data/WorldBank/Data_Extract_From_Jobs.xlsx", sheet = 2)
WB_countries <- read_xlsx("~/cloudstor/scott_projects/informal_virus/data/WorldBank/Data_Extract_From_Jobs.xlsx", sheet = 3)

# WB_social_protection_indicators <- read_xlsx("~/cloudstor/scott_projects/informal_virus/data/WorldBank/Data_Extract_From_World_Development_Indicators.xlsx", sheet = 1)
# WB_social_protection_indicators[WB_social_protection_indicators == "NA"] <- NA
# WB_social_protection_indicators$MRV <- apply(WB_social_protection_indicators[, 5:14], 1, function(x) last(x[!is.na(x)], 1))
# WB_social_protection_indicators$MRV[WB_social_protection_indicators$MRV == "character(0)"] <- NA

# Load Informal employment data (ILO)
IW_stats <- read.csv("~/cloudstor/scott_projects/informal_virus/data/ILO/Informal_employment_stats.csv", header = TRUE)

# Add Continent data (https://datahub.io/JohnSnowLabs/country-and-continent-codes-list#resource-country-and-continent-codes-list_zip)
continents <- read.csv("~/cloudstor/scott_projects/informal_virus/data/Country_by_continent.csv", header = TRUE)
idx <- match(IW_stats$Country_Alpha3, continents$Country_Alpha3)
IW_stats$Continent <- continents$Continent_name [idx]
IW_stats$Continent <- factor(IW_stats$Continent, levels = rev(c("Africa", "North America", "South America", "Asia", "Europe", "Oceania")))

# Extract working population stats (WB)
WB_total_population <- data.frame(WB_jobs[WB_jobs$`Series Code` == "SP.POP.TOTL", c(1:4, 13)])
colnames(WB_total_population) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "Total_population")

WB_total_employment <- data.frame(WB_jobs[WB_jobs$`Series Code` == "SL.EMP.TOTL", c(1:4, 13)])
colnames(WB_total_employment) <- c("Country", "Alpha3", "SeriesID", "SeriesDescription", "Total_employment")

# Extract social protection stats (ILO)
ILO_social_protection <- get_ilostat(id = 'SDG_0131_SEX_SOC_RT_A', segment = 'indicator', quiet = TRUE, filters = list(sex = 'T', time=2000:2020, classif1 = "SOC_CONTIG_TOTAL"))
colnames(ILO_social_protection) <- c("Alpha3", "Source", "Indicator", "Sex", "Classification", "Year", "Social_protection_coverage")

# Add to informal work stats
idx <- match(IW_stats$Country_Alpha3, WB_total_population$Alpha3)
IW_stats$Total_population <- WB_total_population$Total_population [idx]
IW_stats$Total_population <- as.numeric(IW_stats$Total_population)

idx <- match(IW_stats$Country_Alpha3, WB_total_employment$Alpha3)
IW_stats$Total_employment <- WB_total_employment$Total_employment [idx]
IW_stats$Total_employment <- as.numeric(IW_stats$Total_employment)

idx <- match(IW_stats$Country_Alpha3, ILO_social_protection$Alpha3)
IW_stats$Social_protection <- ILO_social_protection$Social_protection_coverage [idx]
IW_stats$Social_protection <- as.numeric(IW_stats$Social_protection)
IW_stats$No_social_protection <- 100 - IW_stats$Social_protection

# Calculate the number of informal workers in different sectors
IW_stats$Total_informal <- as.numeric(IW_stats$Total_employment) *as.numeric(IW_stats$Total_wAg_total / 100)

IW_stats$Total_informal_Ag <- as.numeric(IW_stats$Total_informal) * as.numeric(IW_stats$Agriculture / 100)
IW_stats$Total_informal_Se <- as.numeric(IW_stats$Total_informal) * as.numeric(IW_stats$Service / 100)
IW_stats$Total_informal_Ind <- as.numeric(IW_stats$Total_informal) * as.numeric(IW_stats$Industry / 100)

# Calculate the number of informal workers excluding agriculture
IW_stats$Total_informal_NoAg <- as.numeric(IW_stats$Total_informal_Se) + as.numeric(IW_stats$Total_informal_Ind)
IW_stats$Percent_informal_NoAg <- as.numeric(IW_stats$Total_informal_NoAg) / as.numeric(IW_stats$Total_employment) * 100

# Plot Number of informal workers as a function of social protection - income
IW_stats$Income <- factor(IW_stats$Income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
IW_stats$Total_employment <- as.numeric(IW_stats$Total_employment)

ggplot(IW_stats, aes(x = No_social_protection, y = Percent_informal_NoAg, color = Income)) +
  geom_point(aes(size = No_social_protection), alpha=0.5) +
  geom_label_repel(aes(label = Country_Alpha3),
                   color = 'grey50',
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   seed = 100) +
  scale_colour_viridis_d(option = "A") +
  scale_size(range = c(.3, 10), name="No protection") +
  theme_minimal() +
  ggsave("Informal_workers_by_social_protection_income.pdf")

# Plot Number of informal workers as a function of social protection - region
ggplot(IW_stats, aes(x = No_social_protection, y = Percent_informal_NoAg, color = Continent)) +
  geom_point(aes(size = No_social_protection), alpha=0.5) +
  geom_label_repel(aes(label = Country_Alpha3),
                   color = 'grey50',
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   seed = 100) +
  scale_colour_viridis_d(option = "B") +
  scale_size(range = c(.3, 10), name="No protection") +
  theme_minimal() +
  ggsave("Informal_workers_by_social_protection_region.pdf")


# Calculate the number of informal workers with no social protection - income
IW_stats$Informal_no_protection <- IW_stats$Total_informal_NoAg * IW_stats$No_social_protection / 100

IW_stats[!is.na(IW_stats$Informal_no_protection), ] %>%
  mutate(Country_Alpha3 = fct_reorder(Country_Alpha3, Informal_no_protection)) %>%
  ggplot(aes(x = Country_Alpha3, y = Informal_no_protection, fill = Income)) +
  geom_bar(stat = "identity") +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis_d(option = "A") +
  coord_flip() +
  theme_classic() +
  ggsave("Number_of_people_threatened_by_poverty_income.pdf")

# Calculate the number of informal workers with no social protection - region
IW_stats$Informal_no_protection <- IW_stats$Total_informal_NoAg * IW_stats$No_social_protection / 100

IW_stats[!is.na(IW_stats$Informal_no_protection), ] %>%
  mutate(Country_Alpha3 = fct_reorder(Country_Alpha3, Informal_no_protection)) %>%
  ggplot(aes(x = Country_Alpha3, y = Informal_no_protection, fill = Continent)) +
  geom_bar(stat = "identity") +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Number_of_people_threatened_by_poverty_region.pdf")


# Percentage of total population threatened by poverty - income
IW_stats$Informal_no_protection_percent <- IW_stats$Informal_no_protection / IW_stats$Total_population *100

IW_stats[!is.na(IW_stats$Informal_no_protection_percent), ] %>%
  mutate(Country_Alpha3 = fct_reorder(Country_Alpha3, Informal_no_protection_percent)) %>%
  ggplot(aes(x = Country_Alpha3, y = Informal_no_protection_percent, fill = Income)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "A") +
  coord_flip() +
  theme_classic() +
  ggsave("Percentage_population_threatened_by_poverty_income.pdf")

IW_stats %>%
  group_by(Income) %>%
  summarise(Number = sum(Informal_no_protection[! is.na(Informal_no_protection)])) %>%
  # mutate(Income = fct_reorder(Income, Number)) %>%
  ggplot(aes(x = Income, y = Number, fill = Income)) +
  geom_bar(stat = "identity") +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Number_of_people_facing_poverty_income.pdf")

IW_stats %>%
  group_by(Income) %>%
  summarise(Number = sum(Informal_no_protection[! is.na(Informal_no_protection)]) / sum(Total_population[! is.na(Total_population)]) * 100) %>%
  # mutate(Income = fct_reorder(Income, Number)) %>%
  ggplot(aes(x = Income, y = Number, fill = Income)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Percent_of_people_facing_poverty_income.pdf")

# Percentage of total population threatened by poverty - region
IW_stats$Informal_no_protection_percent <- IW_stats$Informal_no_protection / IW_stats$Total_population *100

IW_stats[!is.na(IW_stats$Informal_no_protection_percent), ] %>%
  mutate(Country_Alpha3 = fct_reorder(Country_Alpha3, Informal_no_protection_percent)) %>%
  ggplot(aes(x = Country_Alpha3, y = Informal_no_protection_percent, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Percentage_population_threatened_by_poverty_region.pdf")

IW_stats %>%
  group_by(Continent) %>%
  summarise(Number = sum(Informal_no_protection[! is.na(Informal_no_protection)])) %>%
  # mutate(Continent = fct_reorder(Continent, Number)) %>%
  ggplot(aes(x = Continent, y = Number, fill = Continent)) +
  geom_bar(stat = "identity") +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Number_of_people_facing_poverty_continent.pdf")

IW_stats %>%
  group_by(Continent) %>%
  summarise(Number = sum(Informal_no_protection[! is.na(Informal_no_protection)]) / sum(Total_population[! is.na(Total_population)]) * 100) %>%
  # mutate(Continent = fct_reorder(Continent, Number)) %>%
  ggplot(aes(x = Continent, y = Number, fill = Continent)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "B") +
  coord_flip() +
  theme_classic() +
  ggsave("Percent_of_people_facing_poverty_continent.pdf")

pdf("World_poverty_increase.pdf")
IW_stats %>%
  group_by(Country_Alpha3) %>%
  summarise(Number = sum(Informal_no_protection[! is.na(Informal_no_protection)]) / sum(Total_population[! is.na(Total_population)]) * 100) %>%
  left_join(Rilostat:::ilostat_ref_area_mapping %>% 
                select(ref_area, ref_area_plotly) %>% 
                label_ilostat(code = 'ref_area'),
            by = c("Country_Alpha3" = "ref_area")) %>% 
  filter(!Number %in% NA) %>% 
  plot_geo(z = ~Number, text = ~ref_area.label, locations = ~ref_area_plotly) %>% 
  add_trace(colors = magma(5), marker = list(line = list(color = toRGB("grey"), width = 0.5)), showscale = TRUE) %>%
  colorbar(title = '%', len = 0.5) %>%
  layout(title = 'Increase in poverty (% of population)', geo = list(showframe = FALSE, showcoastlines = TRUE, projection = list(type = 'natural earth'), showcountries = TRUE, resolution = 300))
dev.off()

write.csv(IW_stats, "Informal_employment_stats_poverty_increase.csv", row.names = FALSE)
