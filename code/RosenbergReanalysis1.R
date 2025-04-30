# Script to merge original Rosenberg model output with mass estimates
library(dplyr)
library(readr)
library(ggplot2)
library(gganimate)

sp_traits <- read_csv("data/input/sp_traits.csv")

#Change cackling, canada geese. Also Pelagic and brandts cormorant based on presumable error in elton/avonet databases
#Below are directly taken from Dunning, 2008
sp_traits$mass[which(sp_traits$species == "Cackling Goose")] <- 1940.10
sp_traits$mass[which(sp_traits$species == "Canada Goose")] <- 3727.33
sp_traits$mass[which(sp_traits$species == "Brandt's Cormorant")] <- 2247.5
sp_traits$mass[which(sp_traits$species == "Pelagic Cormorant")] <- 1856.5

sp_traits2 <- read_csv("data/input/original/original data.csv") %>% select("species","Breeding.Biome") %>% unique()
sp_traits <- merge(sp_traits,sp_traits2)

#
spec_modeled_abd_1970 <- read_csv("data/input/original/species modeled trajectories w projections.csv") %>% 
  select(species, year, mean) %>% 
  filter(year == 1970) %>% 
  select(-year)
colnames(spec_modeled_abd_1970)[2] <- "start_abd"

spec_modeled_abd_2017 <- read_csv("data/input/original/species modeled trajectories w projections.csv") %>% 
  select(species,year, mean) %>% 
  filter(year == 2017) %>% 
  select(-year)
colnames(spec_modeled_abd_2017)[2] <- "end_abd"

spec_change <- merge(spec_modeled_abd_1970,spec_modeled_abd_2017,by=c("species"))
spec_change <- merge(sp_traits,spec_change,by="species")

spec_change$abd_change <- spec_change$end_abd - spec_change$start_abd

spec_change$biomass_start <- spec_change$mass * spec_change$start_abd
spec_change$biomass_end <- spec_change$mass * spec_change$end_abd

spec_change$biomass_change <- spec_change$biomass_end - spec_change$biomass_start

write_csv(spec_change,"data/output/spec_abd_biomass_change.csv")

