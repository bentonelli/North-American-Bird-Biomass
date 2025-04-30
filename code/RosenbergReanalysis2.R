library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(beeswarm)
library(voronoiTreemap)
library(RColorBrewer)

by_spec_biomass <- read_csv("data/output/spec_abd_biomass_change.csv")

#Create Colors
tn_factors <- as.factor(unique(by_spec_biomass$trophic_niche1))
col_tg <- brewer.pal(n = 11, name = 'Set3')[c(1,3:11)]
#as.numeric(as.factor(by_spec_biomass$trophic_niche1))

colors_1970 <- col_tg[as.numeric(as.factor(by_spec_biomass$trophic_niche1))]

#Use omnivore in 1970 as baseline for circle plot size
plt_circle_ref_size <- by_spec_biomass %>% 
  filter(trophic_niche1 == "Omnivore") %>% 
  summarise(sum_biomass = sum(biomass_start)) %>% 
  as.numeric()

yr1970_for_df <- data.frame("h1" = rep("Total",nrow(by_spec_biomass)),
                            "h2"=by_spec_biomass$trophic_niche1,
                            "h3"=by_spec_biomass$species,
                            "color"=colors_1970,
                            "weight" = by_spec_biomass$biomass_start,
                            "codes" = by_spec_biomass$species)

#Highlight biggest "winner" and "loser"
yr1970_for_df$color[which(yr1970_for_df$codes == "Wild Turkey")] <- "black"
yr1970_for_df$color[which(yr1970_for_df$codes == "Herring Gull")] <- "black"

for(each_group in unique(yr1970_for_df$h2)){
  group_df <- yr1970_for_df %>% filter(h2 == each_group)
  json_group <- vt_export_json(vt_input_from_df(group_df))
  vt_1970 <- vt_d3(json_group,label=FALSE,legend = FALSE,seed = 1,
                   color_border= "white",color_circle="black",
                   size_border="3px")
  
  print(vt_1970)
  print(each_group)
  print(sqrt(sum(group_df$weight)/pi)/sqrt(plt_circle_ref_size/pi)) #Scale according to omnivore mass in 1970
}


#Create Colors
tn_factors <- as.factor(unique(by_spec_biomass$trophic_niche1))
col_tg <- brewer.pal(n = 11, name = 'Set3')[c(1,3:11)]
#as.numeric(as.factor(by_spec_biomass$trophic_niche1))

colors_2017 <- col_tg[as.numeric(as.factor(by_spec_biomass$trophic_niche1))]
#colors_1970[502] <- "black"
#Pick out a few species

#spec_names_na[which(yr1970_mass$species %in% spec_highlights)] <- spec_highlights

yr2017_for_df <- data.frame("h1" = rep("Total",nrow(by_spec_biomass)),
                            "h2"=by_spec_biomass$trophic_niche1,
                            "h3"=by_spec_biomass$species,
                            "color"=colors_2017,
                            "weight" = by_spec_biomass$biomass_end,
                            "codes" = by_spec_biomass$species)

#Highlight biggest "winner" and "loser"
yr2017_for_df$color[which(yr2017_for_df$codes == "Wild Turkey")] <- "black"
yr2017_for_df$color[which(yr2017_for_df$codes == "Herring Gull")] <- "black"

for(each_group in unique(yr2017_for_df$h2)){
  group_df <- yr2017_for_df %>% filter(h2 == each_group)
  json_group <- vt_export_json(vt_input_from_df(group_df))
  vt_2017 <- vt_d3(json_group,label=FALSE,legend = FALSE,seed = 1,
                   color_border= "white",color_circle="black",
                   size_border="3px")
  
  print(vt_2017)
  print(each_group)
  print(sqrt(sum(group_df$weight)/pi)/sqrt(plt_circle_ref_size/pi)) #Scale according to omnivore mass in 1970
}

