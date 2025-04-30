library(dplyr)
library(readr)
library(MCMCvis)
library(ggplot2)
library(jagsUI)

#NOTE: This fit model object needs to be rerun using code from Rosenberg 2019
jagsMod <- readRDS("data/input/original/jagsMod.rds") 
sp_pop_chains <- MCMCchains(jagsMod$samples,params=c("N"))

#Get species data 
#NOTE: "original data" is downloaded from Rosenberg 2019 - see README for more details
sp_order_dt <- read_csv("data/input/original/original data.csv") %>% select(species,spfact,native,Breeding.Biome) %>% unique()
sp_mass_dt <- read_csv("data/input/sp_traits.csv") %>% select("species","mass","trophic_niche1") %>% arrange(species)

#Update species' mass estimates from Dunning 2008 due to potential errors in AVONET database
sp_mass_dt$mass[which(sp_mass_dt$species == "Cackling Goose")] <- 1940.10
sp_mass_dt$mass[which(sp_mass_dt$species == "Canada Goose")] <- 3727.33
sp_mass_dt$mass[which(sp_mass_dt$species == "Brandt's Cormorant")] <- 2247.5
sp_mass_dt$mass[which(sp_mass_dt$species == "Pelagic Cormorant")] <- 1856.5

sp_data_all <- merge(sp_order_dt,sp_mass_dt,by="species")
colnames(sp_data_all)[2] <- "sp_num"

sp_yr_grid_fff <- expand.grid(sp_fff = 1:529,
                              yrs_fff = 1:48)

sp_rep_fff <- rep(sp_mass_dt$species,48)

yr_rep_fff <- c()
for (each_year_fff in 1970:2017){
  yr_rep_fff <- c(yr_rep_fff,rep(each_year_fff,48))
}


### Randomly sample chains to produce CrI ####
rand_samp <- sample(1:nrow(sp_pop_chains),1000,replace=FALSE)

yr_abd_rec_mult <- c()
yr_biomass_rec_mult <- c()

yr_abd_rec_mult_native <- c()
yr_biomass_rec_mult_native  <- c()

yr_abd_rec_mult_introduced <- c()
yr_biomass_rec_mult_introduced  <- c()

sum_by_year_breeding_rec <- c()

sum_by_year_tn_rec <- c()
for (each_chain in rand_samp){
  chain_in <- sp_pop_chains[each_chain,]
  
  chain_w_ind <- cbind(sp_yr_grid_fff,chain_in)
  colnames(chain_w_ind)[3] <- "abd"
  chain_w_ind_mass <- merge(chain_w_ind,sp_data_all,by.x = "sp_fff",by.y="sp_num")
  
  chain_w_ind_mass <- chain_w_ind_mass %>% arrange(species,yrs_fff)
  chain_w_ind_mass$biomass <- chain_w_ind_mass$abd * chain_w_ind_mass$mass
  
  #Biomass by year, all species
  sum_by_year <- chain_w_ind_mass %>% 
    group_by(yrs_fff) %>% 
    summarise(total_abd = sum(abd),
              total_biomass = sum(biomass))
  
  yr_rec_abd <- sum_by_year$total_abd
  yr_rec_biomass <- sum_by_year$total_biomass
  
  yr_abd_rec_mult <- rbind(yr_abd_rec_mult,yr_rec_abd)
  yr_biomass_rec_mult <- rbind(yr_biomass_rec_mult,yr_rec_biomass)
  
  #Biomass by year, introduced and native
  sum_by_year_native <- chain_w_ind_mass %>%
    filter(native == "native") %>%
    group_by(yrs_fff) %>% 
    summarise(total_abd = sum(abd),
              total_biomass = sum(biomass))
  
  yr_abd_rec_mult_native <- rbind(yr_abd_rec_mult_native,sum_by_year_native$total_abd)
  yr_biomass_rec_mult_native <- rbind(yr_biomass_rec_mult_native,sum_by_year_native$total_biomass)
  
  sum_by_year_introduced <- chain_w_ind_mass %>%
    filter(native != "native") %>%
    group_by(yrs_fff) %>% 
    summarise(total_abd = sum(abd),
              total_biomass = sum(biomass))
  
  yr_abd_rec_mult_introduced <- rbind(yr_abd_rec_mult_introduced,sum_by_year_introduced$total_abd)
  yr_biomass_rec_mult_introduced <- rbind(yr_biomass_rec_mult_introduced,sum_by_year_introduced$total_biomass)
  
  #Biomass by breeding biome
  sum_by_year_breeding <- chain_w_ind_mass %>%
    group_by(Breeding.Biome,yrs_fff) %>% 
    summarise(total_abd = sum(abd),
              total_biomass = sum(biomass))
  sum_by_year_breeding$each_chain <- each_chain
  sum_by_year_breeding_rec <- rbind(sum_by_year_breeding_rec,sum_by_year_breeding)
  
  
  #Biomass by trophic niche
  sum_by_year_tn <- chain_w_ind_mass %>%
    group_by(trophic_niche1,yrs_fff) %>% 
    summarise(total_abd = sum(abd),
              total_biomass = sum(biomass))
  sum_by_year_tn$each_chain <- each_chain
  sum_by_year_tn_rec <- rbind(sum_by_year_tn_rec,sum_by_year_tn)
}

# Plot code is below

#Save overall changes in each biome, among each trophic group, between introduced and native species
#Also save 95% CrI

axis_label_size <- 1.6
lwd_plots <- 8

### Seperate plot ####
par(mfrow=c(1,2))

#Abundance plot
yr_abd_change <- 100*((yr_abd_rec_mult[,]/yr_abd_rec_mult[,1]) - 1)
uci_chains_abd <- apply(yr_abd_change,2,quantile,.975)
med_chains_abd <- lci_chains_abd <- apply(yr_abd_change,2,mean)
lci_chains_abd <- apply(yr_abd_change,2,quantile,.025)

plot(NULL,xlim=c(1969,2018),ylim=c(-30,20),ylab="Abundance Change, %",xlab="Year",cex.axis=axis_label_size,cex.lab=axis_label_size)
abline(h=0,lty=2,col="darkgrey",lwd=lwd_plots)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_abd,rev(uci_chains_abd)),col="darkgrey",border=NA)
points(1970:2017,med_chains_abd,type="l",col="firebrick4",lwd=4)

#Biomass plot
yr_biomass_change <- 100*((yr_biomass_rec_mult[,]/yr_biomass_rec_mult[,1]) - 1)
uci_chains_bm <- apply(yr_biomass_change,2,quantile,.975)
med_chains_bm <- apply(yr_biomass_change,2,mean)
lci_chains_bm <- apply(yr_biomass_change,2,quantile,.025)

plot(NULL,xlim=c(1969,2018),ylim=c(-30,20),ylab="Biomass Change, %",xlab="Year",
     cex.axis=axis_label_size,cex.lab=axis_label_size)
abline(h=0,lty=2,col="darkgrey",lwd=lwd_plots)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_bm,rev(uci_chains_bm)),col=alpha("orchid4",.8),border=NA)
points(1970:2017,med_chains_bm,type="l",col="black",lwd=4)

overall_abd_biomass_trends <- data.frame(year=1970:2017,
                                         mean_abd = med_chains_abd,
                                         upper_97p5_abd = uci_chains_abd,
                                         lower_2p5_abd = lci_chains_abd,
                                         mean_bm = med_chains_bm,
                                         upper_97p5_bm = uci_chains_bm,
                                         lower_2p5_bm = lci_chains_bm)

#write_csv(overall_abd_biomass_trends,"data/output/overall_abd_biomass_trends.csv")

### Same plot, overall trends ####
par(mfrow=c(1,1))
par(pty="s")
par(las=1)
par(mar=c(4,4,4,4))
lwd_plots <- 5
plot(NULL,xlim=c(1969,2018),ylim=c(-30,20),ylab="",xlab="",
     cex.axis=axis_label_size,cex.lab=axis_label_size)
abline(h=0,lty=2,col="darkgrey",lwd=lwd_plots)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_bm,rev(uci_chains_bm)),col=alpha("forestgreen",.4),border=NA)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_abd,rev(uci_chains_abd)),col=alpha("orchid4",.4),border=NA)
points(1970:2017,med_chains_abd,type="l",col="orchid4",lwd=lwd_plots)
points(1970:2017,med_chains_bm,type="l",col="forestgreen",lwd=lwd_plots)
legend("topleft",legend=c("Abundance","Biomass"),col=c("orchid4","forestgreen"),lwd=lwd_plots,cex=1.75)

### Introduced versus native biomass trends ####

yr_biomass_change_introduced <- 100*((yr_biomass_rec_mult_introduced[,]/yr_biomass_rec_mult_introduced[,1]) - 1)
uci_chains_bm_introduced <- apply(yr_biomass_change_introduced,2,quantile,.975)
med_chains_bm_introduced <- apply(yr_biomass_change_introduced,2,mean)
lci_chains_bm_introduced <- apply(yr_biomass_change_introduced,2,quantile,.025)

yr_biomass_change_native <- 100*((yr_biomass_rec_mult_native[,]/yr_biomass_rec_mult_native[,1]) - 1)
uci_chains_bm_native <- apply(yr_biomass_change_native,2,quantile,.975)
med_chains_bm_native  <- apply(yr_biomass_change_native,2,mean)
lci_chains_bm_native  <- apply(yr_biomass_change_native,2,quantile,.025)

native_introduced_biomass_trends <- data.frame(year=1970:2017,
                                               mean_bm_introduced = med_chains_bm_introduced,
                                               upper_97p5_introduced = uci_chains_bm_introduced,
                                               lower_2p5_introduced = lci_chains_bm_introduced,
                                               mean_bm_native = med_chains_bm_native,
                                               upper_97p5_native = uci_chains_bm_native,
                                               lower_2p5_native = lci_chains_bm_native)

#write_csv(native_introduced_biomass_trends,"data/output/native_introduced_biomass_trends.csv")
# Biomass only - introduced, native

par(mfrow=c(1,1))
par(pty="s")
par(las=1)
par(mar=c(4,4,4,4))
plot(NULL,xlim=c(1969,2018),ylim=c(-50,20),ylab="Change, %",xlab="Year",cex.axis=axis_label_size,cex.lab=axis_label_size)
abline(h=0,lty=2,col="darkgrey",lwd=lwd_plots)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_bm_introduced,rev(uci_chains_bm_introduced)),col=alpha("grey30",.4),border=NA)
polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_bm_native,rev(uci_chains_bm_native)),col=alpha("tan4",.4),border=NA)
points(1970:2017,med_chains_bm_native,type="l",col="tan4",lwd=lwd_plots)
points(1970:2017,med_chains_bm_introduced,type="l",col="grey30",lwd=lwd_plots)
legend("topleft",legend=c("Native","Introduced"),col=c("tan4","grey30"),lwd=lwd_plots)

### By breeding biome ####

col_pal <- c("grey80","lightgoldenrod1","darkgreen","skyblue2","forestgreen","salmon1",
             "tan","orchid3","black","green3","dodgerblue4")
plot(NULL,xlim=c(1969,2018),ylim=c(-50,150),
     xlab="",
     ylab="",
     cex.axis=axis_label_size,
     cex.lab=axis_label_size)
abline(h=0,col="darkgrey",lwd=lwd_plots)
count <- 0

all_biome_bm <- data.frame(year=1970:2017)
all_biome_abd <- data.frame(year=1970:2017)
for (each_biome in unique(sum_by_year_breeding_rec$Breeding.Biome)){
  count <- count + 1
  chn_data_bb <- filter(sum_by_year_breeding_rec,Breeding.Biome==each_biome)
  biome_abd_dt <- c()
  biome_bm_dt <- c()
  for (each_chn_rep in unique(chn_data_bb$each_chain)){
    chn_data_bb_chain <- filter(chn_data_bb,each_chain==each_chn_rep)
    biome_abd_dt <- rbind(biome_abd_dt,chn_data_bb_chain$total_abd/chn_data_bb_chain$total_abd[1])
    biome_bm_dt <- rbind(biome_bm_dt,chn_data_bb_chain$total_biomass/chn_data_bb_chain$total_biomass[1])
  }
  
  biome_change_abd <- 100*((biome_abd_dt[,]/biome_abd_dt[,1]) - 1)
  uci_chains_abd_biome <- apply(biome_abd_dt,2,quantile,.975)
  med_chains_abd_biome  <- apply(biome_abd_dt,2,mean)
  lci_chains_abd_biome <- apply(biome_abd_dt,2,quantile,.025)
  
  biome_change_bm <- 100*((biome_bm_dt[,]/biome_bm_dt[,1]) - 1)
  uci_chains_bm_biome <- apply(biome_change_bm,2,quantile,.975)
  med_chains_bm_biome  <- apply(biome_change_bm,2,mean)
  lci_chains_bm_biome <- apply(biome_change_bm,2,quantile,.025)
  
  polygon(c(1970:2017,rev(1970:2017)),c(lci_chains_bm_biome,rev(uci_chains_bm_biome)),col=alpha(col_pal[count],.2),border=NA)
  points(1970:2017,med_chains_bm_biome,type="l",col=col_pal[count],lwd=lwd_plots,main=each_biome)
  
  #Add abundance data to dataframe
  df_add_abd_biomes <- data.frame(med_chains_abd_biome=med_chains_abd_biome,
                                  uci_chains_abd_biome=uci_chains_abd_biome,
                                  lci_chains_abd_biome=lci_chains_abd_biome)
  colnames(df_add_abd_biomes) <- c(paste("mean_",gsub(" ","_",each_biome),sep=""),
                                   paste("upper97p5_",gsub(" ","_",each_biome),sep=""),
                                   paste("lower2p5_",gsub(" ","_",each_biome),sep=""))
  all_biome_abd <- cbind(all_biome_abd,df_add_abd_biomes)
  
  #Add biome data to dataframe
  df_add_bm_biomes <- data.frame(med_chains_bm_biome=med_chains_bm_biome,
                                 uci_chains_bm_biome=uci_chains_bm_biome,
                                 lci_chains_bm_biome=lci_chains_bm_biome)
  colnames(df_add_bm_biomes) <- c(paste("mean_",gsub(" ","_",each_biome),sep=""),
                                  paste("upper97p5_",gsub(" ","_",each_biome),sep=""),
                                  paste("lower2p5_",gsub(" ","_",each_biome),sep=""))
  all_biome_bm <- cbind(all_biome_bm,df_add_bm_biomes)
  
}

all_biome_abd <- all_biome_abd - 1

#write_csv(all_biome_abd,"data/output/all_biome_abd.csv")
#write_csv(all_biome_bm,"data/output/all_biome_bm.csv")

legend("topleft",legend=unique(sum_by_year_breeding_rec$Breeding.Biome),col=col_pal,lwd=lwd_plots,cex=1.3)

### Trophic niche ####
all_tn_bm <- data.frame(year=1970:2017)
all_tn_abd <- data.frame(year=1970:2017)
for (each_tn in unique(sum_by_year_tn_rec$trophic_niche1)){
  count <- count + 1
  chn_data_bb <- filter(sum_by_year_tn_rec,trophic_niche1==each_tn)
  tn_abd_dt <- c()
  tn_bm_dt <- c()
  for (each_chn_rep in unique(chn_data_bb$each_chain)){
    chn_data_bb_chain <- filter(chn_data_bb,each_chain==each_chn_rep)
    tn_abd_dt <- rbind(tn_abd_dt,chn_data_bb_chain$total_abd/chn_data_bb_chain$total_abd[1])
    tn_bm_dt <- rbind(tn_bm_dt,chn_data_bb_chain$total_biomass/chn_data_bb_chain$total_biomass[1])
  }
  
  tn_change_abd <- 100*((tn_abd_dt[,]/tn_abd_dt[,1]) - 1)
  uci_chains_abd_tn <- apply(tn_abd_dt,2,quantile,.975)
  med_chains_abd_tn  <- apply(tn_abd_dt,2,mean)
  lci_chains_abd_tn <- apply(tn_abd_dt,2,quantile,.025)
  
  tn_change_bm <- 100*((tn_bm_dt[,]/tn_bm_dt[,1]) - 1)
  uci_chains_bm_tn <- apply(tn_change_bm,2,quantile,.975)
  med_chains_bm_tn  <- apply(tn_change_bm,2,mean)
  lci_chains_bm_tn <- apply(tn_change_bm,2,quantile,.025)
  
  #Add abundance data to dataframe
  df_add_abd_tns <- data.frame(med_chains_abd_tn=med_chains_abd_tn,
                               uci_chains_abd_tn=uci_chains_abd_tn,
                               lci_chains_abd_tn=lci_chains_abd_tn)
  colnames(df_add_abd_tns) <- c(paste("mean_",gsub(" ","_",each_tn),sep=""),
                                paste("upper97p5_",gsub(" ","_",each_tn),sep=""),
                                paste("lower2p5_",gsub(" ","_",each_tn),sep=""))
  all_tn_abd <- cbind(all_tn_abd,df_add_abd_tns)
  
  #Add tn data to dataframe
  df_add_bm_tns <- data.frame(med_chains_bm_tn=med_chains_bm_tn,
                              uci_chains_bm_tn=uci_chains_bm_tn,
                              lci_chains_bm_tn=lci_chains_bm_tn)
  colnames(df_add_bm_tns) <- c(paste("mean_",gsub(" ","_",each_tn),sep=""),
                               paste("upper97p5_",gsub(" ","_",each_tn),sep=""),
                               paste("lower2p5_",gsub(" ","_",each_tn),sep=""))
  all_tn_bm <- cbind(all_tn_bm,df_add_bm_tns)
  
}
all_tn_abd <- all_tn_abd - 1

#write_csv(all_tn_abd,"data/output/all_tn_abd.csv")
#write_csv(all_tn_bm,"data/output/all_tn_bm.csv")