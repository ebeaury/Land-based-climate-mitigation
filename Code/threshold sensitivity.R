### Testing sensitivity of area estimates to threshold rules
# For select layers where a threshold was used (other than >50% of a type for mutually exclusive layers)

library(terra)
library(dplyr)
library(sf)
library(ggplot2)

theme_set(theme_bw())

#### Set directory to folder containing data subfolders ####
# Additional dataset inputs
# Additional dataset outputs
path = ""
setwd(path)

### Mangrove sensitivity
ml_iucn = rast("Additional dataset outputs/iucn_mosaic_mangroveloss_1996t2000_1km.tif")
quantile(values(ml_iucn, na.rm=T))
thresholds = c(0.00001,0.0001,0.001, 0.01,0.025, 0.05,0.07,0.09,1)
alldf = data.frame()
for(t in thresholds){
  newml = ml_iucn
  newml[newml>t] = 1
  newml[newml<1] = 0
  df = as.data.frame(freq(newml))
  df$threshold = t
  alldf = rbind(df, alldf)
}
alldf %>% filter(value==1) %>%
  ggplot(aes(threshold, count)) +
  geom_ribbon(aes(ymin = 90000, ymax = 130000), alpha=.3) +
  geom_point() + geom_line(alpha=0.3) +
  geom_vline(xintercept = 0.01, col="red") +
  xlab("Threshold") +
  ylab("Number of pixels") +
  ggtitle("Mangrove restoration sensisitivity")
ggsave("Figures/1 march 2024/supporting/mangrove_sensitivity.png",
       plot = last_plot(), width=5, height=4)

### Saltmarsh sensitivity
sl_iucn = rast("Additional dataset outputs/saltmarsh_loss_percent1km.tif")
quantile(values(sl_iucn, na.rm=T))
vals = values(sl_iucn, na.rm=T)
vals = vals[vals>0]
quantile(vals)
thresholds = c(0.0000000001, 0.000001, 0.0001, 0.001,0.01,0.1,0.25, 0.5,0.75,1)
alldf = data.frame()
for(t in thresholds){
  newsm = sl_iucn
  newsm[newsm>t] = 1
  newsm[newsm<1] = 0
  df = as.data.frame(freq(newsm))
  df$threshold = t
  alldf = rbind(df, alldf)
}
alldf %>% filter(value==1) %>%
  ggplot(aes(threshold, count)) +
  geom_ribbon(aes(ymin = 3000, ymax = 32000), alpha=.3) +
  geom_point() + geom_line(alpha=0.3) +
  geom_vline(xintercept = 0.1, col="red") +
  xlab("Threshold") +
  ylab("Number of pixels") +
  ggtitle("Salt marsh restoration sensisitivity")
ggsave("Figures/1 march 2024/supporting/saltmarsh_sensitivity.png",
       plot = last_plot(), width=5, height=4)

### Biochar sensitivity
bio_cont = rast("Additional dataset outputs/bc_100_constrained_iucn.tif")
quantile(values(bio_cont, na.rm=T)) # C sequestration values
thresholds = c(0.1,0.5,0.75,1,2,3,5,15,25,50,100,400)
alldf = data.frame()
for(t in thresholds){
  newbio = bio_cont
  newbio = ifel(newbio>t,1,0)
  df = as.data.frame(freq(newbio))
  df$threshold = t
  alldf = rbind(df, alldf)
}
alldf %>% filter(value==1) %>%
  ggplot(aes(threshold, count)) +
  geom_point() + geom_line(alpha=0.3) +
  geom_vline(xintercept = 1, col="red") +
  xlab("Threshold\n (net carbon sequestration rate in Mg C/year)") +
  ylab("Number of pixels") +
  ggtitle("Biochar sensisitivity")
ggsave("Figures/1 march 2024/supporting/biochar_sensitivity.png",
       plot = last_plot(), width=5, height=4)

### Tree suitability sensitivity
treepot = rast("Additional dataset inputs/BastinTotal_potential.tif")
# drop areas that are already forested
treemap = rast("Additional dataset outputs/iucn_hansentreecover_25sept2023.tif")
treecov_b = ifel(treemap<30,1,0) # threshold selected given literature definition of forest
treepot_iucn = project(treepot, ml_iucn, method="bilinear")
newtree = treepot_iucn*treecov_b
plot(newtree)
quantile(values(newtree, na.rm=TRUE)) # conventional rule is >30% cover = forest
thresholds = c(0.1,1,3,5,10,15,20,40,60,75,100)
alldf = data.frame()
for(t in thresholds){
  newtree2 = newtree
  newtree2 = ifel(newtree2>t,1,0)
  df = as.data.frame(freq(newtree2))
  df$threshold = t
  alldf = rbind(df, alldf)
}
alldf %>% filter(value==1) %>%
  ggplot(aes(threshold, count)) +
  geom_point() + geom_line(alpha=0.3) +
  geom_vline(xintercept = 30, col="red") +
  xlab("Threshold") +
  ylab("Number of pixels") +
  ggtitle("Potential tree cover sensisitivity")
ggsave("Figures/1 march 2024/supporting/treepotential_sensitivity.png",
       plot = last_plot(), width=5, height=4)

### Improved rice sensitivity
rice = rast("Additional dataset outputs/rice_proportionalharvestedarea_25sept2023.tif")
# mask to base layer croplands
crop = rast("Base layer outputs/iucn_cropland_binary_21sept.tif")
rice_sub = crop*rice
quantile(values(rice_sub, na.rm=T))
thresholds = c(0.00001,0.0001,0.001,0.01,0.1,0.25,0.5,0.75,1)
alldf = data.frame()
for(t in thresholds){
  newrice = rice_sub
  newrice = ifel(newrice>t,1,0)
  df = as.data.frame(freq(newrice))
  df$threshold = t
  alldf = rbind(df, alldf)
}
alldf %>% filter(value==1) %>%
  ggplot(aes(threshold, count)) +
  geom_ribbon(aes(ymin = 1500000, ymax = 1800000), alpha=0.3) +
  geom_point() + geom_line(alpha=0.3) +
  geom_vline(xintercept = 0.25, col="red") +
  xlab("Threshold") +
  ylab("Number of pixels") +
  ggtitle("Improved rice sensisitivity")
ggsave("Figures/1 march 2024/supporting/rice_sensitivity.png",
       plot = last_plot(), width=5, height=4)

