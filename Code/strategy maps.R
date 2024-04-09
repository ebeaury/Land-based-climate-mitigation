## Deriving maps for each climate mitigation strategy
# Updates March 25
# Starting with base layers & harmonizing with additional datasets

library(terra)
library(dplyr)
library(sf)

#### Set directory ####
#path = "/Users/labadmin/Documents/Eve/NEW PIPELINE SEPT 21/"
path = ""
setwd(path)

#### Mask layers ####
# protected areas
pa = vect("Additional dataset inputs/WDPA/WDPA_Subset_I-IV.shp")
world = vect("Additional dataset inputs/World_Countries_(Generalized) (1)/World_Countries__Generalized_.shp")

#### Forest solutions ####
# Avoiding conversion
# Natural forest management
# Plantations

# Load base layers
forest_base = rast("Base layer outputs/iucn_forest_binary_21sept.tif")
plantations_base = rast("Base layer outputs/iucn_plantation_binary_21sept.tif")
# Load forest management layers
nfm = rast("Base layer inputs/iucn_lesiv2022_naturalforestmanagement_cont.tif")
improvplant = rast("Base layer inputs/iucn_lesiv2022_plantations_cont.tif")
# Select pixels with >50% cover
nfm_bin = ifel(nfm>50,1,0)
improvplant_bin = ifel(improvplant >50,1,0)
# Assign base layer forests as the two management types
nfm_base = nfm_bin*forest_base
#plot(nfm_base) # forests with some signs of management
#freq(nfm_base)
imrpv_base =improvplant_bin*forest_base
#plot(imrpv_base)
#freq(imrpv_base)
# Combine lesiv plantations with base
allplant = max(plantations_base, imrpv_base, na.rm=T)
#freq(allplant)
# Remove pixels with majority oil palm
oil = rast("Base layer inputs/iucn_lesiv2022_oilpalm_cont.tif")
oil = ifel(oil>50,1,0)
plant2 = allplant - oil
# fix mismatch with Jung forests
plant2[plant2<0] = 0

# Avoid forest conversion -- restrict base to temperate/subtropical/tropical zones
zone = vect("Additional dataset inputs/Ecoregions2017/temperate_tropical_biomes.shp")
avoid = mask(forest_base, zone)
#plot(avoid)
# remove base layer managed forests
avoid2 = avoid - imrpv_base - nfm_base
#plot(avoid2)

# remove protected areas from avoiding forest conversion
avoid3 = mask(avoid2, pa, inverse=TRUE)

# Export forest layers
writeRaster(nfm_base, "Solution outputs/naturalforestmanagement_25mar2024.tif", overwrite=TRUE)
writeRaster(plant2, "Solution outputs/improvedplantations_25mar2024.tif", overwrite=TRUE)
writeRaster(avoid3, "Solution outputs/avoidforestconversion_25mar2024.tif", overwrite=TRUE)


#### Wetlands and peatlands
## Mangrove loss (restoration)
# Aiming for 9-13 Mha (literature expectation)
ml_iucn = rast("Additional dataset outputs/iucn_mosaic_mangroveloss_1996t2000_1km.tif")
al_bin = ifel(ml_iucn>0.01,1,0) # threshold selected given sensitivity code
#freq(al_bin) # slightly over 9Mha (towards conservative end)
## Salt marsh loss (restoration)
# Aiming for 0.2-3.2 Mha (literature expectation)
sl_iucn = rast("Additional dataset outputs/saltmarsh_loss_percent1km.tif")
salt_bin = ifel(sl_iucn>0.1,1,0) # threshold selected given sensitivity code
#freq(salt_bin) # 0.95 Mha (towards conservative end)
# check for overlap with mangroves
overlap = sum(al_bin, salt_bin, na.rm=T)
#freq(overlap) # minor
# take max across both layers
both = max(salt_bin, al_bin, na.rm=T)
## Peatland loss (restoration)
pl_iucn = rast("Base layer inputs/iucn_leifeld2018_PeatlandRestoration_global.tif")
overlap = sum(pl_iucn,both, na.rm=T)
#freq(overlap) # some pixels to remove from coastal wetland (if they overlap, call it peatland)
# fix missing vals first
pl_iucn[is.na(pl_iucn)] = 0
both2 = ifel(both==pl_iucn,0,both)
#freq(both2) # ~10 Mha
# remove forest from restoration potential
forest_base = rast("Base layer outputs/iucn_forest_binary_21sept.tif")
nfm = rast("Solution outputs/naturalforestmanagement_25mar2024.tif")
plant = rast("Solution outputs/improvedplantations_25mar2024.tif")
currentfor = max(forest_base, nfm, plant, na.rm=T)
# fill in NAs
currentfor[is.na(currentfor)] = 0
pl_iucn2 = ifel(pl_iucn==currentfor,0,pl_iucn)
both3 = ifel(both2 == currentfor,0,both2)
# remove built environment
remove = rast("Base layer outputs/excludenotrocky_landcovers_26mar2024.tif")
both4 = ifel(both3==remove,0,both3) # no effect
pl_iucn3 = ifel(pl_iucn2==remove,0,pl_iucn2)
# Export restoration files
writeRaster(both4, "Solution outputs/coastalwetlandrestoration_25mar2024.tif", overwrite=TRUE)
writeRaster(pl_iucn3, "Solution outputs/peatlandrestoration_25mar2024.tif", overwrite=TRUE)

# Remove restoration areas from avoidance
both3 = rast("Solution outputs/coastalwetlandrestoration_25mar2024.tif")
pl_iucn2 = rast("Solution outputs/peatlandrestoration_25mar2024.tif")
wetland_base = rast("Base layer outputs/iucn_wetland_binary_21sept.tif")
peatland_base = rast("Base layer outputs/iucn_peatland_binary_21sept.tif")
# fill in NAs
wetland_base[is.na(wetland_base)] = 0
# remove
avoidwet = ifel(wetland_base == both3 | wetland_base==pl_iucn2,0,wetland_base)
avoidwet = avoidwet-pl_iucn2
#freq(avoidwet)
avoidwet[avoidwet==-1] = 0
overlap = avoidwet*pl_iucn2
#freq(overlap)
#plot(avoidwet)
#freq(avoidwet)
avoidpeat = ifel(peatland_base == both2 | peatland_base==pl_iucn2,0,peatland_base)
# pl_iucn and peatland_base should already be mutually exclusive
#plot(avoidpeat)
#freq(avoidpeat)

# remove protected areas from avoid
avoidwet2 = mask(avoidwet, pa, inverse=TRUE)
avoidpeat2 = mask(avoidpeat, pa, inverse=TRUE)

# Export avoid files
writeRaster(avoidwet2, "Solution outputs/avoidwetlandconversion_25mar2024.tif", overwrite=TRUE)
writeRaster(avoidpeat2, "Solution outputs/avoidpeatlandconversion_25mar2024.tif", overwrite=TRUE)

#### Grassland solutions ####
# Avoiding conversion in grassland biomes of temperate, tropical, subtropical zones
# Load base layer
grass_base = rast("Base layer outputs/iucn_grassland_binary_21sept.tif")
#plot(grass_base)
# restrict to temperate/subtropical/tropical zones
zone = st_read("Additional dataset inputs/Ecoregions2017/temperate_tropical_biomes.shp")
avoid = mask(grass_base, zone)
#plot(avoid)
#freq(avoid)
# restrict to grassland biomes
o_biome = rast("Additional dataset inputs/Ecoregions2017/iucn_openbiome.tif")
avoid2 = o_biome*avoid
#plot(avoid2)
# remove protected areas
avoid3 = mask(avoid2, pa, inverse=TRUE)
#plot(avoid3)

# remove areas that could be restored
cw = rast("Solution outputs/coastalwetlandrestoration_25mar2024.tif")
pr = rast("Solution outputs/peatlandrestoration_25mar2024.tif")
avoid4 = ifel(avoid3==cw,0,avoid3)
avoid5 = ifel(avoid4==pr,0,avoid4)
# check overlap
overlap = sum(avoid5,cw, na.rm=T)
freq(overlap)
overlap = sum(avoid5,pr,na.rm=T)
freq(overlap)
writeRaster(avoid5, "Solution outputs/avoidgrasslandconversion_25mar2024.tif", overwrite=TRUE)


#### Cropland solutions #####
# Biochar, better nutrients, rice, integrate trees
# load base map
crop = rast("Base layer outputs/iucn_cropland_binary_21sept.tif")
# better nutrients apply to entire area
#plot(crop2)
writeRaster(crop, "Solution outputs/croplandnutrient_25mar2024.tif", overwrite=TRUE)

# Biochar
bio_cont = rast("Additional dataset outputs/bc_100_constrained_iucn.tif")
# discretize with anywhere >0 C sequestration rate / ha / yr
bio_bin = bio_cont
bio_bin = ifel(bio_bin>1,1,0) 
biochar = bio_bin*crop
#freq(biochar)
#freq(crop)
writeRaster(biochar, "Solution outputs/biochar_25mar2024.tif", overwrite=TRUE)

# Improved rice in a subset of croplands
# load proportional rice data
rice = rast("Additional dataset outputs/rice_proportionalharvestedarea_25sept2023.tif")
# restrict to existing croplands
rice_sub = crop*rice
#plot(rice_sub)
# expect rice to equal ~165 Mha
rice_bin = ifel(rice_sub>0.25,1,0)
#freq(rice_bin) # ~163 Mha
writeRaster(rice_bin, "Solution outputs/improvedrice_25mar2024.tif", overwrite=TRUE)

# Trees in croplands
# load potential tree cover data
treepot = rast("Additional dataset outputs/iucn_bastin_potentialforestcover_25sept2023.tif")
# load continuous tree cover maps
treemap = rast("Additional dataset outputs/iucn_hansentreecover_25sept2023.tif")
treecov_b = ifel(treemap<30,1,0) # threshold selected given literature definition of forest
# constrain to areas with low tree cover, but high predicted potential in croplands
intg = treepot*crop*treecov_b
#plot(intg)
#freq(intg)
# remove rice -- flooded so unlikely suitable for trees
intg2 = ifel(intg==rice_bin, 0,intg)
# export
writeRaster(intg2, "Solution outputs/integratetreescroplands_25mar2024.tif", overwrite=TRUE)

###### Pasture solutions #####
# legumes
past = rast("Base layer outputs/iucn_pasture_binary_21sept.tif")
writeRaster(past,  "Solution outputs/legumesinpasture_25mar2024.tif", overwrite=TRUE)
# silvopasture
treepot = rast("Additional dataset outputs/iucn_bastin_potentialforestcover_25sept2023.tif")
treemap = rast("Additional dataset outputs/iucn_hansentreecover_25sept2023.tif")
treecov_b = ifel(treemap<30,1,0) # threshold selected given literature definition of forest
silvo = past*treepot*treecov_b
writeRaster(silvo,  "Solution outputs/silvopasture_25mar2024.tif", overwrite=TRUE)

##### Enhanced chemical weathering ######
ew = rast("Additional dataset outputs/iucn_enhancedweathering_binary_25sept2023.tif")
# restrict to managed landscapes
past = rast("Base layer outputs/iucn_pasture_binary_21sept.tif")
crop = rast("Base layer outputs/iucn_cropland_binary_21sept.tif")
plant = rast("Solution outputs/improvedplantations_25mar2024.tif")
managed = max(past, crop, plant, na.rm=T)
ew_r = ew*managed
writeRaster(ew_r,"Solution outputs/enhancedweathering_25mar2024.tif", overwrite=TRUE)

##### Grassland restoration #####
# croplands and pastures in grassland biomes
past = rast("Base layer outputs/iucn_pasture_binary_21sept.tif")
crop = rast("Base layer outputs/iucn_cropland_binary_21sept.tif")
# grassland biomes
o_biome = rast("Additional dataset inputs/Ecoregions2017/iucn_openbiome.tif")
# mask croplands and pastures to grassland biomes
both = max(past, crop, na.rm=T)
restore = both*o_biome
# remove coastal wetland and peatland restoration
cw = rast("Solution outputs/coastalwetlandrestoration_25mar2024.tif")
pr = rast("Solution outputs/peatlandrestoration_25mar2024.tif")
restore2 = ifel(restore==cw | restore==pr, 0, restore)
plot(restore2)
freq(restore2)
# remove built environment
remove = rast("Base layer outputs/excludenotrocky_landcovers_26mar2024.tif")
restore3 = ifel(restore2==remove,0,restore2) # no effect
writeRaster(restore3, "Solution outputs/grasslandrestoration_25mar2024.tif", overwrite=TRUE)

##### Reforestation/afforestation #####
# load high tree potential
treepot = rast("Additional dataset outputs/iucn_bastin_potentialforestcover_25sept2023.tif")
# load layers that currently have forest
forest_base = rast("Base layer outputs/iucn_forest_binary_21sept.tif")
nfm = rast("Solution outputs/naturalforestmanagement_25mar2024.tif")
plant = rast("Solution outputs/improvedplantations_25mar2024.tif")
currentfor = max(forest_base, nfm, plant, na.rm=T)
# remove from high tree potential
tree2 = ifel(treepot == currentfor,0,treepot)
# differentiate reforestation and afforestation based on biome type
f_biome = rast("Additional dataset inputs/Ecoregions2017/iucn_forestbiome.tif")
o_biome = rast("Additional dataset inputs/Ecoregions2017/iucn_openbiome.tif")
refor = tree2*f_biome
# afforestation in open biomes
afor = tree2*o_biome
# make sure these are mutually exclusive
overlap = refor*afor
freq(overlap) # no overlap
# also with current forest
overlap = refor*currentfor
freq(overlap)
overlap = afor*currentfor
freq(overlap)
# remove coastal wetland and peatland restoration from reforestation (leave as a conflict with afforestation)
cw = rast("Solution outputs/coastalwetlandrestoration_25mar2024.tif")
pr = rast("Solution outputs/peatlandrestoration_25mar2024.tif")
ca = rast("Solution outputs/avoidwetlandconversion_25mar2024.tif")
ap = rast("Solution outputs/avoidpeatlandconversion_25mar2024.tif")
remove = max(cw,pr,ca,ap, na.rm=T)
refor3 = refor-remove
refor3[refor3==-1] = 0
#plot(refor3)
# double check overlap
overlap = refor3*remove
#freq(overlap)
# mask to world countries
world = st_read("Additional dataset inputs/World_Countries_(Generalized) (1)/World_Countries__Generalized_.shp")
refor4 = mask(refor3, world)
afor3 = mask(afor, world)
# remove protected areas from afforestation
afor4 = mask(afor3, pa, inverse=TRUE)
# remove built environment (including rocky areas)
remove = rast("Base layer outputs/exclude_landcovers_26mar2024.tif")
refor5 = ifel(refor4==remove,0,refor4) # no effect
afor5 = ifel(afor4==remove,0,afor4) # no effect

# export
writeRaster(refor5, "Solution outputs/reforestation_25mar2024.tif", overwrite=TRUE)
writeRaster(afor5, "Solution outputs/afforestation_25mar2024.tif", overwrite=TRUE)

##### BECCS #####
# load yield data
yield = rast("Additional dataset outputs/iucn_bioenergyyield.tif")
# select anywhere with a positive yield
yield_bin = ifel(yield>0,1,0)
yield_bin[is.na(yield_bin)] = 0
#plot(yield_bin)
# load ccs basins
basins = st_read("Additional dataset outputs/combined_ccs_basins.shp")
# mask bioenergy yield to ccs
beccs = mask(yield_bin, basins)
#plot(beccs)
# mask to world countries
world = st_read("Additional dataset inputs/World_Countries_(Generalized) (1)/World_Countries__Generalized_.shp")
beccs2 = mask(beccs, world)
#plot(beccs2)
#freq(beccs2)
beccs2[is.na(beccs2)] = 0
# remove protected areas
beccs3 = mask(beccs2, pa, inverse=TRUE)
# remove unsuitable land
remove = rast("Base layer outputs/exclude_landcovers_26mar2024.tif")
beccs4 = ifel(beccs3==remove,0,beccs3)
# export
writeRaster(beccs4, "Solution outputs/beccs_25mar2024.tif", overwrite=TRUE)


