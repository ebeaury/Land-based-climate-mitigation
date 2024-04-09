## Creating binary base habitat layers
# Sept 21, 2023
# Updated Mar 26, 2024

library(terra)
library(dplyr)
library(sf)

## IUCN habitat types & peatland supporting data
# 1) Pull all layers that match habitat types
# 2) Combine and select pixels >50%
# 3) Add in mosaic landscapes
# 4) Mask to world countries

## IUCN land cover types to exclude
# Rocky, deserts, artificial, urban

#### Set directory ####
path = ""
setwd(path)
# path = "/Users/labadmin/Documents/Eve/NEW PIPELINE SEPT 21/"

#### Dominant classifications first

## Croplands
crop = rast(list.files(pattern = ".*Arable*.+\\.tif$"))
crop_bin = ifel(crop>500,1,0)
#writeRaster(crop_bin, "temp files/temp_crop.tif", overwrite=TRUE)

## Pasture
pasture = rast(list.files(pattern = ".*Pasture*.+\\.tif$"))
pasture_bin = ifel(pasture>500,1,0)
#writeRaster(pasture_bin, "temp files/temp_pasture.tif")

## Plantations
plantations = rast(list.files(pattern = ".*Plantations*.+\\.tif$"))
plantations_bin = ifel(plantations>500,1,0)
#writeRaster(plantations_bin, "temp files/temp_plantation.tif")

## Forests
files = list.files(pattern = ".*Forest*.+\\.tif$")
# drop mangroves (included in wetlands)
files = files[!grepl("mangrove", files)]
files = rast(files)
forest = sum(files, na.rm=T)
forest_bin = ifel(forest>500,1,0)
#writeRaster(forest_bin, "temp files/temp_forest.tif")

## Grasslands
files = list.files(pattern = ".*Grassland|Shrubland|Savanna*.+\\.tif$")
files = files[!grepl(".aux", files)]
files = rast(files)
grassland = sum(files, na.rm=T)
grassland_bin = ifel(grassland>500,1,0)
#writeRaster(grassland_bin, "temp files/temp_grassland.tif")

## Wetlands
files = list.files(pattern = ".*Wetlands|mangrove*.+\\.tif$")
# remove water bodies
files = files[!grepl("lakes|rivers|springs|deltas", files)]
files = files[!grepl(".aux", files)]
files = rast(files) 
wetlands = sum(files, na.rm=T)
wetlands_bin = ifel(wetlands>500,1,0)
#writeRaster(wetlands_bin, "temp files/temp_wetland.tif")

## Peatlands
files = list.files(pattern = ".*Peatland*.+\\.tif$")
peatland = rast(files)
# remove everything but wetlands
removemax = max(forest_bin, pasture_bin, plantations_bin, crop_bin, grassland_bin, na.rm=T)
peatland_bin = ifel(peatland==removemax, 0, peatland)
writeRaster(peatland_bin, "temp files/temp_peatland.tif")

## Allocate mosaic vegetation
# for cases in which % cover is equal, set a priority to allocate to a specific layer
# apply constant to differentiate (adjust values to larger integers)
forest2 = (forest*10)+5
wetlands2 = (wetlands*10)+4
grassland2 = (grassland*10)+3
pasture2 = (pasture*10)+2
plantations2 = (plantations*10)+1
crop2 = crop*10
continuous = c(forest2,wetlands2, grassland2, pasture2, plantations2, crop2)
# identify the majority class
maxcontinuous = max(continuous, na.rm=TRUE)
binary = ifel(continuous==maxcontinuous,1,0) # denotes areas where each land cover is the majority
#plot(binary)

# drop areas that have already been classified as one of the other types
#combine = max(forest_bin, grassland_bin, pasture_bin, peatland_bin, plantations_bin, wetlands_bin, crop_bin, na.rm=T)
#writeRaster(combine, "temp files/combinedbinary_premosaic.tif")
combine = rast("temp files/combinedbinary_premosaic.tif")
# flip pixels
combine = ifel(combine ==1,0,1)
# drop areas where the sum across types is <50%
sumcontinuous = sum(forest,wetlands, grassland, pasture, plantations, crop, na.rm=T)
sum_bin = ifel(sumcontinuous>500,1,0)
mask = combine*sum_bin # all the pixels that need an assigned mosaic (not already classified and has >50% cover of the combined types)
freq(mask) # ~650 Mha that we can assign to a type

# multiply by max value to identify which land cover type receives the area
binary2 = binary*mask
#plot(binary2) # mosaic landscapes that need to be allocated
#freq(binary2)

## Combine back with classified layers and mask to countries
world = st_read("Additional dataset inputs/World_Countries_(Generalized) (1)/World_Countries__Generalized_.shp")

# Order in stack: c(forest2,wetlands2, grassland2, pasture2, plantations2, crop2)
allforest = max(forest_bin, binary2[[1]], na.rm=T)
#plot(allforest)
allforest = mask(allforest, world)
writeRaster(allforest, "Base layer outputs/iucn_forest_binary_21sept.tif", overwrite=TRUE)

allcropland = max(crop_bin, binary2[[6]], na.rm=T)
#plot(allcropland)
allcropland = mask(allcropland, world)
writeRaster(allcropland, "Base layer outputs/iucn_cropland_binary_21sept.tif", overwrite=TRUE)

allpasture = max(pasture_bin, binary2[[4]], na.rm=T)
#plot(allpasture)
allpasture = mask(allpasture, world)
writeRaster(allpasture, "Base layer outputs/iucn_pasture_binary_21sept.tif", overwrite=TRUE)

allplantation = max(plantations_bin, binary2[[5]], na.rm=T)
#plot(allplantation)
allplantation = mask(allplantation, world)
writeRaster(allplantation, "Base layer outputs/iucn_plantation_binary_21sept.tif", overwrite=TRUE)

allgrassland = max(grassland_bin, binary2[[3]], na.rm=T)
#plot(allgrassland)
allgrassland = mask(allgrassland, world)
writeRaster(allgrassland, "Base layer outputs/iucn_grassland_binary_21sept.tif", overwrite=TRUE)

# remove peatland from wetlands
allwetland = max(wetlands_bin, binary2[[2]], na.rm=T)
allwetland2 = ifel(allwetland==peatland_bin,0,allwetland)
#plot(allwetland)
#freq(allwetland)
allwetland2 = mask(allwetland2, world)
writeRaster(allwetland2, "Base layer outputs/iucn_wetland_binary_21sept.tif", overwrite=TRUE)

# export peatland (didn't receive extra)
writeRaster(peatland_bin, "Base layer outputs/iucn_peatland_binary_21sept.tif", overwrite=TRUE)

##
##
##

## Verifying process worked
# Expect all layers to be mutually exclusive
setwd("Base layer outputs")
files = list.files()
maps = rast(files)
#plot(maps)
sum = sum(maps, na.rm=T)
#plot(sum) # no overlap

##
##
##

## Creating a surface of unsuitable land cover types for LBMS
setwd("Base layer inputs")
# Two layers
  # one to exclude rocky areas (from particular layers, such as reforestation/BECCS)
  # one including rocky areas (for particular layers, such as coastal wetland restoration)
files = list.files(pattern = ".*Rocky|Desert|Artificial|Urban*.+\\.tif$")
# remove cold deserts (included in tundra/boreal for peatland conservation and restoration)
files = files[!grepl("Cold", files)]
files = rast(files) 
unsuit = sum(files, na.rm=T)
#plot(unsuit)
unsuit_bin = ifel(unsuit>500,1,0)
#plot(unsuit_bin)
# check overlap with other dominant lc types
overlap = unsuit_bin*sum 
#plot(overlap) # some small degree of overlap (assuming rocky areas in mosaic vegetation)
# consider these areas in bounds for LBMS
unsuit_bin2 = ifel(unsuit_bin==sum,0,unsuit_bin)
#plot(unsuit_bin2)
# areas to exclude from LBMS
writeRaster(unsuit_bin2, "Base layer outputs/exclude_landcovers_26mar2024.tif", overwrite=TRUE)

# Next layer - don't include rocky
files = files[!grepl("Rocky", files)]
files = rast(files) 
unsuit = sum(files, na.rm=T)
unsuit_bin = ifel(unsuit>500,1,0)
# remove dominant lc types
unsuit_bin2 = ifel(unsuit_bin==sum,0,unsuit_bin)
writeRaster(unsuit_bin2, "Base layer outputs/excludenotrocky_landcovers_26mar2024.tif", overwrite=TRUE)
