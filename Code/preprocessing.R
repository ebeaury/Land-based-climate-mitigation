## Preprocessing data inputs
# August 15, 2024

library(dplyr)
library(terra)
library(sf)

#### Due to file size, datasets are linked for each processing step ####
#### More details on data access are provided in Table S1 of Supporting Methods ####

#### Set directory to folder containing data subfolders ####
path = ""
setwd(path)

## Load any file with IUCN resolution
iucn = rast("Base layer inputs/iucn_habitatclassification_fraction_lvl2__404_Grassland – Temperate__ver003.tif")

#### Peatlands ####
# Leifeld, J. & Menichetti, L. The underappreciated potential of peatlands in global climate change mitigation strategies. Nat Commun 9, 1071 (2018).
# Shared via personal communication (request for access)
peatland = rast("Additional dataset inputs/PeatlandDegradation_rev4.tif")
peatland
plot(peatland)
# Project and resample
peat_pj = project(peatland, iucn, method="bilinear")
plot(peat_pj)

# Threshold to existing peatlands
exis = ifel(peat_pj<0.5 | peat_pj==0.5, 1, 0)
plot(exis)
freq(exis)
writeRaster(exis, "Base layer inputs/iucn_leifeld2018_Peatland_global.tif")
# Threshold to peatland restoration
restore = ifel(peat_pj>0.5,1,0)
writeRaster(restore, "Base layer inputs/iucn_leifeld2018_PeatlandRestoration_global.tif")


#### Forest management data ####
# Lesiv, M. et al. Global forest management data for 2015 at a 100 m resolution. Sci Data 9, 199 (2022).
# https://zenodo.org/records/5879022
# FML_v3-2_with-colorbar.tif
forest = rast("Additional dataset inputs/FML_v3-2_with-colorbar.tif")
# select natural forest management classes
nfm = ifel(forest==20|forest==31,1,0)
plot(nfm)
# aggregate to ~1km
nfm10 = aggregate(nfm, fact=10,fun="sum") # percent of 1km region that is nfm
# reproject to iucn
nfm10 = project(nfm10, iucn, method="bilinear")
# export
writeRaster(nfm10, "Base layer inputs/iucn_lesiv2022_naturalforestmanagement_cont.tif")

# repeat process for base layer plantations (oil palms and plantation forestry)
plant = ifel(forest==32|forest==40,1,0)
plot(plant)
plant10 = aggregate(plant, fact=10, fun="sum") # percent 1km region that is combined plantation
plant10 = project(plant10, iucn, method="bilinear")
writeRaster(plant10, "Base layer inputs/iucn_lesiv2022_plantations_cont.tif")

# also export layer for oil palms (oil palms, 40, are not included in 'improved plantations')
oil = ifel(forest==40,1,0)
oil10 = aggregate(oil, fact=10, fun="sum")
oil10 = project(oil10, iucn, method="bilinear")
writeRaster(oil10, "Base layer inputs/iucn_lesiv2022_oilpalm_cont.tif")


##### Biome maps #####
# Dinerstein, E. et al. An Ecoregion-Based Approach to Protecting Half the Terrestrial Realm. BioScience 67, 534–545 (2017).
# http://ecoregions.appspot.com/
biome = st_read("Additional dataset inputs/Ecoregions2017/Ecoregions2017.shp")
biome = biome %>% select(BIOME_NAME)

# make a layer that combines temperate, tropical, subtropical biomes
names = unique(biome$BIOME_NAME)
names = names[!grepl("Boreal|Tundra|N/A", names)]
names
biome_sub = biome %>% filter(BIOME_NAME %in% names)
#sf_use_s2(FALSE)
#biomes_d = st_union(biome_sub, by_feature = FALSE)
st_write(biome_sub, "Additional dataset inputs/Ecoregions2017/temperate_tropical_biomes.shp")

# make a layer that delineates forest and grassland biomes (with no overlap at the border)
biome = st_read("Additional dataset inputs/Ecoregions2017/Ecoregions2017.shp")
biomerast = rasterize(biome, iucn, field="BIOME_NUM", background=0)
plot(biomerast)
# make a forest layer, excluding mangroves and tundra
forest = ifel(biomerast %in% c(1,2,3,4,5,6),1,0)
plot(forest)
# make a non-forest layer
grass = ifel(biomerast %in% c(7,8,9,10,12,13),1,0)
plot(grass)
# make sure these are mutually exclusive
overlap = forest*grass
freq(overlap) # no overlap
# export
writeRaster(forest, "Additional dataset inputs/Ecoregions2017/iucn_forestbiome.tif", overwrite=TRUE)
writeRaster(grass, "Additional dataset inputs/Ecoregions2017/iucn_openbiome.tif", overwrite=TRUE)
writeRaster(biomerast, "Additional dataset inputs/Ecoregions2017/iucn_allbiomes.tif", overwrite=TRUE)


##### Mangrove loss data #####
# Bunting, P. et al. Global Mangrove Extent Change 1996–2020: Global Mangrove Watch Version 3.0. Remote Sensing 14, 3657 (2022).
# https://zenodo.org/records/6894273
# gmw_v3_f1996_t2020
setwd("~Additional dataset inputs/gmw_v3_f1996_t2020")
files <- list.files(getwd(), "tif$", full.names=TRUE)
ic <- sprc(lapply(files, rast))
# simplify and tile together
simplify = function(map){
  map[map>1] = 0
  map = terra::aggregate(map, fact=44, fun="sum", na.rm=TRUE)
}
modrasts = sprc(lapply(ic, simplify)) # takes awhile
all = mosaic(modrasts, fun="sum")
plot(all, col="red") 
all # max val is  2139-- divide by the 44x44 = % pixel
# aggregated raster counting number of pixels w/ mangrove loss per 1km
# export temp file
# writeRaster(all, "~temp files/mosaic_mangroveloss_1996t2000_1km.tif", overwrite=TRUE)

# reproject to iucn
al_iucn = resample(all, iucn, method = "bilinear")
#plot(al_iucn, col="red")
# compare vals
sum(values(all), na.rm=T)
sum(values(al_iucn), na.rm=T)
# make into % based on 44x44 aggregation
al_iucn = al_iucn/1936
writeRaster(al_iucn, "~Additional dataset outputs/iucn_mosaic_mangroveloss_1996t2000_1km.tif", overwrite=TRUE)


##### Salt marsh loss data #######
# Campbell, A. D., Fatoyinbo, L., Goldberg, L. & Lagomasino, D. Global hotspots of salt marsh change and carbon emissions. Nature 612, 701–706 (2022).
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2122
# sm_loss.tif
salt = rast("~/Downloads/sm_loss.tif")
# pixel values are years of salt marsh loss
# assuming anything with a value is a loss over the study period (2000-2019)
salt = aggregate(salt, 35, fun="sum")
# exporting version so I have it
#writeRaster(salt, "~temp files/sm_loss_35.tif")
# values extremely high -- need to divide by 2000 to get closer to the estimate
# resample to IUCN
salt = resample(salt, iucn, method="bilinear")
# values in original would be 2000-2015, dividing by 2000 gets all the vals close to 1
salt2 = salt/2000
# so the sum should be number of 30m pixels in the new grid cell
# divide value by 35x35 to fix aggregation to % of pixel w/ salt marsh loss
salt2 = salt2/1225
# clamp vals to 1
salt2[salt2>1] = 1
salt2
writeRaster(salt2, "Additional dataset outputs/saltmarsh_loss_percent1km.tif")


##### Rice data #####
# Monfreda, C., Ramankutty, N. & Foley, J. A. Farming the planet: 2. Geographic distribution of crop areas, yields, physiological types, and net primary production in the year 2000. Global Biogeochemical Cycles 22, (2008).
# http://www.earthstat.org/
# HarvestedAreaYield175Crops_Geotiff

# fractional harvest that is rice
# first estimate total harvested area across all crops
setwd("Additional dataset inputs/HarvestedAreaYield175Crops_Geotiff/GeoTiff")
crops = list.files()
allstack = c()
for(C in crops){
  map = rast(paste0(C, "/", C, "_HarvestedAreaHectares.tif"))
  allstack = c(map, allstack)
}
names(allstack)
# total hectares of harvest per pixel
all_sum = sum(allstack, na.rm=T)
plot(all_sum)
# what fraction of area harvested is rice
rice = rast("rice/rice_HarvestedAreaHectares.tif")
plot(rice)
prop_rice = rice/all_sum
# project to IUCN
prop_rice_iucn = project(prop_rice, iucn, method="bilinear")
plot(prop_rice_iucn)
writeRaster(prop_rice_iucn, "Additional dataset outputs/rice_proportionalharvestedarea_25sept2023.tif")


##### Potential tree cover data #####
# Bastin, J.-F. et al. The global tree restoration potential. Science 365, 76–79 (2019).
# https://www.research-collection.ethz.ch/handle/20.500.11850/350258
# Total_potential.tif
treepotential = rast("Additional dataset inputs/BastinTotal_potential.tif")
plot(treepotential)
# project to iucn
tree_iucn = project(treepotential, iucn, method="bilinear")
plot(tree_iucn)
# select pixels with high predicted tree cover (>30%)
tree_b = ifel(tree_iucn>30,1,0)
writeRaster(tree_b, "Additional dataset outputs/iucn_bastin_potentialforestcover_25sept2023.tif")
# hansen continuous forest cover
treecov = rast("Additional dataset inputs/Hansen_treeCanopyCover_1km.tif")
plot(treecov)
# project to iucn
treecov_iucn = project(treecov, iucn, method="bilinear")
writeRaster(treecov_iucn, "Additional dataset outputs/iucn_hansentreecover_25sept2023.tif")


##### Albedo data #####
# Hasler, N., Williams, C. A., Denney, V. C., Ellis, P. W., Shrestha, S., Terasaki Hart, D. E., ... & Cook-Patton, S. C. (2024). Accounting for albedo change to identify climate-positive tree cover restoration. Nature Communications, 15(1), 2275.
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/G17RXL
# AlbedoOffset_005.tif
albedo = rast("Additional dataset inputs/AlbedoOffset_005.tif")
albedo  
# project to iucn
albedo_pj = project(albedo, iucn, method="bilinear")
plot(albedo_pj, type="interval", breaks=c(-10000,0,25,50,75,100,10000))
# identify areas where CO2e lost from albedo > CO2e gained from added forest cover (areas >100%)
albedo_b = ifel(albedo_pj>100,1,0)
#albedo_b[is.na(albedo_b)] = 1 # fill in no data values to account for pixel alignment from projecting
plot(albedo_b)
writeRaster(albedo_b, "Additional dataset outputs/iucn_alebdo_binary_15aug2024.tif", overwrite=TRUE)

##### Enhanced weathering ####
# Bertagni, M. B. & Porporato, A. The Carbon-Capture Efficiency of Natural Water Alkalinization: Implications For Enhanced weathering. Science of The Total Environment 838, 156524 (2022).
# Shared via personal communication
# derived from M. Bertagni's code mapping carbon capture efficiency rates of EW (matlab code)
ew = rast("Additional dataset inputs/EW_EBderived.tif")
plot(ew)
# project to iucn
ew_pj = project(ew, iucn, method="bilinear")
plot(ew_pj)
# select areas with a positive ACE
ew_b = ifel(ew_pj >0,1,0)
ew_b[is.na(ew_b)] = 0
plot(ew_b)
writeRaster(ew_b, "Additional dataset outputs/iucn_enhancedweathering_binary_25sept2023.tif")


#### CCS basins ####
# NETL, NATCARB Atlas Saline Basin 10km Grid (open access)
# https://edx.netl.doe.gov/dataset/natcarb-atlas-saline-basin-10km-grid
# natcarb_saline_poly_shapefile.zip
# U.S. Geological Survey World Petroleum Assessment 2000 (open access)
# https://certmapper.cr.usgs.gov/data/apps/we-data/
# tps_geog.e00.shp
global = st_read("Additional dataset inputs/tps_geogshp/tps_geog.shp")
plot(st_geometry(global))
# define crs
st_crs(global) = st_crs(iucn)
# project to crs with meters
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"
global_t = st_transform(global, robinson)
# add a 40 km buffer around the basins
global_buff = st_buffer(global_t, 40000)
plot(st_geometry(global_buff), col="red")
plot(st_geometry(global_t), col="blue", add=T)
# simplify
global2 = st_union(global_buff)
plot(st_geometry(global2))
# north america
na = st_read("Additional dataset inputs/RCSPSalineOutline/natcarb_saline_poly_v1502.shp")
plot(st_geometry(na))
# define crs
st_crs(na) = st_crs(iucn)
# project
na_t = st_transform(na, robinson)
# buffer 40 km
na_buff = st_buffer(na_t, 40000)
plot(st_geometry(na_buff), col="red")
plot(st_geometry(na_t), col="blue", add=T)
# simplify
na2 = st_union(na_buff)
plot(st_geometry(na2))
# merge
all = st_union(global2, na2)
#plot(st_geometry(all))
st_write(all, "Additional dataset outputs/combined_ccs_basins_15aug2024.shp", append=FALSE)


#### Bionenergy yield data ####
# Li, W. et al. Mapping the yields of lignocellulosic bioenergy crops from observations at the global scale. Earth System Science Data 12, 789–804 (2020).
# https://zenodo.org/records/3274254
# Bioenergy_crop_yields.zip
yield = rast("Additional dataset inputs/Bioenergy_crop_yields.nc")
yield
names(yield)
plot(yield$Best_crop) # maximum yield of the best crop suited for that environment
# project to iucn
yield_iucn = project(yield$Best_crop, iucn, method="bilinear")
plot(yield_iucn)
writeRaster(yield_iucn, "Additional dataset outputs/iucn_bioenergyyield.tif")


#### Biochar ####
# Karan, S. K., Woolf, D., Azzi, E. S., Sundberg, C. & Wood, S. A. Potential for biochar carbon sequestration from crop residues: A global spatially explicit assessment. GCB Bioenergy 15, 1424–1436 (2023).
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/Y6NGFM
# bc_100_constrained.tif
bio = rast("Additional dataset inputs/bc_100_constrained.tif")
plot(bio)
sum(values(bio, na.rm=T)) # 360591950
# disaggregate data by factor - divide sequestration by more pixels
bio_d = disagg(bio, 10, method="bilinear")
sum(values(bio_d, na.rm=T)) # great! similar but an order of magnitude higher
# divide by 100 (10x10)?
bio_d = bio_d/100
sum(values(bio_d, na.rm=T)) # match net sequestration at coarser resolution (slightly off but really close!)

# resample to IUCN resolution
bio_pj = resample(bio_d, iucn, method="bilinear") # lost quite a bit of net sequestration potential
plot(bio_pj)
sum(values(bio_pj, na.rm=T)) # lose values here - under-estimating sequestration potential
# compare to # pixels w/ a value
length(bio_d[bio_d>0])
length(bio_pj[bio_pj>0]) # under-estimating # pixels with a positive sequestration value
writeRaster(bio_pj, "Additional dataset outputs/bc_100_constrained_iucn.tif", overwrite=TRUE)


#### World Database of Protected Areas ####
# UNEP_WCMC and IUCN. Protected Planet: The World Database on Protected Areas (WDPA). (2019).
# https://www.protectedplanet.net/en
# subset to IUCN accepted areas, and levels I-IV protected (limited land use change)
wdp = vect("Additional dataset inputs/WDPA/WDPA_Merged.shp")
wdp_sub = wdp[wdp$IUCN_CAT == "Ia" | wdp$IUCN_CAT == "Ib" | wdp$IUCN_CAT == "II" |
                wdp$IUCN_CAT == "III" | wdp$IUCN_CAT == "IV"]
wdp_sub = wdp_sub[wdp_sub$PA_DEF==1]
table(wdp_sub$IUCN_CAT)
#wdp_ag = aggregate(wdp_sub, dissolve=TRUE)
# mask to terrestrial landscapes
# world countries
# https://hub.arcgis.com/maps/esri::world-countries-generalized
world = vect("Additional dataset inputs/World_Countries_(Generalized) (1)/World_Countries__Generalized_.shp")
wdp_t = mask(wdp_sub, world)
plot(wdp_t)
writeVector(wdp_t, "Additional dataset inputs/WDPA/WDPA_Subset_I-IV.shp", overwrite=TRUE)
