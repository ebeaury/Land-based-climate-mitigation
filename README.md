# Land-based-climate-mitigation
Data and code associated with: 'Global suitability and spatial overlap of land-based climate mitigation strategies'. In press as of 9/20/2024.

Authors: Evelyn M. Beaury*, Jeffrey R. Smith, Jonathan Levine.

*lead and corresponding author, eve.beaury@gmail.com

# Reference

Beaury, E.M.B, Smith, J.R., Levine, J. Global suitability and spatial overlap of land-based climate mitigation strategies. Global Change Biology. In press.

# Abstract

Land-based mitigation strategies (LBMS) are critical to reducing climate change and will require large areas for their implementation. Yet few studies have considered how and where LBMS either compete for land or could be deployed jointly across Earth’s surface. To assess the opportunity costs of scaling up LBMS, we derived high resolution estimates of the land suitable for 19 different LBMS, including ecosystem maintenance, ecosystem restoration, carbon-smart agricultural and forestry management, and converting land to novel states. Each 1km resolution map was derived using Earth’s current geographic and biophysical features without socioeconomic constraints. By overlaying these maps, we estimated 8.56 billion hectares theoretically suitable for LBMS across the Earth. This includes 5.20 Bha where only one of the studied strategies is suitable, typically the strategy that involves maintaining the current ecosystem and the carbon it stores. The other 3.36 Bha are suitable for more than one LBMS, framing the choices society has among which LBMS to implement. The majority of these regions of overlapping LBMS include strategies that conflict with one another, such as the conflict between better management of existing land cover types and restoration-based strategies such as reforestation. At the same time, we identified several agricultural management LBMS that were geographically compatible over large areas, including for example, enhanced chemical weathering and improved plantation rotations. Our analysis presents local stakeholders, communities, and governments with the range of LBMS options, and the opportunity costs associated with scaling up any given LBMS to reduce global climate change.

# File description
All data files are linked for working through the code to derive binary 1km maps for each of the 19 land-based climate mitigation strategies. Download links for original data inputs and detailed methods are described in the manuscript and supporting information, which is in press (09/10/2024). A previous version of the manuscript is presently available on bioRxiv: https://doi.org/10.1101/2024.01.04.574063.

# Strategy output maps
Beaury, Evelyn (2024). Strategy maps. figshare. Dataset. https://doi.org/10.6084/m9.figshare.24933312.
Figshare zip folder containing 1km resolution binary output maps for each mitigation strategy.

# R files

## base habitat types.R

Preprocessing data to assign pixels as mutually exclusive land cover types, given Earth's current distribution of land. The base layer outputs are used to derive mitigation strategy maps and to assess whether strategies are conflicting (change the current land cover type) or are mutually compatible (apply to current land cover type). Some files are exported as temporary layers, which are then further processed in later coding steps.

## preprocessing.R

Preprocessing additional input data layers prior to deriving maps for each mitigation strategy. Steps incldue discretizing, resampling, and projecting raster data.

## strategy maps.R

Final processing for creating a 1km map for each mitigation strategy, using base layers, additional input datasets, and any processing done prior to this step. Code is not necessarily sequential given layers are harmonzied with one another as they are created.

## threshold sensitivity.R

Plots showing change in area given discretizing continuous input files.




