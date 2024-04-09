# Land-based-climate-mitigation
Data and code associated with 'Global spatial potential for land-based climate mitigation'.

Authors: Evelyn Beaury*, Jeffrey Smith, Jonathan Levine.

*lead and corresponding author, eve.beaury@gmail.com or evelynbeaury@princeton.edu

All data files are linked for working through the code to derive binary 1km maps for each of 19 land-based climate mitigation strategies. Download links for original data inputs and detailed methods are described in the manuscript and supporting information, which is under review (1/3/2024) and available on bioRxiv: https://doi.org/10.1101/2024.01.04.574063.

# Strategy output maps
Beaury, Evelyn (2024). Strategy maps. figshare. Dataset. https://doi.org/10.6084/m9.figshare.24933312.v1
Figshare zip folder containing 1km resolution binary output maps for each mitigation strategy

# R files

## base habitat types.R

Preprocessing data to assign pixels as mutually exclusive land cover types, given Earth's current distribution of land. The base layer outputs are used to derive mitigation strategy maps and to assess whether strategies are conflicting (change the current land cover type) or are mutually compatible (apply to current land cover type). Some files are exported as temporary layers, which are then further processed in later coding steps.

## preprocessing.R

Preprocessing additional input data layers prior to deriving maps for each mitigation strategy. Steps incldue discretizing, resampling, and projecting raster data.

## strategy maps.R

Final processing for creating a 1km map for each mitigation strategy, using base layers, additional input datasets, and any processing done prior to this step. Code is not necessarily sequential given layers are harmonzied with one another as they are created.

## threshold sensitivity.R

Plots showing change in area given discretizing continuous input files.




