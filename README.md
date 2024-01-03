# Land-based-climate-mitigation
Data and code associated with 'Global spatial potential for land-based climate mitigation'.

Authors: Evelyn Beaury*, Jeffrey Smith, Jonathan Levine.

*lead and corresponding author, eve.beaury@gmail.com or evelynbeaury@princeton.edu

All data files are provided for working through the code to derive binary 1km maps for each of 19 land-based climate mitigation strategies. Download links for original data inputs and detailed methods are  described in the manuscript and supporting information, which is under review (1/3/2024) and available on bioRxiv: .

Code is ordered:
1. base habitat types.R
2. preprocessing.R
3. strategy maps.R

# R files

## base habitat types.R

Preprocessing data to assign pixels as mutually exclusive land cover types, given Earth's current distribution of land. The base layer outputs are used to derive mitigation strategy maps and to assess whether strategies are conflicting (change the current land cover type) or are mutually compatible (apply to current land cover type). Some files are exported as temporary layers, which are then further processed in later coding steps.

## preprocessing.R

Preprocessing additional input data layers prior to deriving maps for each mitigation strategy. Steps incldue discretizing, resampling, and projecting raster data.

## strategy maps.R

Final processing for creating a 1km map for each mitigation strategy, using base layers, additional input datasets, and any processing done prior to this step. Code is not necessarily sequential given layers are harmonzied with one another as they are created.

# Data folders

All are directed to in the code. Strategy outputs includes the exported raster file for each individual mitigation strategy.

1. Base layer inputs
2. Base layer outputs
3. Additional dataset inputs
4. Additional dataset outputs
5. temp files
6. Solution outputs
All folders are directed to within the code. 




