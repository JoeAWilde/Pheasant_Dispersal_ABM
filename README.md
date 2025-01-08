# Pheasant dispersal model
## Repo structure
As you might expect, all of the code needed to run the model is in the "code/" folder, found at the root. Within that folder, there are 3 R scripts labelled with "1_", "2_", and "3_". These are the analytical steps of the process that give us the rules by which the pheasants move. We then have 3 folders, "/APHA sites/", "/ATLAS sites/", and "/PA sites/". Each of these contain the R scripts needed to run the simulation at the anonymised APHA sites, at Northwyke, and across the 5 releases at 9 sites at varying distances from protected areas (or PAs). The outputs from these scripts need to go into a folder called "outputs" that should be at the root (i.e. alongside the "code/" folder). Because the outputs from the models are too large for GitHub hosting, this folder will need to be created, and the sub-folders within here will need to be created manually. The general structure within "outputs/" is "outputs/script_1/", "outputs/script_2/", etc. but this will need to inferred from the output paths in the R scripts. 

### APHA data
All data for the APHA sites are not available on this repository for privacy reasons. If this data is needed, please reach out to myself or APHA, as permission will be needed. 

### ATLAS data
The landscape data is available for Northwyke site, but the tracking data is too large for GitHub to host. Again, if this data is needed, please reach out to myself or Joah Madden at University of Exeter. 

### Protected area sites
The names and coordinates of all 5 releases across 9 sites close to protected area boundaries can be found in "all_PA_sites.xlsx" at the root. The scripts in "code/PA sites/" will read this in and run the simulations at these sites.

### Missing data
All of the landscape data necessary for this model to run is too large for GitHub to host, but it is all freely available online. 
The UKCEH habitat rasters can be downloaded from: https://www.ceh.ac.uk/data/ukceh-land-cover-maps
The linear woody features raster can be downloaded from: https://www.ceh.ac.uk/data/ukceh-land-cover-plus-hedgerows-2016-2021-england
The shapefiles of the SPAs, SACS, RAMSAR, and SSSIs can be downloaded at: https://jncc.gov.uk/our-work/uk-protected-area-datasets-for-download/
Once downloaded, these will need to be put in the "data/" folder in their own sub-folders. The structure of these sub-folders can be found in "4_" script of the respective site's code sub-folder.

## Simulating with different management techniques
Unfortunately there's no easy one-button method to enact the management changes, and what needs to be changed depends on the management technique. For example, adding new/more feeders to a site would require adding a file with the coordinates of those feeders to the script that pulls in/creates coordinates for each site (which would be the "4_" script in the respective sub-folder). If you wish to enact a certain management change but are not sure where to start, reach out to me and I can try to help.
