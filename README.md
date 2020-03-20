# Deines et al. 2020: Derived data and analysis code

21 February 2020
Code by: Jillian Deines with contributions from Samuel Zipper and Caitlin Rottler
Contact: jillian.deines@gmail.com

This codebase accompanies the paper:

Deines, J.M., M.E. Schipanski, B. Golden, S.C. Zipper, S. Nozari, C. Rottler, B. Guerrero, & V. Sharda. 2020. Transitions from irrigated to dryland agriculture in the Ogallala Aquifer: Land use suitability and regional economic impacts. Agricultural Water Management 233:106061. DOI: https://doi.org/10.1016/j.agwat.2020.106061

## Contents

### Data

* Data needed to reproduce the figures from Deines et al. 2020 can be found in the `data` folder.
* Input data such as groundwater level projections and annual irrigation maps consist of large, publicly available geospatial files. The code accesses these through pre-ingested assets in Google Earth Engine. Permanent locations for these source data that do not require a Google Earth Engine account include:
  * Annual irrigated areas as mapped in [Deines et al. 2019](https://t.co/Y1qaiUSrnz?amp=1) using Landsat satellite data are also available for download at [Hydroshare](https://www.https://www.hydroshare.org/resource/a371fd69d41b4232806d81e17fe4efcb/); DOI https://doi.org/10.4211/hs.a371fd69d41b4232806d81e17fe4efcb.
  * Groundwater level maps available from XXX and described in [Haacker et al. 2016](https://ngwa.onlinelibrary.wiley.com/doi/full/10.1111/gwat.12350)
  * USDA NASS Cropland Data Layers are available from https://nassgeodata.gmu.edu/CropScape/
  * NRCS [gSSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/home/?cid=nrcs142p2_053628) soil data are available from https://nrcs.app.box.com/v/soils
  
  
### Code

Script filenames are numbered by manuscript figure numbers. Code primarily uses [R Markdown](https://rmarkdown.rstudio.com/) (*.Rmd) within an R project structure. Operational scripts have extension .Rmd; knitted outputs in .md (for easing viewing on Github) and .html (for desktop viewing) are also provided. When this repository is cloned into an R project, the `here` package should manage all relative filepaths.




