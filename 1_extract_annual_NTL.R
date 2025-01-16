
### extract_annual
### Adrian Amaya
### 2023-02-18


###### r setup
extractthis = "slmavg"         # "slmavg" = sl corrected, masked (EOG), average radiance; 
                              # "cfcvg" = cloud-free coverage; 
                              # "mavg" = masked (EOG), average radiance, not sl corrected; "cvg" = coverage
extractfun = "sum"          # sum, mean, weighted_sum, or weighted_mean
withweights = "no"          # cfcvg, area, or if no weights type no
withmask = "no"             # builtarea, If no mask type no


ntldir = "G:/My Drive/Nightlights_Global/"

########################################################

#Directories ####
datadir = paste0(ntldir, "data/")
shpdir = paste0(ntldir, "shapefiles/")
mtifdir = paste0(datadir, "viirs_monthly_tiffs/")
atifdir = paste0(datadir, "viirs_annual_tiffs/")
codedir = paste0(ntldir, "code/")
figdir = paste0(ntldir, "figures/")

`%!in%` = Negate(`%in%`)

library(rnaturalearth)
library(terra)
library(dplyr)
library(exactextractr)
library(sf)
library(ggplot2)
library(countrycode)
library(stringr)


######### check-inputs-prep
if(extractthis %!in% c("slmavg", "mavg", "cfcvg", "cvg")){
  stop(paste0("extractthis = ", extractthis, " not allowed"))
}
if(extractfun %!in% c("sum", "mean", "median", "min", "max", "weighted_sum", "weighted_mean")){
  stop(paste0("extractfun = ", extractfun, " not allowed"))
}
if(withweights %!in% c("cfcvg", "area", "no")){
  stop(paste0("withweights = ", withweights, " not allowed"))
}
if(withmask %!in% c("builtarea", "no")){
  stop(paste0("withmask = ", withmask, " not allowed"))
}

if(extractthis == "slmavg"){
  y1 = 2013
  yn = 9
  extract.pattern = "vcmslcfg.*average_masked.dat.tif$"
  filename = paste0(extractthis)
}
if(extractthis == "mavg"){
  y1 = 2011
  yn = 11
  m1 = 4
  extract.pattern = "vcmcfg.*avg_rade9h.masked.tif$"
  filename = paste0(extractthis)
}
if(extractthis == "cfcvg"){
  y1 = 2013
  yn = 10
  m1 = 1
  extract.pattern = "vcmslcfg.*cf_cvg.tif$"
  filename = paste0(extractthis)
}
if(extractthis == "cvg"){
  y1 = 2013
  yn = 9
  m1 = 4
  extract.pattern = "vcmslcfg.*.cvg.tif$"
  filename = paste0(extractthis)
}
##################
if(withmask %in% c("urban", "rural", "builtarea")){
  umv = matrix(c(0, Inf, 1), ncol=3, byrow=T) # a matrix to make urbanmask(maskvalues=0)/ruralmask(maskvalue=1)
  if(withmask== "urban"){
    valuetomask = 0
  }
  if(withmask== "rural"){
    valuetomask = 1
  }
  if(withmask== "builtarea"){
    valuetomask = 0
  }
  filename = paste0(filename, "_", withmask)
}
##################
if(withweights == "cfcvg"){
  weights.pattern = "vcmslcfg.*cf_cvg.*\\.tif$"
  filename = paste0(filename, "_", withweights)
} else if(withweights == "area"){
  weights = "area"
  filename = paste0(filename, "_", withweights)
} else if(withweights == "no"){
  weights = NULL
}
##################

if(extractfun == "weighted_sum"){
  weights.pattern = "vcmslcfg.*cf_cvg.*\\.tif$"
  filename = paste0(filename, "w_sum")
} else if(extractfun == "weighted_mean"){
  filename = paste0(filename, "w_mean")
} else{
  filename = paste0(filename, "_", extractfun)
}
##################

# Make extracted varname and finalize filename
varname = filename
filename = paste0(atifdir, filename, ".csv")

##################
# COUNTRY POLYGONS
# Map-units only
rnecountries = ne_countries(scale = 110, type = "countries", returnclass = "sf") %>%
  filter(type %!in% c("Dependency", "Indeterminate")) %>%
  rename(iso3c = gu_a3, country = geounit) %>%
  mutate(iso3c = case_when(sovereignt == "Kosovo" ~ "KSV",
                           sovereignt == "Northern Cyprus" ~ "CYN",
                           TRUE ~ iso3c))

# Built area mask
builtareamask = rast(paste0(shpdir, "GHS_BUILT/builtarea_mask.tif"))



###### EXTRACT

rasterlist = list.files(paste0(atifdir), pattern = extract.pattern, full.names=T)
ordered = data.frame(filenames = rasterlist, rasteryear = str_extract(rasterlist, "\\d{4}")) %>% 
  arrange(rasteryear)
extract.rasters = ordered[[1]]

annual = list()

for(n in 1:length(extract.rasters)){
  year = y1 + n
  print(year)
  terraraster = extract.rasters[[n]] %>% terra::rast()
  raster = terraraster
  rm(terraraster)
    
  polygons = rnecountries %>% vect() %>%
    project(raster) %>% st_as_sf()
  crs(polygons) == crs(raster)
  
  extracted = exact_extract(raster, polygons, fun=extractfun, weights=weights, append_cols= c("country", "iso3c")) %>%
    rename(!! c(varname) := c(extractfun))

  
  annual[[n]] = extracted
  annual[[n]]$year = year
}

NTL.annual = bind_rows(annual)

write.csv(NTL.annual, paste0("NTL_annual_", filename), row.names = F)




