library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggspatial)
library(viridis)
library(readxl)
library(stars)
library(raster)
library(pbapply)

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 7: characteristic species suitability

### Calculating suitability for each habitat type in terms characteristic species occurrence
### Based on list of characteristic species (Bijlsma et al., Nijssen et al.)
### and species occurrence from NDFF (Nationale Databank Flora en Fauna)


## load NBP (all nature areas in Zeeland)
NBP <- st_read('sources/ext_nat_nbp.gpkg')

## put a buffer of 100m around the NBP
NBP_union <- NBP %>% st_union() %>% st_buffer(dist = 100)

## make grid of 250x250m on the NBP
ZLD_grid250 <- st_make_grid(cellsize = 250, x = NBP)[NBP_union] %>% st_as_sf() %>%
  mutate(grid_cell_id = 1:n())

plot(ZLD_grid250)

## Load in species occurrence data. It used to have a bug
## This bug is solved in the code below, but not necessary anymore as the fixed dataset is used

#NDFF$geom <-
#  pbapply::pbsapply(NDFF$geom, function(i){
## convert to text
#    geom_text <- st_as_text(i)
## remote prefix MULTISURFACE
#    geom_text <- gsub('^MULTISURFACE //(|//)$','',geom_text)


#    geom_text <- st_as_sfc(
#      geom_text, 
#      crs = sf::st_crs(i)
#    )
#    return(geom_text)
#  }) %>% st_as_sfc

#NDFF <- st_set_crs(NDFF, 28992)

#st_write(dsn = "C:/Users/nhof0/OneDrive - Provincie Zeeland/Habitattypen-buiten_N2K_Niels/R/outputs/NDFFtest.gpkg", obj = NDFF)

## load in the already fixed data
NDFF <- st_read("Habitattypen-buiten_N2K_Niels/R/NDFF_geometries_fix/NDFF_NBP25/ndff_zl_nbp_v2_fixed.gpkg")
NDFF <- st_read("sources/species/ndff_zl_nbp_v2_fixed.gpkg") ## move the file to this location!

## make points of the polygons. The middlepoint is taken for the cirular polygons
## for the kilometre squares, a random point inside the square is taken
st_to_point <- function(area_m2, geom){
  
  geom_new <-
    ifelse(area_m2 == 1e6,                  
           st_sample(x = geom, size = 1),   ## sample randomly in a km square
           st_centroid(x = geom)            ## otherwise pick the centroid
           
    ) %>% sf::st_as_sfc()
  
  
  st_crs(geom_new) <- st_crs(geom) ## make sure crs is put back
  
  return(geom_new)
  
}

NDFF_points <- 
  NDFF %>% 
  mutate(geom = st_to_point(area_m2 = area_m2,geom = geom))

## save this as geopackage because it takes more than an hour to run
st_write(dsn = 'sources/species/ndff_zl_nbp_v2_points.gpkg', obj = NDFF_points)

## import the saved geopackage
NDFF_points <- st_read('sources/species/ndff_zl_nbp_v2_points.gpkg')

## filter on the last 10 years
NDFF_points$datm_stop <- as.Date(NDFF_points$datm_stop)
NDFF_points <- NDFF_points %>%
  filter(datm_stop > as.Date("2015-05-01")) ## from May 1st 2015

## do a spatial join to join the NDFF occurrence points to the 250m grid cells
NDFF_grid <- st_join(ZLD_grid250,NDFF_points, left = F )

## load in characteristic species (separate list for flora and fauna)
KSoorten_fauna <- read_excel("sources/species/CSoortenKleineFauna 202411def.xlsx") %>%
  mutate(hcode = gsub("_", "", hcode)) ## remove underscore from habitatcode
KSoorten_flora <- read_excel("sources/species/CSoortenFloraFunga 202411def.xlsx")%>% 
  mutate(hcode = gsub("_", "", hcode))

## only habitat type and species name is important
KSoorten_fauna <- KSoorten_fauna %>%
  dplyr::select(hcode, wetnaam)
KSoorten_flora <- KSoorten_flora %>%
  dplyr::select(hcode, wetnaam)

## combine the flora and fauna lists
KSoorten <- rbind(KSoorten_fauna, KSoorten_flora)

## list all habitat types
habitat_codes <- unique(KSoorten$hcode)

## loop through all habitat types and calculate the suitability in terms of species
for (habitat_code in habitat_codes) {
  KSoorten_habtype <- KSoorten %>%
    filter(hcode == habitat_code) %>%
    pull(wetnaam)
  
  ## calculate amount of characteristic species per grid cell
  suitability_soorten <- NDFF_grid %>%
    filter(soort_wet %in% KSoorten_habtype) %>%
    distinct(grid_cell_id, soort_wet) %>% ## make sure every species is only counted once
    group_by(grid_cell_id) %>%
    summarise(aantal_aanwezig = n(), .groups = 'drop')
  
  ## add all grid cells (also the ones with no species)
  suitability_soorten <- ZLD_grid250 %>%
    left_join(suitability_soorten, by = "grid_cell_id")
  
  ## turn NA values into 0 in suitability
  suitability_soorten$aantal_aanwezig[is.na(suitability_soorten$aantal_aanwezig)] <- 0
  
  ## get the total characteristic species of the habitat type in question
  tot_KSoorten <- length(KSoorten_habtype)
  
  ## calculate the suitability value (species found/total char. species)
  suitability_soorten <- suitability_soorten %>%
    mutate(suitability_species = aantal_aanwezig / tot_KSoorten) %>%
    dplyr::select(grid_cell_id, aantal_aanwezig, suitability_species, x)
  
  ## make an sf object
  suitability_soorten <- st_as_sf(suitability_soorten)
  
  ## exporteren to geopackage
  st_write(dsn = paste0("outputs/outputmaps_species/suitability_species_",
                        habitat_code, ".gpkg"), obj = suitability_soorten, layer = "suitability_species", driver = "GPKG", append = F)
}

