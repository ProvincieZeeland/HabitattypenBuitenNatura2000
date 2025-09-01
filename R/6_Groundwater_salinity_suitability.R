library(sf)          ## gis package for vectors
library(dplyr)       ## package for 'piping'
library(ggplot2)     ## package voor plots
library(ggspatial)   ## package for gis plots
library(stars)       ## gis package for rasters
library(readxl)      ## to read excel files
library(odbc)        ## ro read access files
library(DBI)         ## to read access files

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 6: Groundwater salinity suitability

### Calculating suitability for each habitat type in terms of groundwater salinity
### Based on abiotic requirements from N2000 profile documents,
### and groundwater salinity maps from Delsman et al., 2018 (FRESHEM)
### Salinity maps were altered before use. See script 4 and 5.

## load in groundwater salinity
Cl <- stars::read_stars('sources/groundwater_salinity/chloride_0tot-250_mean.tif')

hist(Cl)

plot(Cl, breaks = "equal")

## import salinity ranges for each habitat type, through an Access connection
accessconn <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=sources/VereistenHabitattypenDec2008.mdb;")
Cl_habtypes <- dbReadTable(accessconn, "groundwater_salinityHabtype_Juni2008") # de Juni 2008 versie komt overeen met de huidige profieldocumenten op natura2000.nl
dbDisconnect(accessconn)
## Remove columns with "opmerkingen" and "bespreekgeval"
Cl_habtypes <- Cl_habtypes[,-c(2, 9,10)]
## Remove rows that have no requirements for Cl (marine habitat types)
Cl_habtypes <- Cl_habtypes %>%
  filter(!if_all(-HabitatCode, ~ is.na(.) | . == ""))

## import Cl ranges for the classes, sourced from Runhaar et al., 2009
Cl_ranges <- read_excel("sources/groundwater_salinity/Cl_ranges.xlsx") 

## function to link Cl categories to pH values
get_limits_Cl <- function(habitat_code) {
  ## filter data on selected habitat type
  habitat_row <- Cl_habtypes[Cl_habtypes$HabitatCode == habitat_code, ]
  
  ## stop if habitat type is not found
  if (nrow(habitat_row) == 0) {
    stop("Habitat code niet gevonden")
  }
  
  ## all categories that have K (or a variation of K). This is the core range.
  relevant_categories_K <- colnames(Cl_habtypes)[-1][!is.na(habitat_row[1, -1]) & habitat_row[1, -1] %in% c("K", "Ka", "Kb")]
  
  ## designate upper and lower limits
  lower_limit_K <- Inf
  upper_limit_K <- -Inf
  
  ## loop through categories and find the accompanying Cl ranges
  for (category in relevant_categories_K) {
    ## Cl range voor de categorie
    Cl_range <- Cl_ranges[Cl_ranges$Klasse == category, ]
    
    ## stop if the categorie does not exist in the Cl ranges file
    if (nrow(Cl_range) == 0) {
      stop(paste("No Cl range defined for category:", category))
    }
    
    ## update upper and lower limits
    lower_limit_K <- min(lower_limit_K, Cl_range$Ondergrens, na.rm = TRUE)
    upper_limit_K <- max(upper_limit_K, Cl_range$Bovengrens, na.rm = TRUE)
  }
  
  ## all categories that have A (or a variation of A). This is the supplementary range.
  relevant_categories_A <- colnames(Cl_habtypes)[-1][!is.na(habitat_row[1, -1]) & habitat_row[1, -1] %in% c("A", "Aa", "Ab")]
  
  ## designate upper and lower limits
  lower_limit_A <- Inf
  upper_limit_A <- -Inf
  
  ## loop through categories and find the accompanying pH ranges
  for (category in relevant_categories_A) {
    Cl_range <- Cl_ranges[Cl_ranges$Klasse == category, ]
    
    ## stop if the categorie does not exist in the Cl ranges file
    if (nrow(Cl_range) == 0) {
      stop(paste("No Cl range defined for category:", category))
    }
    
    ## update upper and lower limits
    lower_limit_A <- min(lower_limit_A, Cl_range$Ondergrens, na.rm = TRUE)
    upper_limit_A <- max(upper_limit_A, Cl_range$Bovengrens, na.rm = TRUE)
  }
  
  return(list(lowerA = lower_limit_A, upperA = upper_limit_A, lowerK = lower_limit_K, upperK = upper_limit_K))
}

## List all habitat types
habitat_codes <- unique(Cl_habtypes$HabitatCode)

## Make a dataframe to store all pH ranges
limits_df <- data.frame(habitat_code = character(),
                        lowerA = numeric(),
                        lowerK = numeric(),
                        upperK = numeric(),
                        upperA = numeric(),
                        stringsAsFactors = FALSE)

## loop through all habitat types and put the reference values in the dataframe
for (habitat_code in habitat_codes) {
  ## get limits for K and A
  getlimits <- get_limits_Cl(habitat_code)
  
  ## combine the limits in the df
  limits <- data.frame(lowerA = getlimits$lowerA,
                       lowerK = getlimits$lowerK,
                       upperK = getlimits$upperK,
                       upperA = getlimits$upperA)
  
  ## correct limits if necessary (when K is the same as A, the following calculations will not work so add slight difference)
  if (limits$upperA <= limits$upperK) {
    limits$upperA <- limits$upperK + 0.0001
  }
  if (limits$lowerA >= limits$lowerK) {
    limits$lowerA <- limits$lowerK - 0.0001
  }
  
  ## add habitatcode to limits
  limits$habitat_code <- habitat_code
  
  ## add limits to dataframe
  limits_df <- rbind(limits_df, limits)
}

## reference values dataframe for each habitat type
referentie_waarden <- limits_df %>%
  mutate(
    habitattype = habitat_code,
    lower0 = lowerA - (upperA - lowerA) * 0.1 , #lower0 ligt 10% lager dan de hele range (was eerst 500)
    upper0 = upperA + (upperA - lowerA) * 0.1, #upper0 ligt 10% hoger dan de hele range (was eerst 500)
    predictor   = "Cl"
  ) %>%
  dplyr::select(habitattype, predictor, lower0, lowerA, lowerK, upperK, upperA, upper0)

## Make a suitability map by looping through all habitat types
teller <- 0
tot_aantal_kaarten <- length(unique(referentie_waarden$habitattype))

for (i in unique(referentie_waarden$habitattype)) {
  
  ## make a function to translate a given pH to a value between 0 and 1, based on reference
  suitability_functie_Cl <-
    approxfun(x = referentie_waarden %>% filter(habitattype ==  i, predictor == 'Cl') %>%    ### pak uit ref waarden het habitattype en het type
                dplyr::select(lower0,
                              lowerA,
                              lowerK,
                              upperK,
                              upperA,
                              upper0) %>% unlist, 
              y = c(0, .5, 1, 1, .5, 0),    ## suitability score for each range (A, K and 0)
              yleft = 0,    ## which values left of this range?
              yright = 0)     ## which values right of this range?
  
  ## add suitability values to the map
  Cl_suitability_kaart <- 
    st_apply(Cl, c('x','y'), suitability_functie_Cl)
  
  ## change raster to vector
  Cl_suitability_kaart_polygon <- st_as_sf(Cl_suitability_kaart)
  
  ## write to geopackage (vector)
  st_write(dsn = paste0("outputs/outputmaps_salinity/suitability_salinity_", i, ".gpkg"),
           obj = Cl_suitability_kaart_polygon)
  
  ## increase count by one
  teller <- teller + 1
  
  ## give a notification when a map is finished
  print(paste0("suitability_salinity_", i, ".gpkg created successfully (", teller, "/", tot_aantal_kaarten, ")."))
}
