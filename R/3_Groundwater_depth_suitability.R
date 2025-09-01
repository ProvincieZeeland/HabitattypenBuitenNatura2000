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
### Script 3: Groundwater depth suitability

### Calculating suitability for each habitat type in terms of groundwater depth
### Based on abiotic requirements from N2000 profile documents,
### and expected GVG map made in script 2

## load in expected GVG
GVG <- stars::read_stars('sources/groundwater_depth/expected_gvg.tif')

plot(GVG)

## import categories for groundwater depth from access database
accessconn <- dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=sources/VereistenHabitattypenDec2008.mdb;")
GVG_habtypes <- dbReadTable(accessconn, "VochtklasseHabtype_Juni2008")
dbDisconnect(accessconn)
## remove columns with habitatID, kragge, drijftil, opmerkingen and bespreekgeval
GVG_habtypes <- GVG_habtypes[,-c(2, 13:16)]

## import ranges for GVG from excel, sourced from Runhaar et al., 2009
GVG_ranges <- read_excel("sources/groundwater_depth/GVG_ranges.xlsx")

## function to link GVG categories to GVG values
get_limits_GVG <- function(habitat_code) {
  ## filter data on selected habitat type
  habitat_row <- GVG_habtypes[GVG_habtypes$HabitatCode == habitat_code, ]
  
  ## stop if habitat type is not found
  if (nrow(habitat_row) == 0) {
    stop("Habitat type not found")
  }
  
  ## all categories that have K (or a variation of K). This is the core range.
  relevant_categories_K <- colnames(GVG_habtypes)[-1][!is.na(habitat_row[1, -1]) & habitat_row[1, -1] %in% c("K", "Ka", "Kb")] #wellicht alleen K overlaten
  
  ## designate upper and lower limits
  lower_limit_K <- Inf
  upper_limit_K <- -Inf
  
  ## loop through categories and find the accompanying GVG ranges
  for (category in relevant_categories_K) {
    GVG_range <- GVG_ranges[GVG_ranges$Klasse == category, ]
    
    ## stop if the categorie does not exist in the GVG ranges file
    if (nrow(GVG_range) == 0) {
      stop(paste("Geen GVG range gedefinieerd voor categorie:", category))
    }
    
    ## update upper and lower limits
    lower_limit_K <- min(lower_limit_K, GVG_range$Ondergrens, na.rm = TRUE)
    upper_limit_K <- max(upper_limit_K, GVG_range$Bovengrens, na.rm = TRUE)
  }
  
  ## all categories that have A (or a variation of A). This is the supplementary range.
  relevant_categories_A <- colnames(GVG_habtypes)[-1][!is.na(habitat_row[1, -1]) & habitat_row[1, -1] %in% c("A", "Aa", "Ab")]
  
  ## designate upper and lower limits
  lower_limit_A <- Inf
  upper_limit_A <- -Inf
  
  ## loop through categories and find the accompanying GVG ranges
  for (category in relevant_categories_A) {
    GVG_range <- GVG_ranges[GVG_ranges$Klasse == category, ]
    
    ## stop if the categorie does not exist in the GVG ranges file
    if (nrow(GVG_range) == 0) {
      stop(paste("Geen GVG range gedefinieerd voor categorie:", category))
    }
    
    ## update upper and lower limits
    lower_limit_A <- min(lower_limit_A, GVG_range$Ondergrens, na.rm = TRUE)
    upper_limit_A <- max(upper_limit_A, GVG_range$Bovengrens, na.rm = TRUE)
  }
  
  return(list(lowerA = lower_limit_A, upperA = upper_limit_A, lowerK = lower_limit_K, upperK = upper_limit_K))
}

## List all habitat types
habitat_codes <- unique(GVG_habtypes$HabitatCode)

## Make a dataframe to store all GVG ranges
limits_df <- data.frame(habitat_code = character(),
                        lowerA = numeric(),
                        lowerK = numeric(),
                        upperK = numeric(),
                        upperA = numeric(),
                        stringsAsFactors = FALSE)

## loop through all habitat types and put the reference values in the dataframe
for (habitat_code in habitat_codes) {
  ## get limits for K and A
  getlimits <- get_limits_GVG(habitat_code)
  
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
    lower0 = lowerA - (upperA - lowerA) * 0.1 , ## lower0 is 10% lower than the whole range
    upper0 = upperA + (upperA - lowerA) * 0.1, ## upper0 is 10% higher than the whole range
    predictor   = "GVG"
  ) %>%
  dplyr::select(habitattype, predictor, lower0, lowerA, lowerK, upperK, upperA, upper0)
## please note that upper means a lower groundwater level, and lower a higher groundwater level (because it is groundwater depth)

## Make a suitability map by looping through all habitat types
teller <- 0
tot_aantal_kaarten <- length(unique(referentie_waarden$habitattype))

for (i in unique(referentie_waarden$habitattype)) {
  
  ## make a function to translate a given GVG to a value between 0 and 1, based on reference
  suitability_functie_GVG <-
    approxfun(x = referentie_waarden %>% filter(habitattype ==  i, predictor == 'GVG') %>%
                dplyr::select(lower0,
                              lowerA,
                              lowerK,
                              upperK,
                              upperA,
                              upper0) %>% unlist, 
              y = c(0, .5, 1, 1, .5, 0),    ## suitabilitty score for each range (A, K and 0)
              yleft = 0,    ## which values left of this range?
              yright = 0)     ## which values right of this range?
  
  ## add suitability values to the map
  GVG_suitability_kaart <- 
    st_apply(GVG, c('x','y'), suitability_functie_GVG)
  
  ## change raster to vector
  GVG_suitability_kaart_polygon <- st_as_sf(GVG_suitability_kaart)
  
  ## write to geopackage (vector)
  st_write(dsn = paste0("outputs/outputmaps_gvg/suitability_gvg_", i, ".gpkg"),
           obj = GVG_suitability_kaart_polygon)

  ## increase count by one
  teller <- teller + 1
  
  ## give a notificationwhen one is ready
  print(paste0("suitability_gvg_ ", i, ".gpkg created successfully (", teller, "/", tot_aantal_kaarten, ")."))
}

