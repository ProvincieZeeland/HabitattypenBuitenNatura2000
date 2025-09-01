library(sf)
library(beepr)
library(dplyr)
library(stars) ## maybe not useful anymore
library(raster)
library(exactextractr) ## to extract all intersecting gridcells
library(tidyr)
library(ggplot2)
library(cowplot)
library(randomForest)
library(readxl)
library(caret) ## to make data handling easier for making subsets
library(pROC) ## for ROC curve

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 9: Random forest

### Combining the 4 suitability maps from all previous scripts,
### training a random forest on these maps and the current habitat type map
### and making predictions of habitat type suitability for 19 habitat types in Zeeland


## import N2000 habtypes distribution
N2000 <- st_read("sources/overzicht_N2000_habtypes_ZLD.gpkg") %>%
  dplyr::select(hab_types, geom) ## the rest of the information is not relevant
## import N2000 goals
N2000_goals <- read_excel("sources/Natura2000_doelstellingen_ZLD_20250318_Niels.xlsx", sheet = "Alle doelstellingen") %>%
  dplyr::select(Habitatcode) ## the rest of the information is not relevant

## RWS has denoted some H2160 polygons to either H2160A or H2160B. Change these to H21660
N2000 <- N2000 %>%
  mutate(hab_types = case_when(
    hab_types == "H2160A" ~ "H2160",
    hab_types == "H2160B" ~ "H2160",
    TRUE ~ hab_types  # Keep the original value if no condition is met
  ))

## make a list of all habitattypes present/designated in Zeeland
habtypeslist <- bind_rows(
  N2000 %>% dplyr::select(habitattype = hab_types),
  N2000_goals %>% dplyr::select(habitattype = Habitatcode)
  ) %>%
  distinct(habitattype) %>%
  filter(habitattype != "H0000", ## remove H0000
         !startsWith(habitattype, "H11"),
         !startsWith(habitattype, "H12"), ## remove marine habitats
         !is.na(habitattype)) %>% ## remove NA
  arrange(habitattype) %>% ## arrange in a logical order
  pull(habitattype)

## for loop to loop through all habitattypes and combine the 4 maps
for (habitattype in habtypeslist) {
  ## load the 4 layers of abiotic suitabilities
  GVG <- st_read(paste0("outputs/outputmaps_gvg/suitability_gvg_", habitattype, ".gpkg"))
  pH <- st_read(paste0("outputs/outputmaps_pH/suitability_pH_", habitattype, ".gpkg"))
  Cl <- st_read(paste0("outputs/outputmaps_salinity/suitability_salinity_", habitattype, ".gpkg"))
  Species <- st_read(paste0("outputs/outputmaps_species/suitability_species_", habitattype, ".gpkg"))

  ## join N2000 with species (for the species' gridcells). Keep in mind that there are multiple polygons (in the same place) being created if there are multiple habtypes
  joined <- st_join(Species, N2000, join = st_intersects)

  ## Make a separate column for presence/absence of the habitattype in question and remove double gridcells
  joined <- joined %>%
    mutate(presence = ifelse(hab_types == habitattype, "Present", "Absent")) %>% ## presence is presence of the habitat type
    group_by(grid_cell_id) %>%
    summarise(presence = case_when(
      any(presence == "Present") ~ "Present", ## if there is any instance of the gridcell with present, return present
      any(presence == "Absent") ~ "Absent",   ## if there is no instance of present but there is absent, return absent
      TRUE ~ NA_character_                  ## if there is none of the two present, return NA
    ), 
    across(everything(), first),
    .groups = 'drop') %>%
    dplyr::select(-hab_types) %>%
    mutate(presence = factor(presence, levels = c("Present", "Absent"))) ## turn the presence into a factor
  
  ## Check the number of "Present" cells
  present_count <- sum(joined$presence == "Present", na.rm = TRUE)
  
  ## If there are less than 8 "Present" cells, skip to the next habitattype. Habitat types with less than 8 occurrence were not used
  if (present_count < 8) {
    message(paste0("Skipping ", habitattype, " due to insufficient 'Present' cells: ", present_count))
    next
  }

  ## join all abiotic variables to the gridcells
  join_pH <- st_join(joined, pH, join = st_intersects)
  join_GVG <- st_join(joined, GVG, join = st_intersects)
  join_Cl <- st_join(joined, Cl, join = st_intersects)

  ## calculate mean pH for every gridcell
  mean_pH <- join_pH %>%
    group_by(grid_cell_id) %>%  ## group the dataframe by grid_cell_id
    summarise(suitability_pH = mean(suitability_functie, na.rm = TRUE)) %>% ## calculate the mean for pH suitability for every gridcell, suitability_functie is pH suitability (in later outputs this is suitability_functie_pH)
    st_drop_geometry() ## drop the geometry in order to perform left join later

  ## calculate mean GVG for every gridcell
  mean_GVG <- join_GVG %>%
    group_by(grid_cell_id) %>%  ## group the dataframe by grid_cell_id
    summarise(suitability_GVG = mean(suitability_functie_GVG, na.rm = TRUE)) %>% ## calculate the mean for pH suitability for every gridcell
    st_drop_geometry() ## drop the geometry in order to perform left join later

  ## calculate mean Cl for every gridcell
  mean_Cl <- join_Cl %>%
    group_by(grid_cell_id) %>%  ## group the dataframe by grid_cell_id
    summarise(suitability_Cl = mean(suitability_functie_Cl, na.rm = TRUE)) %>% ## calculate the mean for pH suitability for every gridcell
    st_drop_geometry() ## drop the geometry in order to perform left join later

  ## join the mean  values to the joined dataset
  joined <- joined %>%
    left_join(mean_pH, by = "grid_cell_id") %>%
    left_join(mean_GVG, by = "grid_cell_id") %>%
    left_join(mean_Cl, by = "grid_cell_id") %>%
    dplyr::select(-aantal_aanwezig) ## drop aantal aanwezig column

  st_write(joined, paste0("outputs/outputmaps_joined/", habitattype, "_joined.gpkg"))

} 

## random forest approach. manually input the habitat type below.
## no for loop because we need to check what the optimal amount of trees and
## variables to try at each node is
habitattype = "H2130A"

## load in joined map
joined <- st_read(paste0("outputmaps_joined/", habitattype, "_joined.gpkg"))

## turn the presence into a factor
joined$presence <- factor(joined$presence, levels = c("Present", "Absent"))

## remove the rows that have missing data for the suitabilities. They cannot be used.
joined.clean <- joined %>%
  drop_na(suitability_species,
          suitability_pH,
          suitability_GVG,
          suitability_Cl)

## remove the rows that have not been surveyed (i.e. not in N2000 areas). The surveyed data will be used for training and validation
joined.surveyed <- joined.clean %>%
  drop_na(presence)

## custom undersampling to balance the data (as much present as absent)
joined.surveyed.balanced <- joined.surveyed %>%
  filter(presence == "Present") %>%  ## keep all instances of the present class
  bind_rows(
    joined.surveyed %>%
      filter(presence == "Absent") %>%
      sample_n(size = sum(joined.surveyed$presence == "Present"), replace = FALSE)  ## sample as much from absent as there are present
  )

## make a subset to train and to validate
trainIndex <- createDataPartition(joined.surveyed.balanced$presence, p = .75, ## 75% is training, 25% validation
                                  list = FALSE, 
                                  times = 1)
joined.train <- joined.surveyed.balanced[trainIndex, ]
joined.test <- joined.surveyed.balanced[-trainIndex, ]

## make the random forest model, first test with 1000 trees
rfmodel <- randomForest(presence ~ suitability_species + suitability_pH + suitability_GVG + suitability_Cl, data = joined.train, ntree = 1000, proximity = TRUE)

rfmodel

## plot the error rate as a function of the amount of trees, to find out the optimal amount of trees
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rfmodel$err.rate), times=3),
  Type=rep(c("OOB", "Present", "Absent"), each=nrow(rfmodel$err.rate)),
  Error=c(rfmodel$err.rate[,"OOB"], 
          rfmodel$err.rate[,"Present"], 
          rfmodel$err.rate[,"Absent"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## put in the optimal amount of trees, based on the plot
ntree <- 50 ## I have now put 50, but check for each separate habitat type!

## next, let's check if we are considering the optimal number of variables at each internal node (default is 2)
oob.values.mtry <- vector(length = 4) ## create an empty vector that can hold 4 values
for(i in 1:4) { ## make a loop that tests different numbers of variables at each step
  temp.model <- randomForest(presence ~ suitability_species + suitability_pH + suitability_GVG + suitability_Cl, data = joined.train, mtry = i, ntree = ntree) ## mtry is the amount of variables at each node
  oob.values.mtry[i] <- temp.model$err.rate[nrow(temp.model$err.rate), 1] ## take the last value in the first column, the OOB error rate when all 1000 trees have been made.
}

oob.values.mtry

## put in the optimal value of mtry below:
mtry <- which.min(oob.values.mtry)

## I found out that nodesize 1 is always the best
nodesize <- 1

## so we make a random forest with the above optimal values
rfmodel <- randomForest(presence ~ suitability_species + suitability_pH + suitability_GVG + suitability_Cl, data = joined.train, ntree = ntree, nodesize = nodesize, mtry = mtry, proximity = TRUE)

rfmodel

## make predictions on the test dataset with the train dataset
predicted_probs <- predict(rfmodel, joined.test, type = "prob")[, "Present"]
predicted_classes <- predict(rfmodel, joined.test) ## in predicted classes, all probabilities 0.5 and higher for present are given the category "present"

## make a confusion matrix to evaluate the model's performance
confusion_matrix <- confusionMatrix(predicted_classes, joined.test$presence)
confusion_matrix_output <- capture.output(confusion_matrix)

## capture precision and recall from confusion matrix
precision <- confusion_matrix$byClass['Precision']
recall <- confusion_matrix$byClass['Recall']

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

## calculate relative importances
importance_raw <- varImp(rfmodel)/sum(varImp(rfmodel))

importance <- paste(rownames(importance_raw), ": ", round(importance_raw[, 1], 4))

## make ROC plot
roc_curve <- roc(joined.test$presence, predicted_probs)

## save the ROC for later use
saveRDS(roc_curve, paste0("outputs/outputmaps_predictions/ROC_curves/", habitattype, "_ROC.rds"))
## load the ROC
#roc_curve <- readRDS(paste0("outputs/outputmaps_predictions/ROC_curves/", habitattype, "_ROC.rds"))

## calculate AUC
auc_value <- auc(roc_curve)

## plot the ROC in Rstudio interface
plot(roc_curve, 
     main = paste("ROC curve", habitattype), 
     col = "blue", 
     lwd = 2)
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), col = "black")

## Save the plot as png file
png(paste0("outputs/outputmaps_predictions/ROC_curves/", habitattype, "_ROC.png"),
           width = 800, height = 600)  # Specify the file name and dimensions

plot(roc_curve, 
     main = paste("ROC curve", habitattype), 
     col = "blue", 
     lwd = 2)
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), col = "black")

## close the graphics device
dev.off()

## make a summary of all statistics
statsoutput <- c(paste("Statistics for Random Forest model made for habitattype", habitattype),
                 paste("Number of trees:", ntree),
                 paste("No. of variables tried at each split:", mtry),
                 paste("Nodesize:", nodesize),
                 paste("Amount of presences / absences used for training:", sum(joined.train$presence == "Present", na.rm = TRUE),"/", sum(joined.train$presence == "Absent", na.rm = TRUE)),
                 paste(""),
                 "Statistics when applying model on separate testing set:",
                 confusion_matrix_output,
                 paste("F1 Score:", round(f1_score, 4)),
                 paste("AUC:", round(auc_value, 4)),
                 paste(""),
                 paste("Relative importance of predictors:"),
                 importance)

## save this summary to a text file
## in the future I'd recommend saving all the statistics values to csv as well, because now I manually typed them into Excel
writeLines(statsoutput, paste0("outputs/outputmaps_predictions/", habitattype, "_statistics.txt"))

## use the random forest to predict presence/absence on the whole map
joined.clean$predicted_probs <- predict(rfmodel, joined.clean, type = "prob")[, "Present"]
joined.clean$predicted_classes <- predict(rfmodel, joined.clean)

## save the habitat type suitability map as a geopackage
st_write(joined.clean, paste0("outputs/outputmaps_predictions/", habitattype, "_predicted.gpkg"))
