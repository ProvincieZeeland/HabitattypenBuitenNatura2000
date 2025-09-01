library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(pROC)

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 10: Report graphs

### Making graphs for the statistic values to put into the report

## load in statistics
stats <- read_excel("outputs/outputmaps_predictions/Habitattypes_results_statistics.xlsx")
str(stats)
stats$Used <- factor(stats$Used, levels = c("Yes", "No"))

stats_used <- stats %>% filter(Used == "Yes")

# Reshape the data to long format
stats_long <- stats_used %>%
  pivot_longer(cols = c(Accuracy, Sensitivity, Specificity, AUC), 
               names_to = "Metric", 
               values_to = "Value")

# Reorder the Metric factor levels
stats_long$Metric <- factor(stats_long$Metric, 
                            levels = c("Accuracy", "Sensitivity", "Specificity", "AUC"))

# Create the boxplots for AUC, Accuracy, Sensitivity and Specificity
statbp <- ggplot(data = stats_long, aes(x = Metric, y = Value, fill = Metric)) +
  stat_boxplot(geom = "errorbar",
               width = 0.18,      ## Width of the error bars
               lwd = 0.55,) +     ## line width of the error bar
  geom_boxplot(outlier.shape = NA,## remove outliers
               width = 0.7,       ## Width of the boxplot
               lwd = 0.55,) +     ## line width boxplot
  geom_jitter(width = 0.1, alpha = 0.6, color = "black") +
  scale_fill_manual( values = c("AUC" = "#D2B48C",    
                                "Accuracy" = "#CD853F",  
                                "Sensitivity" = "#FFD700",
                                "Specificity" = "#FFA500")
  ) +
  labs(y = "Value") +
  theme_bw() + ## black and white background theme
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(size = 14, colour = "#4d4d4d"), 
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        panel.grid.major = element_line(linewidth = 0.8), # Style for major grid lines
        panel.grid.minor = element_line(linewidth = 0.8)) # Style for minor grid lines
statbp

## save stats boxplot
ggsave("outputs/report_graphs/statboxplot.png",
       plot = statbp, units = "in",
       width = 5.7, height = 4.00, dpi = 1000)

## barplot with accuracy for every habitattype
accplot <- ggplot(data = stats_used, aes(x = Habitattype, y = Accuracy)) +
  geom_bar(stat="identity", width = 0.7, fill = "steelblue") +
  geom_errorbar(aes(ymin = Accuracy_CI_lower, ymax = Accuracy_CI_higher), 
                width = 0.4, color = "black") +  ## Add error bars
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 14, colour = "#4d4d4d"),
        axis.title.x = element_text(size = 14, colour = "#4d4d4d"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        panel.grid.major = element_line(linewidth = 0.8), # Style for major grid lines
        panel.grid.minor = element_line(linewidth = 0.8)) + # Style for minor grid lines
  labs(x = "Habitat type")
accplot

ggsave("outputs/report_graphs/accuracyplot.png",
       plot = accplot, units = "in",
       width = 6, height = 4.00, dpi = 1000)

## calculate the mean accuracy
mean(stats_used$Accuracy)

## stacked barplot with importance of predictors
## first make long form
stats_long <- stats_used %>%
  pivot_longer(cols = c(imp_species, imp_pH, imp_GVG, imp_Cl), 
               names_to = "importance_variable", 
               values_to = "importance_value")

stats_long$importance_variable <- factor(stats_long$importance_variable, 
                                         levels = c("imp_species", "imp_Cl", "imp_pH", "imp_GVG"))

## Create the stacked bar plot
stackplot <- ggplot(data = stats_long, aes(x = Habitattype, y = importance_value, fill = importance_variable)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("imp_Cl" = "#984ea3",       ## Salinity - Yellow
                               "imp_GVG" = "#377eb8",      ## Groundwater depth - Blue
                               "imp_pH" = "#e31b1b",       ## Soil acidity - Red
                               "imp_species" = "#4daf4a"), ## Species - Green
                    labels = c("imp_Cl" = "Groundwater salinity", 
                               "imp_GVG" = "Groundwater depth", 
                               "imp_pH" = "Soil acidity", 
                               "imp_species" = "Characteristic species")) +  ## Rename legend labels
  labs(x = "Habitat type", y = NULL, fill = "Predictor") + 
  theme_bw() +
  theme(axis.title.y = element_text(size = 14, colour = "#4d4d4d"),
        axis.title.x = element_text(size = 14, colour = "#4d4d4d"),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        panel.grid.major = element_line(linewidth = 0.8), # Style for major grid lines
        panel.grid.minor = element_line(linewidth = 0.8)) # Style for minor grid lines
stackplot

ggsave("outputs/report_graphs/varimportanceplot.png",
       plot = stackplot, units = "in",
       width = 7, height = 4.00, dpi = 1000)

## calculate the mean of the importances
mean(stats_used$imp_species)
mean(stats_used$imp_Cl)
mean(stats_used$imp_pH)
mean(stats_used$imp_GVG)

## make the ROC curves look nicer for the report

## define habitattype
habitattype = "H2130A"

## load in ROC curves
roc_curve <- readRDS(paste0("outputs/outputmaps_predictions/ROC_curves/", habitattype, "_ROC.rds"))

ROCplot <- ggroc(roc_curve, legacy.axes = FALSE, colour = "steelblue", size = 1) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", colour = "red", size = 1) +
  xlab("Specificity") +
  ylab("Sensitivity") +
  annotate("text", x = 0.8, y = 0.2,  # Adjust x and y for placement
           label = paste("AUC =", round(auc(roc_curve), 3)), 
           hjust = -2, vjust = 0, size = 4, color = "#4d4d4d") +
  theme_bw() + 
  theme( axis.title.x = element_text(size = 14, colour = "#4d4d4d"),
         axis.title.y = element_text(size = 14, colour = "#4d4d4d"),
         axis.text = element_text(size = 11),
         panel.grid.major = element_line(linewidth = 0.8), # Style for major grid lines
         panel.grid.minor = element_line(linewidth = 0.8)) # Style for minor grid lines) 
ROCplot  

ggsave(paste0("outputs/report_graphs/ROC/ROC_", habitattype, ".png"),
       plot = ROCplot, units = "in",
       width = 5.7, height = 5.7, dpi = 1000)

## make a table with area and percentage outside of N2000 with a high probability of occurrence for each habitattype.
## this table was eventually not used anymore in the report
library(sf)
library(stars)
library(tidyr)
library(writexl)

## Loop through each habitat code and make the dataframes ready
for (habitat in habtypeslist) {
  # Construct the file path
  file_path <- paste0("outputs/outputmaps_predictions/", habitat, "_predicted.gpkg")
  
  # Read the data
  predicted_data <- st_read(file_path)
  
  # Fix the structure
  predicted_data$presence <- factor(predicted_data$presence, levels = c("Present", "Absent"))
  predicted_data$predicted_classes <- factor(predicted_data$predicted_classes, levels = c("Present", "Absent"))
  
  # Filter on grid cells outside of N2000
  predicted_data_noN2000 <- predicted_data %>% 
    filter(is.na(presence))
  
  # Create a separate dataframe with the habitat code in the name
  assign(paste0(habitat, "_predicted_noN2000"), predicted_data_noN2000, envir = .GlobalEnv)
}

## make a list of all the dataframes
dataframes_list <- lapply(habtypeslist, function(x) get(paste0(x, "_predicted_noN2000")))

## Function to count levels in each dataframe
count_levels <- function(df) {
  counts <- table(df$predicted_classes)
  data.frame(
    predicted_present = counts["Present"],
    predicted_absent = counts["Absent"]
  )
}

## use the function to list all counts
summary_df <- lapply(dataframes_list, count_levels) %>%
  bind_rows(.id = "Habitattype")
summary_df$Habitattype <- habtypeslist

## calculate the average of predicted present gridcells
mean(stats_used$percentage_present)
median(stats_used$percentage_present)


## export to csv
write_xlsx(summary_df, "outputs/gridcells_present_absent_outside_N2K.xlsx")


