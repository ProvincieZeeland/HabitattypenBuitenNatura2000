library(raster)
library(stars)
library(ggplot2)

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 2: estimating GVG based on GHG

### Estimating GVG based on GHG, using a linear model
### Based on GVG and GHG maps from Deltares (unpublished)
### Expected GVG will be used in script 3 to calculate groundwater depth suitability


## Load in GHG, GVG and NBP Zeeland (for extent of nature areas)
GHG = raster('sources/groundwater_depth/ghg-mediaan.tif')
GVG = raster('sources/groundwater_depth/gvg-mediaan.tif')
NBP <- st_read('sources/ext_nat_nbp.gpkg')

## crop GHG and GVG on Zeeland
GHG <- crop(GHG, st_bbox(NBP))
GVG <- crop(GVG, st_bbox(NBP))

## resample GVG to GHG (GVG has less points)
GVG_resamp = round(resample(GVG, GHG, "bilinear"))

plot(GVG_resamp) ## there are still enough locations left to perform a test on

## Make a dataframe of both GVG and GHG values
data <- data.frame(GVG = getValues(GVG_resamp), GHG = getValues(GHG))
data <- na.omit(data)

## make a simple plot to show the linear relationship
plot(getValues(GVG_resamp) ~ getValues(GHG))

## fit a linear model between GVG and GHG
LM1 <- lm(GVG ~ GHG, data = data)

## visual model diagnostics
par(mfrow = c(2, 2))    # Plot in a 2x2 grid
plot(LM1, which = 1)    # Residuals vs fitted
plot(LM1, which = 2)    # use plot(LM, which = 2) in case of errors #qqPlot(LM1, reps = 1e4)
plot(LM1, which = 3)    # Scale-location
plot(LM1, which = 5)    # Cook's distance vs leverage
par(mfrow = c(1, 1))    # Restore the default  
# Residuals vs Fitted: Linearity is OK, 
# Q-Q Residuals: residuals are also somewhat normally distributed. At least there are no gaps
# Scale-Location: residuals have a constant variance
# Residuals vs Leverage: there are a few outliers, but they have a very small leverage. No real problems

## The assumptions are all met, so we can make the regression table
modelsummary <- summary(LM1)
modelsummary

## GVG and GHG are significantly correlated
## When GHG is 0, GVG is approximately 42. When GHG increases by 1, GVG increases by 0.9035

## make a nice looking graph to show the linear relationship

## r squared for in the graph
r_squared <- modelsummary$r.squared

intercept <- coef(LM1)[1]
slope <- coef(LM1)[2]

# Create the formula string for the graph
formula_text <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x")

## Nicer looking graph
gvgplot <- ggplot(data, aes(x = GHG, y = GVG)) +
  geom_point(color = "steelblue", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "#C21807", se = FALSE) +  # Regression line
  labs(x = "Mean highest groundwater depth \n GHG (metres from ground level)",
       y = "Mean spring groundwater depth \n GVG (metres from ground level)") +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared, 3), "\n", formula_text), 
           hjust = 1.5, vjust = 6, size = 4, color = "#4d4d4d") +  # R-squared and formula annotation
  theme_bw() +  # Clean theme
  theme( axis.title.x = element_text(size = 14, colour = "#4d4d4d"),
         axis.title.y = element_text(size = 14, colour = "#4d4d4d"),
         axis.text = element_text(size = 11),
         panel.grid.major = element_line(linewidth = 0.8), # Style for major grid lines
         panel.grid.minor = element_line(linewidth = 0.8)) + # Style for minor grid lines) 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 254)) +  ## Set x-axis limits and remove space
  scale_y_continuous(expand = c(0, 0), limits = c(0, 254))    ## Set y-axis limits and remove 
gvgplot

ggsave("outputs/report_graphs/gvgvsghg.png",
       plot = gvgplot, units = "in",
       width = 5.7, height = 4.00, dpi = 1000)

## function to predict GVG based on GHG
predict_GVG <- function(GHG) {
  intercept <- coef(LM1)[1]
  coefficient <- coef(LM1)[2]
  return(intercept + coefficient * GHG)
}

## use function to make predictions
expected_GVG <- calc(GHG, predict_GVG)

plot(expected_GVG)

## combine GVG and expected GVG
## first remove the cells where GVG and expected GVG overlap
masked_expected_GVG <- mask(expected_GVG, GVG, inverse = T)

plot(masked_expected_GVG)

## add GVG from places where it is known
combined_expected_GVG <- merge(masked_expected_GVG, GVG)

## add the CRS
combined_expected_GVG <- projectRaster(combined_expected_GVG, crs = 28992)

plot(combined_expected_GVG)

## write expectd GVG to .tif to use in further analysis
writeRaster(combined_expected_GVG, filename = "sources/groundwater_depth/expected_gvg.tif")
