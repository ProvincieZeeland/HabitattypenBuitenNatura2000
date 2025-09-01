library(raster)
library(stars)
library(lmtest)

### Niels van Hof, nielsvanhof@proton.me
### Internship Project "Habitat types outside of Natura 2000"
### Provincie Zeeland, Radboud University
### Script 5: mean salinity 0-250m
### Please note that the previous script (4) was written in Python rather than R

### Assessing the difference in salinity between layers
### And taking the mean of the top 5 salinity layers 
### Using salinity layers from Delsman et al., 2018, altered by Tim Dubbeldam


## load in data
Cl0_50    <- raster('sources/groundwater_salinity/chloride_0tot-50.tif')
Cl51_100  <- raster('sources/groundwater_salinity/chloride_-100tot-51.tif')
Cl101_150 <- raster('sources/groundwater_salinity/chloride_-150tot-101.tif')
Cl151_200 <- raster('sources/groundwater_salinity/chloride_-200tot-151.tif')
Cl201_250 <- raster('sources/groundwater_salinity/chloride_-250tot-201.tif')
Cl251_300 <- raster('sources/groundwater_salinity/chloride_-300tot-251.tif')

## fit a linear model, with 0-50, 51-100 101-150 en 151-200 depth
LMCl1 <- lm(getValues(Cl0_50) ~ getValues(Cl51_100)+getValues(Cl101_150)+getValues(Cl151_200))

## visual model diagnostics
par(mfrow = c(2, 2))    # Plot in a 2x2 grid
plot(LMCl1, which = 1)    # Residuals vs fitted
plot(LMCl1, which = 2)    # use plot(LM, which = 2) in case of errors #qqPlot(LM1, reps = 1e4)
plot(LMCl1, which = 3)    # Scale-location
plot(LMCl1, which = 5)    # Cook's distance vs leverage
par(mfrow = c(1, 1))    # Restore the default  

# residuals vs fitted is good, so linearity is OK
# qqplot is a little dubious, so do a shapiro test
shapiro.test(sample(resid(LMCl1),5000))
# p-value = 2.2e-16, so not normally distributed
# scale-location is good, so constant variance
# residuals vs leverage: not problematic outliers

summary(LMCl1)
## strong linear relationship (R = 0.982). When the three deeper layers are 0, 0-50cm is about 18.
## given the very high values of mg/l, that is basically the same. Also the slopes are approximately 1.


## try the same model with 201-250cm depth added
LMCl2 <- lm(getValues(Cl0_50) ~ getValues(Cl51_100)+getValues(Cl101_150)+getValues(Cl151_200)+getValues(Cl201_250))

## visual model diagnostics
par(mfrow = c(2, 2))    # Plot in a 2x2 grid
plot(LMCl2, which = 1)    # Residuals vs fitted
plot(LMCl2, which = 2)    # use plot(LM, which = 2) in case of errors #qqPlot(LM1, reps = 1e4)
plot(LMCl2, which = 3)    # Scale-location
plot(LMCl2, which = 5)    # Cook's distance vs leverage
par(mfrow = c(1, 1))    # Restore the default  

# residuals vs fitted is good, so linearity is OK
# qqplot is a little dubious, so do a shapiro test
shapiro.test(sample(resid(LMCl2),5000))
# p-value = 2.2e-16, so not normally distributed
# scale-location is good, so constant variance
# residuals vs leverage: not problematic outliers

summary(LMCl2)

## add 251-250cm
LMCl3 <- lm(getValues(Cl0_50) ~ getValues(Cl51_100)+getValues(Cl101_150)+getValues(Cl151_200)+getValues(Cl201_250)+getValues(Cl251_300))

## visual model diagnostics
par(mfrow = c(2, 2))    # Plot in a 2x2 grid
plot(LMCl3, which = 1)    # Residuals vs fitted
plot(LMCl3, which = 2)    # use plot(LM, which = 2) in case of errors #qqPlot(LM1, reps = 1e4)
plot(LMCl3, which = 3)    # Scale-location
plot(LMCl3, which = 5)    # Cook's distance vs leverage
par(mfrow = c(1, 1))    # Restore the default  

# residuals vs fitted is good, so linearity is OK
# qqplot is a little dubious, so do a shapiro test
shapiro.test(sample(resid(LMCl1),5000))
# p-value = 2.2e-16, so not normally distributed
# scale-location is good, so constant variance
# residuals vs leverage: not problematic outliers

summary(LMCl3)

## they are all very similar
## so take the mean to combine the top 5 layers
Cl0_250 <- calc(stack(Cl0_50, Cl51_100, Cl101_150, Cl151_200, Cl201_250), fun = mean, na.rm = T)
## change crs
crs(Cl0_250) <- "EPSG:28992"

## write to .tif
writeRaster(Cl0_250, filename = "sources/groundwater_salinity/chloride_0tot-250_mean.tif")

