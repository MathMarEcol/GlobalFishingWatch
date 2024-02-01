library(spatialplanr)
library(ggplot2)
library(gfwr)
library(spatialgridr)


source("/Users/eve067/GitHub/GlobalFishingWatch/get_gfwData.R")

Region <- "Coral Sea" # "Australia"
Type <- "Oceans" # "EEZ"

Shape <- "Hexagon" # "Shape of PUs
PU_size <- 10000 # km2

cCRS <- "ESRI:54009"

Bndry <- splnr_get_boundary(Region, Type, cCRS)

landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  sf::st_transform(cCRS)

PUs <- splnr_get_planningUnits(Bndry, landmass, PU_size, Shape)

gfw_data <- get_gfwData('Australia', "2022-01-01", "2022-12-31", "yearly", cCRS = cCRS, compress = TRUE)

# Try sf::st_interpolate_aw. JDE - Not sure if this is the best option....
gfw_PU <- sf::st_interpolate_aw(gfw_data, PUs, extensive = TRUE)

PUs$Apparent.Fishing.Hours <- 0
PUs$Apparent.Fishing.Hours[as.numeric(rownames(gfw_PU))] <- gfw_PU$Apparent.Fishing.Hours

gg1 <- ggplot() +
  geom_sf(data = PUs, aes(fill = log10(Apparent.Fishing.Hours)))

gg2 <- ggplot() +
  geom_sf(data = gfw_data %>% sf::st_crop(PUs), aes(fill = log10(`Apparent Fishing Hours`),
                                                    colour = log10(`Apparent Fishing Hours`)))

patchwork::wrap_plots(gg1, gg2, ncol = 1)

gfw_data_gridded <- data_to_planning_grid(planning_grid = PUs, dat = gfw_data, name = "FishHrs",
                                          meth = "average",
                                          sf_col_layer_names = "Apparent Fishing Hours")

ggplot(data = gfw_data_gridded) + geom_sf(aes(fill = FishHrs))
