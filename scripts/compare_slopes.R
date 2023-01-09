library(tidyverse)
library(raster)
library(ambient)
library(spatialEco)
library(OpenImageR)

source("scripts/util.R")

size <- 100
w    <- 1/size

set.seed(5678)
perlin <- noise_perlin(c(size, size)) + 1

perlin %>% raster() %>% plot()

p_kernel <- matrix(c(-1, 0, 1, -1, 0, 1, -1, 0, 1), nrow=3, ncol=3)
q_kernel <- matrix(c(1, 1, 1, 0, 0, 0, -1, -1, -1), nrow=3, ncol=3)

P <- convolution(perlin, p_kernel)
Q <- convolution(perlin, q_kernel)

slope_manual <- atan(sqrt(P^2 + Q^2)) # in rads

# Now compare with the function in raster
perlin_raster <- raster(perlin)
projection(perlin_raster) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
slope_terrain <- terrain(perlin_raster, opt="slope", units="radians")