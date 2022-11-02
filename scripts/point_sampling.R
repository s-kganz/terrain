library(tidyverse)
library(raster)
library(ambient)
library(foreach)
library(doParallel)

source("scripts/util.R")

size <- 1001 # in px
w    <- 1 / size

# consistency
set.seed(1234)
# add 1 to force positive values
my_perlin <- (noise_perlin(c(size, size))+1) %>%
  raster()
plot(my_perlin)

# Goal: repeatedly sample an 8-neighborhood of points exactly on top of raster
# cells while gradually increasing the spacing
dx <- rep(c(-1, 0, 1), 3)
dy <- c(rep(1, 3), rep(0, 3), rep(-1, 3))
center_x <- 0.5
center_y <- 0.5

step <- 50 # number of px away from center

x <- center_x + (dx * step * w)
y <- center_y + (dy * step * w)

# Check that the points are in the right spot by plotting them
my_points <- SpatialPoints(cbind(x, y))
#plot(my_perlin)
#points(my_points)

# Sample Z values, then calculate the terrain metrics
z_values <- extract(my_perlin, cbind(x, y))
metrics <- get_terrain_metrics(z_values, w=step*w)

render_evans_young_fit(z_values)

# Now repeat the above, up to a max neighborhood of 500px
metrics_df <- foreach(step=1:(size%/%2), .combine=rbind) %do% {
  x <- center_x + (dx * step * w)
  y <- center_y + (dy * step * w)
  
  metrics <- get_terrain_metrics(
    raster::extract(my_perlin, cbind(x, y)),
    w=step*w
  )
  
  metrics["step"] <- step
  
  unlist(metrics)
} %>% data.frame()

rownames(metrics_df) <- NULL

metrics_df %>%
  pivot_longer(-step, names_to="metric", values_to="value") %>%
  ggplot(aes(x=step, y=value)) + geom_line() +
  facet_wrap(~ metric, scales="free_y")
