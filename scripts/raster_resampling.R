library(tidyverse)
library(raster)

# Goal: essentially do the same thing as the point sampling script, but resample
# the raster instead and see if the terrain metrics are any different

size <- 1001 # in px
w    <- 1 / size

# consistency
set.seed(1234)
# add 1 to force positive values
my_perlin <- (noise_perlin(c(size, size))+1) %>%
  raster()
plot(my_perlin)

# small_size <- 9
# small <- raster(matrix(data=1:small_size^2, nrow=small_size, ncol=small_size))
# w <- 1/small_size

# when resampling, we do not want the central pixel to move
newsize <- 3 # MUST be odd
w_new <- 1/newsize

template <- raster(nrows=newsize, ncols=newsize,
                   xmn=0, xmx=1, ymn=0, ymx=1)


# the resampled pixels should align *exactly* with these points
dx <- rep(c(-1, 0, 1), 3)
dy <- c(rep(1, 3), rep(0, 3), rep(-1, 3))
center_x <- 0.5
center_y <- 0.5
x <- center_x + (dx * (w_new))
y <- center_y + (dy * (w_new))

resampled <- raster::resample(my_perlin, template)
plot(resampled)
points(cbind(x, y))

# Then just use raster::extract as in the point sampling script
