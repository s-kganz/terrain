library(tidyverse)
library(raster)
library(ambient)
library(foreach)

source("scripts/util.R")

size <- 1001
w    <- 1/size

set.seed(1234)
perlin <- noise_perlin(c(size, size)) %>% raster()
center <- 0.5 # extent is just 0-1 on both axes

# As the raster is resampled to coarser and coarser scales, does resampling
# yield different terrain metrics than point sampling from the original
# raster?

metrics <- foreach(newsize=seq(1001, 3, by=-2), .combine=rbind) %do% {
  template <- raster(nrows=newsize, ncols=newsize,
                     xmn=0, xmx=1, ymn=0, ymx=1)
  resampled <- raster::resample(perlin, template)
  
  # The resampled pixel centers should align *exactly* with these points, when
  # we sample the unresampled raster these snap to the nearest px.
  w_new <- 1/newsize
  dx <- rep(c(-1, 0, 1), 3)
  dy <- c(rep(1, 3), rep(0, 3), rep(-1, 3))
  x <- center + (dx * (w_new))
  y <- center + (dy * (w_new))
  
  # now sample terrain metrics from both rasters
  metrics_resample <- get_terrain_metrics(
    raster::extract(resampled, cbind(x, y)),
    w=w_new
  )
  metrics_resample["resampled"] <- 1
  metrics_resample["scale"]  <- w_new
  
  metrics_original <- get_terrain_metrics(
    raster::extract(perlin, cbind(x, y)),
    w=w_new
  )
  metrics_original["resampled"] <- 0
  metrics_original["scale"]  <- w_new
  
  rbind(
    unlist(metrics_resample), unlist(metrics_original)
  )
}
metrics <- as.data.frame(metrics)

metrics %>%
  pivot_longer(-c(resampled, scale)) %>%
  mutate(resample_factor = scale / w) %>%
  filter(scale < 0.1) %>%
  ggplot(aes(x=resample_factor)) + 
  geom_line(aes(y=value, group=resampled, color=as.logical(resampled))) +
  facet_wrap(~ name, scales="free_y") +
  labs(color="Raster resampled?",
       x="Resample factor") +
  scale_x_log10()
