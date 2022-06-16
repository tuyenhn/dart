library(sf)
source("R/dart_centroid.R")


# get_max_dist
dart_radius <- function(sf) {
    dist <- sf %>%
        st_cast("POINT", warn = FALSE) %>%
        st_distance(centroid_with_crs(sf, 9210)$centroid)

    max(dist)
}
