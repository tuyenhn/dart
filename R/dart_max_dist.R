dart_max_dist <- function(geo_feature, with_centroid = FALSE) {
    if (!with_centroid) {
        geo_feature <- dart_centroid(geo_feature, 9210)
    }

    dist <- geo_feature %>%
        sf::st_cast("POINT", warn = FALSE) %>%
        sf::st_distance(.$centroid)

    max(dist)
}
