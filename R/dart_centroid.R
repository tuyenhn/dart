library(sf)

#' @export
dart_centroid <- function(sf, target_crs = 4326) {
    sf %>% mutate(
        centroid = sf::st_transform(sf, sf::st_crs(target_crs)) %>%
            sf::st_geometry() %>%
            sf::st_centroid() %>%
            sf::st_transform(sf::st_crs(4326))
    )
}
