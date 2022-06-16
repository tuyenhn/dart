library(sf)

#' @export
dart_centroid <- function(sf, target_crs) {
    # orig_name: centroid_with_crs
    sf %>% dplyr::mutate(
        centroid = st_transform(sf, st_crs(target_crs)) %>%
            st_geometry() %>%
            st_centroid() %>%
            st_transform(st_crs(4326))
    )
}
