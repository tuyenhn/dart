#' Calculate maximum distance from centroid of geometry feature to its
#' boundaries
#'
#' @param geo_feature geometry feature (polygon with coordinates)
#' @param centroid centroid information column
#'
#' @return A "units"
#' @export
#'
#' @examples
#' \dontrun{
#' # read in gson vector
#' hcmc_gson <- st_read(
#'     readChar(gson_fname, file.info(gson_fname)$size),
#'     quiet = TRUE
#' )
#'
#' feature_with_centroid <- centroid_with_crs(hcmc_gson[1, ], 9210)
#' get_max_dist(feature_with_centroid, feature_with_centroid$centroid)
#' }
dart_max_dist <- function(geo_feature, centroid) {
    dist <- geo_feature %>%
        sf::st_cast("POINT", warn = FALSE) %>%
        sf::st_distance(centroid)

    max(dist)
}
