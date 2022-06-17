#' Calculate maximum distance from centroid of geometry feature to its
#' boundaries
#'
#' @param geo_feature geometry feature (polygon with coordinates)
#' @param with_centroid centroid information is included with geo_feature
#'                      (default: FALSE)
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
#' # geo features without centroid
#' get_max_dist(hcmc_gson[1, ])
#'
#' # geo features comes with centroid
#' feature_with_centroid <- centroid_with_crs(hcmc_gson[1, ], 9210)
#' get_max_dist(feature_with_centroid, with_centroid = TRUE)
#' }
dart_max_dist <- function(geo_feature, with_centroid = FALSE) {
    if (!with_centroid) {
        geo_feature <- dart_centroid(geo_feature, 9210)
    }

    dist <- geo_feature %>%
        sf::st_cast("POINT", warn = FALSE) %>%
        sf::st_distance(.$centroid)

    max(dist)
}
