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
    if (!class(geo_feature)[1] == "sf") {
        stop("geo_feature must be of class `sf`", call. = FALSE)
    }
    if (!class(centroid)[1] == "sfc_POINT") {
        stop("centroid must be of class `sfc_POINT`", call. = FALSE)
    }

    dart_max_dist_(
        geo_feature = geo_feature,
        centroid = centroid
    )
}

dart_max_dist_ <- function(geo_feature, centroid) {
    geo_feature %>%
        sf::st_cast("POINT", warn = FALSE) %>%
        sf::st_distance(centroid) %>%
        max()
}
