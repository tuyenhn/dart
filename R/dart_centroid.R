#' sf::st_centroid() wrapper with optional CRS transformation
#'
#' @param sf sf object
#' @param target_crs CRS which the centroid calculation will be performed
#'                   with (optional)
#' @param output_crs CRS which the function will return
#'
#' @return original sf object with new centroid column
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
#' hcmc_gson_w_centroid <- dart_centroid(hcmc_gson, 9210)
#' }
dart_centroid <- function(sf, target_crs = 4326, output_crs = 4326) {
    if (!class(sf)[1] == "sf") {
        stop("sf must be of class `sf`", call. = FALSE)
    }
    if (!is.numeric(target_crs)) {
        stop("target_crs must be a number", call. = FALSE)
    }
    if (!is.numeric(output_crs)) {
        stop("output_crs must be a number", call. = FALSE)
    }

    dart_centroid_(
        sf = sf,
        target_crs = target_crs,
        output_crs = output_crs
    )
}

dart_centroid_ <- function(sf, target_crs, output_crs) {
    sf$centroid <- sf::st_transform(sf, sf::st_crs(target_crs)) %>%
        sf::st_geometry() %>%
        sf::st_centroid() %>%
        sf::st_transform(sf::st_crs(output_crs))
    sf
}
