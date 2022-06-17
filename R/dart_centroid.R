#' sf::st_centroid() wrapper with optional CRS transformation
#'
#' @param sf sf object
#' @param target_crs CRS which the centroid calculation will be performed
#'                   with (optional)
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
dart_centroid <- function(sf, target_crs = 4326) {
    sf %>% mutate(
        centroid = sf::st_transform(sf, sf::st_crs(target_crs)) %>%
            sf::st_geometry() %>%
            sf::st_centroid() %>%
            sf::st_transform(sf::st_crs(4326))
    )
}
