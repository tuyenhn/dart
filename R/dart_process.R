#' Calculate average of raster pixels in a buffer for each geometry
#' feature of an sf object
#'
#' @param sf sf object
#' @param raster stars object
#' @param id_col name of column in `sf` that holds features unique ids
#' @param sf_crs projection CRS that will be used to calculate (default: 4326)
#' @param null_sentinel value representing null in raster (default: -9999)
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' if (rlang::is_installed("geodata") &&
#'     rlang::is_installed("chirps") &&
#'     rlang::is_installed("stars")) {
#'     vnm_lvl3 <- geodata::gadm("VNM", level = 3, path = "vnm_lvl3")
#'     hcmc_lvl3 <- vnm_lvl3[vnm_lvl3$NAME_1 == "Hồ Chí Minh", ]
#'     hcmc_lvl3_sf <- sf::st_as_sf(hcmc_lvl3)
#'
#'     # read raster
#'     raster <- chirps::get_chirps(
#'         hcmc_lvl3,
#'         c("2022-01-01", "2022-01-03"),
#'         server = "CHC",
#'         as.raster = TRUE
#'     )
#'     raster <- stars::st_as_stars(raster)
#'
#'     avg_df <- dart_process(hcmc_lvl3_sf, raster, "GID_3", 9210)
#'
#'     # cleanup
#'     unlink("vnm_lvl3", recursive = TRUE)
#' }
dart_process <- function(sf, raster, id_col, sf_crs = 4326, null_sentinel = -9999) {
    if (!inherits(sf, "sf")) {
        stop("sf must be of class `sf`", call. = FALSE)
    }
    if (!inherits(raster, "stars")) {
        stop("raster must be of class `stars`", call. = FALSE)
    }
    if (!(id_col %in% colnames(sf))) {
        stop("id_col must be a column name in `sf`", call. = FALSE)
    }
    if (!is.numeric(sf_crs)) {
        stop("sf_crs must be a number", call. = FALSE)
    }
    if (!is.numeric(null_sentinel) != "numeric") {
        stop("null_sentinel must be a number", call. = FALSE)
    }
    dart_process_(
        sf = sf,
        raster = raster,
        id_col = id_col,
        sf_crs = sf_crs,
        null_sentinel = null_sentinel
    )
}


dart_process_ <- function(sf, raster, id_col, sf_crs, null_sentinel) {
    # get centroids for each commune/ward
    sf <- dart_centroid(sf, sf_crs)

    # create bounding box around inputted `sf` object
    sf_bbox <- sf::st_bbox(sf)

    # crop the raster to the bouding box
    cropped_raster <- sf::st_crop(raster, sf_bbox)

    # convert cropped raster to sf
    r_sf <- sf::st_as_sf(cropped_raster)

    # create a temporary `sf` with only id and centroid
    temp_sf <- sf[, c(id_col, "centroid")]

    # spatially join raster and buffer (only take pixels inside the buffers)
    df_sf <- sf::st_join(r_sf, temp_sf) %>%
        stats::na.omit() %>%
            dplyr::rename(val = colnames(.)[1])

    # dropping nulls
    df_sf <- df_sf[!(df_sf$val == null_sentinel), ]

    # calculate distances between centroid and pixel centroid for each
    # geometry feature
    df_sf$r_centroid <- dart_centroid(df_sf["geometry"], sf_crs)$centroid
    df_sf$cent_dist <- sf::st_distance(
        df_sf$centroid, df_sf$r_centroid,
        by_element = TRUE
    ) %>% units::drop_units()

    df_sf %>%
        by(
            dplyr::pull(df_sf, id_col),
            FUN = function(x) {
                sum((x$val) / (x$cent_dist)) / sum(1 / (x$cent_dist))
            }
        ) %>%
        unclass() %>%
        as.data.frame()
}
