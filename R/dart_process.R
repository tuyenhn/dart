#' Calculate average of raster pixels in a buffer for each geometry
#' feature of an sf object
#'
#' @param sf sf object
#' @param raster stars object
#' @param sf_crs projection CRS that will be used to calculate (default: 4326)
#' @param null_sentinel value representing null in raster (default: -9999)
#' @param buffer_rad radius of buffer around the centroid of
#'                   geometry feature (optional)
#'
#' @return A dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' # read in gson vector
#' hcmc_gson <- st_read(
#'     readChar(gson_fname, file.info(gson_fname)$size),
#'     quiet = TRUE
#' )
#' # read raster
#' raster <- stars::read_stars(r_fname)
#'
#' avg_df <- dart_process(hcmc_gson, raster, 9210)
#' }
dart_process <- function(sf,
                         raster,
                         sf_crs = 4326,
                         null_sentinel = -9999,
                         buffer_rad = 0) {
    if (!inherits(sf, "sf")) {
        stop("sf must be of class `sf`", call. = FALSE)
    }
    if (!inherits(raster, "stars")) {
        stop("raster must be of class `stars`", call. = FALSE)
    }
    if (!is.numeric(sf_crs)) {
        stop("sf_crs must be a number", call. = FALSE)
    }
    if (!is.numeric(buffer_rad) != "numeric" || buffer_rad < 0) {
        stop("buffer radius must be a positive number", call. = FALSE)
    }
    dart_process_(
        sf = sf,
        raster = raster,
        sf_crs = sf_crs,
        null_sentinel = null_sentinel,
        buffer_rad = buffer_rad
    )
}


dart_process_ <- function(sf,
                          raster,
                          sf_crs,
                          null_sentinel,
                          buffer_rad) {
    # get centroids for each commune/ward
    sf <- dart_centroid(sf, sf_crs)

    # calculate max buffer radius
    rad <- seq_len(0)
    if (!buffer_rad) {
        for (i in seq_len(nrow(sf))) {
            rad[[i]] <- dart_max_dist(sf[i, ], sf[i, ]$centroid)
        }
        buffer_rad <- max(rad)
    }
    units(buffer_rad) <- as_units("m")
    # TODO: not final solution
    # 13761.42 (hcmc)

    # create buffers around *centroids*
    buffers <- sf::st_buffer(
        sf[seq_len(nrow(sf)), "centroid"],
        dist = buffer_rad
    )

    # create bounding box around buffers
    buffers_bbox <- sf::st_bbox(buffers)

    # crop the raster to the bouding box
    cropped_raster <- sf::st_crop(raster, buffers_bbox)

    # convert cropped raster to sf
    r_sf <- sf::st_as_sf(cropped_raster)

    # convert buffers to sf with centroids and commune/ward ID
    buffers_sf <- buffers %>%
        sf::st_as_sf() %>%
        mutate(centroid = sf$centroid) %>%
        mutate(id = sf$ID_3)

    # spatially join raster and buffer (only take pixels inside the buffers)
    df_sf <- sf::st_join(r_sf, buffers_sf) %>%
        na.omit() %>%
        rename(val = colnames(.)[1])

    # calculate distances between centroid and pixel centroid for each
    # geometry feature
    df_sf$r_centroid <- dart_centroid(df_sf["geometry"], sf_crs)$centroid
    df_sf$cent_dist <- sf::st_distance(
        df_sf$centroid, df_sf$r_centroid,
        by_element = TRUE
    ) %>% drop_units()

    # dropping nulls
    df_sf <- df_sf[!(df_sf$val == null_sentinel), ]

    df_sf %>%
        by(
            .$id,
            FUN = function(x) {
                sum((x$val) / (x$cent_dist)) / sum(1 / (x$cent_dist))
            }
        ) %>%
        unclass() %>%
        as.data.frame()
}
