dart_process <- function(sf,
                         raster,
                         null_sentinel = -9999,
                         buffer_rad = 0) {
    if (!(is.data.frame(sf) && class(sf)[1] == "sf")) {
        stop("sf must be of class `sf`", call. = FALSE)
    }
    if (class(raster) != "stars") {
        stop("raster must be of class `stars`", call. = FALSE)
    }
    if (class(buffer_rad) != "numeric" || buffer_rad < 0) {
        stop("raster must be a positive number", call. = FALSE)
    }
    dart_process_(sf = sf,
                  raster = raster,
                  null_sentinel = null_sentinel,
                  buffer_rad = buffer_rad)
}


dart_process_ <- function(sf,
                           raster,
                          null_sentinel,
                          buffer_rad) {
    # get centroids for each commune/ward
    centroids <- dart_centroid(sf, 9210)

    # calculate max buffer radius
    rad <- seq_len(0)
    if (!buffer_rad) {
        for (i in seq_len(nrow(sf))) {
            rad[[i]] <- dart_max_dist(sf[i, ])
        }
        buffer_rad <- max(rad)
    }
    units(buffer_rad) <- as_units("m")
    # TODO: not final solution
    # 13761.42 (hcmc)

    # create buffers around *centroids*
    buffers <- sf::st_buffer(
        centroids[seq_len(nrow(centroids)), "geometry"],
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
        mutate(centroid = centroids$centroid) %>%
        mutate(ID_3 = centroids$ID_3)

    # spatially join raster and buffer (only take pixels inside the buffers)
    df_sf <- sf::st_join(r_sf, buffers_sf) %>%
        tidyr::drop_na(ID_3)
    colnames(df_sf)[1] <- "val" # quick column rename

    # calculate distances between centroid and pixel centroid for each commune/ward
    df_sf$r_centroid <- dart_centroid(df_sf["geometry"], 9210)$centroid
    df_sf$cent_dist <- sf::st_distance(
        df_sf$centroid, df_sf$r_centroid,
        by_element = TRUE
    ) %>% drop_units()

    # dropping nulls
    df_sf <- df_sf[!(df_sf$val == null_sentinel), ]

    df_sf %>%
        by(
            .$ID_3,
            FUN = function(x) {
                sum((x$val) / (x$cent_dist)) / sum(1 / (x$cent_dist))
            }
        ) %>%
        unclass() %>%
        as.data.frame()
}
