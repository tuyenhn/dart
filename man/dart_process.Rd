% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dart_process.R
\name{dart_process}
\alias{dart_process}
\title{Calculate average of raster pixels in a buffer for each geometry
feature of an sf object}
\usage{
dart_process(sf, raster, sf_crs = 4326, null_sentinel = -9999, buffer_rad = 0)
}
\arguments{
\item{sf}{sf object}

\item{raster}{stars object}

\item{sf_crs}{projection CRS that will be used to calculate (default: 4326)}

\item{null_sentinel}{value representing null in raster (default: -9999)}

\item{buffer_rad}{radius of buffer around the centroid of
geometry feature (optional)}
}
\value{
A dataframe
}
\description{
Calculate average of raster pixels in a buffer for each geometry
feature of an sf object
}
\examples{
\dontrun{
# read in gson vector
hcmc_gson <- st_read(
    readChar(gson_fname, file.info(gson_fname)$size),
    quiet = TRUE
)
# read raster
raster <- stars::read_stars(r_fname)

avg_df <- dart_process(hcmc_gson, raster, 9210)
}
}
