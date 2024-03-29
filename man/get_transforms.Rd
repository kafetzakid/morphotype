% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_transforms.R
\name{get_transforms}
\alias{get_transforms}
\title{Transform the binary profile matrix into a dataframe}
\usage{
get_transforms(image_profile)
}
\arguments{
\item{image_profile}{a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.}
}
\value{
a melted dataframe. The melted binary profile matrix.
}
\description{
Transform the binary profile matrix into a dataframe
}
\examples{
get_transforms(image_profile = m7$image_profile)
}
\author{
Danai Kafetzaki
}
