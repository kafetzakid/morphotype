% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rim_diameter.R
\name{get_rim_diameter}
\alias{get_rim_diameter}
\title{Compute the rim diameter of the profile}
\usage{
get_rim_diameter(xx, sherd_height, margin)
}
\arguments{
\item{xx}{a dataframe. Melted profile matrix as a result of get_transforms.}

\item{sherd_height}{a numeric scalar. The height of the profile.}

\item{margin}{a numeric scalar. The number of pixels checked for correction in the calculation.}
}
\value{
A list with the following items:
\itemize{
\item sherd_rimDiam - a numeric scalar. The rim diameter of the profile in pixels.
\item sherd_rimDiam.certain - a character vector. Specifies whether the calculated rim diameter is 'known' or 'unknown'.
}
}
\description{
Compute the rim diameter of the profile
}
\examples{
get_rim_diameter(xx = m10, sherd_height = m9, margin = 10)
}
\author{
Danai Kafetzaki
}
