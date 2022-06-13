#' @title Compute the curl of the profile segment
#'
#' @param perip.original a dataframe with one named numeric vector. The periphery of the profile as a result of the function get_vector_df.
#' @param m28 a numeric scalar. The height from the top of the profile of the point defining the considered segment.
#'
#' @return a numeric scalar. The curl value for the profile segment.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_curl(perip.original = m58$perip.original, m28 = 67)
get_curl = function(perip.original, m28) {
  curl = dim(perip.original)[1]/m28
  return(curl)
}
