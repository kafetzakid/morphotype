#' @title Compute the extent of the profile segment
#'
#' @param profile.wall a numeric 2D vector. The profile binary matrix.
#' @param m9 a numeric scalar. The height of the profile.
#' @param m28 a numeric scalar. The height of the profile segment.
#'
#' @return a numeric scalar. The extent of the profile segment.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_extent1(profile.wall = m19$profile.wall, m9 = m9, m28 = 67)
get_extent1 = function(profile.wall, m9, m28) {
  bbxArea = m28*max(colSums(profile.wall[,(m9-m28+1):m9]))
  extent1 = sum(profile.wall[,(m9-m28+1):m9])/bbxArea
  return(extent1)
}
