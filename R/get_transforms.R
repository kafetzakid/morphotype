#' @title Transform the binary profile matrix into a dataframe
#'
#' @param image_profile a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#'
#' @return a melted dataframe. The melted binary profile matrix.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_transforms(image_profile = m7$image_profile)
get_transforms = function(image_profile){

  xx = reshape2::melt(image_profile)
  return(xx)

}
