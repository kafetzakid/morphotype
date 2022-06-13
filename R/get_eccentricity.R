#' @title Compute the eccentricity of the profile segment
#'
#' @importFrom EBImage computeFeatures
#' @importFrom EBImage writeImage
#' @importFrom EBImage as.Image
#' @importFrom EBImage rotate
#' @importFrom EBImage flip
#'
#' @param profile.wall a numeric 2D vector. The profile binary matrix.
#' @param outer.line a numeric 1D vector. The outer profile line.
#' @param inner.line a numeric 1D vector. The inner profile line.
#' @param m9 a numeric scalar. The height of the profile.
#' @param m28 a numeric scalar. The height of the profile segment.
#' @param filename a character vector. Name of the image file including extension. The supported extensions are jpeg, png and tiff.
#' @param writeImgA a logical value. Whether to write the image of the profile segment. ---- MOVE TO OTHER FUNCTION ----
#' @param writeImgwd a character vector. Directory in which the image file is written. ---- MOVE TO OTHER FUNCTION ----
#'
#' @return A list with the following items:
#' \itemize{
#'   \item eccentricity - a numeric scalar. The major axis value of the profile segment.
#'   \item mass.centre.x - a numeric scalar. The x position of the profile segment center of mass.
#'   \item mass.centre.y - a numeric scalar. The y position of the profile segment center of mass.
#'   \item majoraxis - a numeric scalar. The major axis value of the profile segment.
#'   \item df_img_rim - a numeric 2D vector. The binary matrix of the profile segment.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_eccentricity(profile.wall = m19$profile.wall, outer.line = m19$outer.line,
#'                  inner.line = m19$inner.line, m9 = m9, m28 = 67,
#'                  filename = filename, writeImgA = TRUE, writeImgwd = '~/')
get_eccentricity = function(profile.wall, outer.line, inner.line, m9, m28, filename, writeImgA, writeImgwd) {

  # min and max radius of an ellipse > check reference

  ## computeFeatures in EBImage
  innP = max(inner.line[(m9-m28+1):m9]) # I had max(...)+2 and I systematically got 2px extra margin on the inner side.
  extP = min(outer.line[(m9-m28+1):m9])
  df_img_rim = rbind(0, profile.wall[extP:innP,(m9-m28+1):m9], 0) # I had 1:innP instead of extP:innP so there was extra margin in the external side when the vessel is not of close shape
  df_img_rim = cbind(0, df_img_rim, 0)
  img_rim = EBImage::as.Image(df_img_rim)
  # display(img_rim)
  img_rotate = EBImage::rotate(EBImage::flop(1-img_rim), 180, bg.col = "white")
  if (writeImgA) {EBImage::writeImage(img_rotate, paste0(writeImgwd, "/img_rim_", filename), quality = 100)}

  # computeFeatures.basic(img_rim, ref = img_rim)
  # computeFeatures.shape(img_rim) # s.radius.min / s.radius.max,  s.perimeter
  # computeFeatures.moment(img_rim, ref = img_rim)   # m.theta: object angle?
  # computeFeatures.haralick(img_rim, ref = img_rim)

  cFmoment = EBImage::computeFeatures.moment(img_rim, ref = img_rim)
  eccentricity = cFmoment[4] # eccentricity
  mass.centre.x = cFmoment[1] # centre of mass x (in pixels)
  mass.centre.y = cFmoment[2] # centre of mass y (in pixels)
  majoraxis = cFmoment[2] # elliptical fit major axis (in pixels)

  returns = list("eccentricity" = eccentricity, "mass.centre.x" = mass.centre.x, "mass.centre.y" = mass.centre.y, "majoraxis" = majoraxis, "df_img_rim" = df_img_rim)
  return(returns)

  # saved also theta, mass cx, mass cy, majoraxis elliptical fit, img_rotate

}
