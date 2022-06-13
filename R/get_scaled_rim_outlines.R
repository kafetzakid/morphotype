#' @title Scale the profile and retrieves the outline
#'
#' @importFrom EBImage as.Image
#' @importFrom EBImage resize
#' @importFrom EBImage combine
#' @importFrom EBImage getFrames
#'
#' @param profile.wall a numeric 2D vector. The profile binary matrix.
#' @param inner.line a numeric 1D vector. The inner profile line.
#' @param m9 a numeric scalar. The height of the profile.
#' @param m28 a numeric scalar. The height of the rim profile.
#' @param hsc a numeric scalar. The new profile height in pixels.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item df.img.rim.scaled - a numeric 2D vector. The scaled profile binary matrix.
#'   \item outer.rim.line.scaled - a numeric 1D vector. The outer line of the scaled profile.
#'   \item inner.rim.line.scaled - a numeric 1D vector. The inner line of the scaled profile.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_scaled_rim_outlines(profile.wall = m19$profile.wall,
#'                         inner.line = m19$inner.line,
#'                         m9 = m9, m28 = 67, hsc = 90)
get_scaled_rim_outlines = function(profile.wall, inner.line, m9, m28, hsc) {

  innP = max(inner.line[(m9-m28+1):m9])
  df_img_rim = profile.wall[1:innP,(m9-m28+1):m9]
  img_rim = EBImage::as.Image(df_img_rim)
  # display(img_rim)
  # img_rotate = rotate(img_rim, 180, bg.col = "black") # do not rotate
  # img_rotate = img_rim
  # display(img_rotate)
  img_scale = EBImage::resize(img_rim, h = hsc)
  # display(img_scale)
  threshold = 0.01 # global threshold 0.01
  img_thr = EBImage::combine( mapply(function(frame, th) frame > th, EBImage::getFrames(img_scale), threshold, SIMPLIFY = FALSE) )
  # display(img_thr, all = TRUE)
  df_img = img_thr@.Data
  df_img = ifelse(df_img == TRUE, 1, 0)

  # df outer scaled
  df_outer = NULL
  for (outer in 1:dim(df_img)[2]) {
    df_outer[outer] = min(which(df_img[,outer]>0))
  }
  # df inner scaled
  df_inner = NULL
  for (inner in 1:dim(df_img)[2]) {
    df_inner[inner] = max(which(df_img[,inner]>0))
  }

  returns = list("df.img.rim.scaled" = df_img, "outer.rim.line.scaled" = df_outer, "inner.rim.line.scaled" = df_inner)

  return(returns)

}
