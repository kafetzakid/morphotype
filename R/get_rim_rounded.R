#' @title Compute rim measures of roundness
#'
#' @importFrom EBImage as.Image
#' @importFrom EBImage computeFeatures.shape
#'
#' @param profile.wall a numeric 2D vector. The profile binary matrix.
#' @param sherd.height a numeric scalar. The height of the profile.
#' @param rim.height a numeric scalar. The height of the rim profile.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item FormFactor - a numeric scalar. The form factor of the profile segment.
#'   \item AspectRatio - a numeric scalar. The aspect ratio of the profile segment.
#'   \item Roundness - a numeric scalar. The roundness of the profile segment.
#' }
#'
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_rounded(profile.wall = m19$profile.wall, sherd.height = m9, rim.height = 67)
get_rim_rounded = function(profile.wall, sherd.height, rim.height) {

  FormFactor = 9999
  AspectRatio = 9999
  Roundness = 9999

  if (length(rim.height) > 0 && rim.height != 9999 && rim.height > 0) {
    dataRim = profile.wall[, sherd.height:(sherd.height - rim.height + 1)]
    dataRim = dataRim[rowSums(dataRim)>0,]

    dataRimImg = EBImage::as.Image(dataRim)
    # display(dataRimImg)
    features.shape = as.data.frame(EBImage::computeFeatures.shape(dataRimImg))
    FormFactor = (4*pi*(features.shape$s.area)) / (features.shape$s.perimeter^2)
    AspectRatio = (2*features.shape$s.radius.max) / (2*features.shape$s.radius.min)
    Roundness = (4*features.shape$s.area) / (pi*(2*features.shape$s.radius.max)^2)
  }

  returns = list("FormFactor" = FormFactor, "AspectRatio" = AspectRatio, "Roundness" = Roundness)

  return(returns)
}
