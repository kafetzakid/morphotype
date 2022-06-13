#' @title Compute the horizontal cut of rim profile
#'
#' @param outL a numeric 1D vector. The outer profile line.
#' @param m28 a numeric scalar. The height of the rim profile.
#' @param innL a numeric 1D vector. The inner profile line.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item Rim_HorizCut.Out_WT - a numeric scalar. The horizontal cut length at the outer of the rim profile.
#'   \item Rim_HorizCut.Out_Width - a numeric scalar. The width of Rim_HorizCut.Out_WT.
#'   \item Rim_HorizCut.Out_Cont - a character vector. Checks whether there are discontinuities in Rim_HorizCut.Out_WT.
#'   \item Rim_HorizCut.Inn_WT - a numeric scalar. The horizontal cut length at the inner of the rim profile.
#'   \item Rim_HorizCut.Inn_Width - a numeric scalar. The width of Rim_HorizCut.Inn_WT.
#'   \item Rim_HorizCut.Inn_Cont - a character vector. Checks whether there are discontinuities in Rim_HorizCut.Inn_WT.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_thickness_HC(outL = m19$outer.line, m28 = 67, innL = m19$inner.line)
get_rim_thickness_HC = function(outL, m28, innL){

  outWp = min(outL[(length(outL) - m28):length(outL)]) # == min() instead of == 1
  Rim_HorizCut.Out_WT = sum(outL[(length(outL) - m28):length(outL)] == outWp)
  df.int = data.frame("V1" = outL[(length(outL) - m28):length(outL)] == outWp)
  rleV1 = rle(df.int$V1)
  Rim_HorizCut.Out_Width = sum(rleV1$values)
  if (Rim_HorizCut.Out_Width == 1) {Rim_HorizCut.Out_Cont = "no discontinuities"} else {Rim_HorizCut.Out_Cont = "discontinuities"}

  innWp = max(innL[(length(innL) - m28):length(innL)])
  Rim_HorizCut.Inn_WT = sum(innL[(length(innL) - m28):length(innL)] == innWp)
  df.int = data.frame("V1" = innL[(length(innL) - m28):length(innL)] == innWp)
  rleV1 = rle(df.int$V1)
  Rim_HorizCut.Inn_Width = sum(rleV1$values)
  if (Rim_HorizCut.Inn_Width == 1) {Rim_HorizCut.Inn_Cont = "no discontinuities"} else {Rim_HorizCut.Inn_Cont = "discontinuities"}

  returns = list("Rim_HorizCut.Out_WT" = Rim_HorizCut.Out_WT, "Rim_HorizCut.Out_Width" = Rim_HorizCut.Out_Width, "Rim_HorizCut.Out_Cont" = Rim_HorizCut.Out_Cont,
                 "Rim_HorizCut.Inn_WT" = Rim_HorizCut.Inn_WT, "Rim_HorizCut.Inn_Width" = Rim_HorizCut.Inn_Width, "Rim_HorizCut.Inn_Cont" = Rim_HorizCut.Inn_Cont)

  return(returns)
}



