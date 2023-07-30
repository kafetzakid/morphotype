#' Normalizes GAM representation
#'
#' @param mat_spl_FP - a list with the GAM representation for each profile in the dataset.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item mat_spl_FP_sc - a list with the scaled GAM representation for each profile in the dataset. The scaling is performed such that both ends of the GAM vector are superimposed in all profiles.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' load(file = "~/myRpacks/morphotype/data/mat_spl_FP.RData")
#' normalize_GAM(mat_spl_FP = mat_spl_FP)
normalize_GAM = function(mat_spl_FP) {

  mat_spl_FP_sc = mat_spl_FP
  for (i in 1:length(mat_spl_FP)) {
    minY = min(mat_spl_FP[[i]][,2])
    maxY = max(mat_spl_FP[[i]][,2])
    mat_spl_FP_sc[[i]][,2] = (mat_spl_FP[[i]][,2] - minY)/(maxY - minY)
  }

  returns = list("mat_spl_FP_sc" = mat_spl_FP_sc)

  return(returns)

}
