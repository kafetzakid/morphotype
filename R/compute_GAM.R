#' Computes GAM representation of profile wall
#'
#' @importFrom stats smooth.spline
#' @importFrom reshape2 melt
#'
#' @param df_profiles_2D_ScH a list of numeric matrices. Each numeric matrix contains the data for the rescaled profile, as a result of function get_data_GAM().
#'
#' @return A list with the following items:
#' \itemize{
#'   \item mat_spl_FP - a list of a 2D vector for each profile in the dataset. The y axis is the GAM vector for each profile and the x axis is the index sequence which by construction has equal length for all profiles in the dataset.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' load(file = "~/myRpacks/morphotype/data/profile_ScH_1C100_20230405.RData")
#' compute_GAM(df_profiles_2D_ScH = mat_cropped_profile_ScH)
compute_GAM = function(df_profiles_2D_ScH) {

  mat_spl_FP = list()
  for (i in 1:length(df_profiles_2D_ScH)) {
    mat_i = df_profiles_2D_ScH[[i]]
    mat_i_melted = reshape2::melt(mat_i)
    mat_i_melted = mat_i_melted[mat_i_melted$value>0,]
    fit = stats::smooth.spline(mat_i_melted$Var2, mat_i_melted$Var1, cv = FALSE)
    df_spl_ScH = data.frame("spl_x" = fit$x, "spl_y" = fit$y)
    mat_spl_FP[i] = list(df_spl_ScH)
  }

  returns = list("mat_spl_FP" = mat_spl_FP)

  return(returns)

}
