#' @title Compute the normalized wall thickness across the profile height and relevant statistics
#'
#' @importFrom stats median
#' @importFrom stats var
#' @importFrom e1071 skewness
#' @importFrom e1071 kurtosis
#'
#' @param df_img_rim a numeric 2D vector. The binary matrix of the profile segment.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item WT.vec_norm - a numeric 1D vector. The wall thickness in pixels across the height of the normalized rim profile.
#'   \item WT.mean_norm - a numeric scalar. The average of WT.vec_norm.
#'   \item WT.med_norm - a numeric scalar. The median of WT.vec_norm.
#'   \item WT.var_norm - a numeric scalar. The variance of WT.vec_norm.
#'   \item WT.skew_norm - a numeric scalar. The skewness of WT.vec_norm.
#'   \item WT.kurt_norm - a numeric scalar. The kurtosis of WT.vec_norm.
#'   \item WT.BotMed_norm - a numeric scalar. The bottom width of the normalized rim profile.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_Rim_norm_wall(df_img_rim = m52$df_img_rim)
get_Rim_norm_wall = function(df_img_rim) {

  # could be merged with get_Rim.average.wall
  WT.vec.i_norm = colSums(df_img_rim) # column 1 is the bottom border, row 1 is the left border, at the outer line of the profile.
  WT.vec.i_norm = WT.vec.i_norm[-c(1,length(WT.vec.i_norm))]
  WT.vec_orig = WT.vec.i_norm
  WT.vec_norm = (WT.vec.i_norm - min(WT.vec.i_norm)) / (max(WT.vec.i_norm) - min(WT.vec.i_norm))
  WT.mean_norm = mean(WT.vec_norm)
  WT.med_norm = stats::median(WT.vec_norm)
  WT.var_norm = stats::var(WT.vec_norm)
  WT.skew_norm = e1071::skewness(WT.vec_norm) # (-) = large tail of small values
  WT.kurt_norm = e1071::kurtosis(WT.vec_norm) # (+) = thin pointed distribution
  WT.BotMed_norm = WT.vec.i_norm[length(WT.vec.i_norm)]/WT.med_norm # WT bottom over WT median

  returns = list("WT.vec_norm" = WT.vec_norm, "WT.mean_norm" = WT.mean_norm, "WT.med_norm" = WT.med_norm, "WT.var_norm" = WT.var_norm, "WT.skew_norm" = WT.skew_norm, "WT.kurt_norm" = WT.kurt_norm, "WT.BotMed_norm" = WT.BotMed_norm)
  return(returns)

}
