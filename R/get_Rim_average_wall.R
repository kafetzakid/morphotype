#' @title Compute the wall thickness across the rim profile height
#'
#' @param outL a numeric 1D vector. The outer part of the profile outline.
#' @param innL a numeric 1D vector. The inner part of the profile outline.
#' @param m28 a numeric scalar. The height of the rim profile.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item all_WT - a numeric 1D vector including the wall thickness in pixels across the height of the profile wall.
#'   \item mean.WT - the average of all_WT.
#'   \item median.WT - the median of all_WT.
#'   \item sd.WT - the standard deviation of all_WT.
#'   \item min.WT - the minimum of all_WT.
#'   \item max.WT - the maximum of all_WT.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_Rim_average_wall(outL = m19$outer.line, innL = m19$inner.line, m28 = 67)
get_Rim_average_wall = function(outL, innL, m28) {

  outL = outL[(length(outL) - m28):length(outL)]
  innL = innL[(length(innL) - m28):length(innL)]

  all_WT = innL - outL
  mean.WT = mean(all_WT)
  median.WT = median(all_WT)
  sd.WT = sd(all_WT)
  min.WT = min(all_WT)
  max.WT = max(all_WT)

  returns = list("all_WT" = all_WT, "mean.WT" = mean.WT, "median.WT" = median.WT, "sd.WT" = sd.WT, "min.WT" = min.WT, "max.WT" = max.WT)

  return(returns)

}
