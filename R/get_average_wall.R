#' @title Compute the wall thickness across the profile height and relevant statistics
#'
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @param outer.line a numeric 1D vector. The outer profile line.
#' @param inner.line a numeric 1D vector. The inner profile line.
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
#' get_average_wall(outer.line = m19$outer.line, inner.line = m19$inner.line)
get_average_wall = function(outer.line, inner.line) {

  all_WT = inner.line - outer.line - 1
  mean.WT = mean(all_WT)
  median.WT = stats::median(all_WT)
  sd.WT = stats::sd(all_WT)
  min.WT = min(all_WT)
  max.WT = max(all_WT)

  returns = list("all_WT" = all_WT, "mean.WT" = mean.WT, "median.WT" = median.WT, "sd.WT" = sd.WT, "min.WT" = min.WT, "max.WT" = max.WT)

  return(returns)

}
