#' @title Compute the radius ratio for a profile segment
#'
#' @param m47 a list with the following items:
#' \itemize{
#'   \item all_WT - a numeric 1D vector including the wall thickness in pixels across the height of the profile wall.
#'   \item mean.WT - the average of all_WT.
#'   \item median.WT - the median of all_WT.
#'   \item sd.WT - the standard deviation of all_WT.
#'   \item min.WT - the minimum of all_WT.
#'   \item max.WT - the maximum of all_WT.
#' }
#'
#' @return a numeric scalar. The radius ratio of the profile segment.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_radius.ratio(m47 = m47)
get_radius.ratio = function(m47) {
  radius.ratio = m47$max.WT/m47$min.WT
  # not correct? inscribed radius / circumscribed radius
  # in computeFeatures.shape: s.radius.min / s.radius.max ?
  return(radius.ratio)
}
