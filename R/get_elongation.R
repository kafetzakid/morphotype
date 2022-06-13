#' @title Compute the eccentricity of the profile segment
#'
#' @param m28 a numeric scalar. The height of the profile segment.
#' @param m47 a list. The result of the get_average_wall.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item elongation.min - a numeric scalar. The elongation using as basis the minimum wall thickness.
#'   \item elongation.max - a numeric scalar. The elongation using as basis the maximum wall thickness.
#'   \item elongation.avg - a numeric scalar. The elongation using as basis the average wall thickness.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_elongation(m28 = 67, m47 = m47)
get_elongation = function(m28, m47) {
  elong.min = m28/m47$min.WT
  elong.max = m28/m47$max.WT
  elong.avg = m28/m47$mean.WT
  returns = list("elongation.min" = elong.min, "elongation.max" = elong.max, "elongation.avg" = elong.avg)
  return(returns)
}
