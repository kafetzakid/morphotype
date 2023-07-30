#' Plots all GAM outlines
#'
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics lines
#'
#' @param mat_spl_FP a list. The list contains the GAM representation for each profile in the dataset. Each item of the list has two variables in the following order: 'spl_x', 'spl_y', as a result of function compute_GAM().
#' @param ylim_range a numeric vector. The vector contains the minimum and maximum values for the y-axis.
#'
#' @return A plot of all GAM outlines, original or normalized.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' load(file = "~/myRpacks/morphotype/data/mat_spl_FP.RData")
#' plot_GAM(mat_spl_FP = mat_spl_FP, ylim_range = c(0, 550))
plot_GAM = function(mat_spl_FP, ylim_range) {

  par(mfrow = c(1,1))
  plot(mat_spl_FP[[1]][,1], mat_spl_FP[[1]][,2], type = "l", ylim = ylim_range)
  if (length(mat_spl_FP) > 1) {
    for (i in 2:length(mat_spl_FP)) {lines(mat_spl_FP[[i]][,1], mat_spl_FP[[i]][,2])}
  }

}

