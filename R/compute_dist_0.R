#' @title Compute distance between point measures
#'
#' @importFrom stats dist
#'
#' @param data a dataframe. The first column includes the filename of the profile drawing which serves as key. The following columns include numeric data which should be used for computing the distance matrix.
#' @param fun a character vector. The distance method that should be used. This is one of the following: "euclidean", "manhattan", "canberra", "minkowski", "chebysev".
#' @param p_mink a numeric scalar. The order that should be used for the Minkowski distance. This parameter is optional. In case the Minkowski distance is computed, p_mink takes the default value of 4.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item data_normalised - a dataframe. The normalised version of the input data.
#'   \item dist_matrix - a 2D matrix. The distance matrix of the normalised data.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' data = read.csv('~/myRpacks/morphotype/inst/extdata/data_0D.csv')
#' data = as.data.frame(data, stringsAsFactors = FALSE)
#' data = data[,c(1:28, 31:32, 37, 47:58, 62:65)]
#' compute_dist_0(data = data, fun = "manhattan")
#' compute_dist_0(data = data, fun = "minkowski", p_mink = 4)
compute_dist_0 = function(data, fun, p_mink = 4) {

  df_labels = data[,1]
  data = data[,-1]
  vmin = lapply(data, min)
  vmax = lapply(data, max)
  df_norm = data
  for (i in 1:dim(data)[2]) {
    if ((vmax[[i]] - vmin[[i]]) > 0) {
      df_norm[,i] = (data[,i] - vmin[[i]]) / (vmax[[i]] - vmin[[i]])
    } else {
      df_norm[,i] = data[,i]
    }
  }
  rownames(df_norm) = df_labels

  # Euclidean, Manhattan, Canberra, Minkowski
  if (fun %in% c("euclidean", "manhattan", "canberra", "minkowski")) {
    if (is.null(p_mink)) {print("Order is not provided. The order of 4 is used."); p_mink = 4}
    distM = as.matrix(stats::dist(df_norm, method = fun, p = p_mink))
  }

  # Chebysev
  if (fun == "chebysev") {
    distM = as.matrix(stats::dist(df_norm, method = "maximum"))
  }

  # provide column and row names
  rownames(distM) = colnames(distM) = df_labels

  returns = list("data_normalised" = df_norm, "dist_matrix" = distM)
  return(returns)


}
