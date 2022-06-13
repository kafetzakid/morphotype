#' @title Cluster data with fuzzy c-means
#'
#' @importFrom e1071 cmeans
#'
#' @param distM a distance matrix. Computed using compute_dist_0, compute_dist_1 or compute_dist_2.
#' @param data a dataframe. Includes at least the key column 'filename'.
#' @param ncl a numeric scalar. The number of clusters to be specified.
#'
#' @return a dataframe. Includes the fuzzy clustering results.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' distM = read.csv('~/myRpacks/morphotype/inst/extdata/distM.csv', row.names = 1)
#' data = read.csv('~/myRpacks/morphotype/inst/extdata/data_0D.csv')
#' data = as.data.frame(data, stringsAsFactors = FALSE)
#' data = data[,c("filename","TV", "Type")]
#' cluster_data(distM = distM, data = data, ncl = 3)
cluster_data = function(distM, data, ncl) {
  set.seed(611)
  cm = e1071::cmeans(distM, ncl, iter.max = 100000, dist = "euclidean", method = "cmeans", m = 2)
  cm_data = cbind.data.frame(data, cm$cluster, cm$membership)
  dist_from = paste0("dist_from", 1:ncl)
  names(cm_data)[(dim(data)[2]+1):dim(cm_data)[2]] = c("hard_cl", dist_from)
  return(cm_data)
}
