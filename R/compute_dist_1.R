#' @title Compute distance between one-dimensional outlines
#'
#' @importFrom stats cor
#' @importFrom stats ks.test
#'
#' @param data a dataframe. The first column of the dataframe includes the index of the outline. The rest of the columns include the scaled 1D outline of each profile under study.
#' @param fun a character vector. The distance method that should be used, which is one of the following: "manhattan", "chebysev", "canberra", "minkowski", "euclidean", "pearson", "spearman", "kendall", "kst".
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
#' data = read.csv('~/myRpacks/morphotype/inst/extdata/data_1D.csv')
#' compute_dist_1(data = data, fun = "manhattan")
#' compute_dist_1(data = data, fun = "minkowski", p_mink = 4)
compute_dist_1 = function(data, fun, p_mink = 2){

  data = data[,-1]

  # scale data in [0,1]
  vmin = min(unlist(lapply(data, min)))
  vmax = max(unlist(lapply(data, max)))
  df_norm = (data - vmin) / (vmax - vmin)

  # initialise distance data
  distM = matrix(0, nrow = dim(df_norm)[2], ncol = dim(df_norm)[2])

  # manhattan distance
  if (fun == "manhattan") {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = sum(abs(df_norm[,i] - df_norm[,j]))
        distM[j,i] = distM[i,j]
      }
    }
  }

  # chebysev distance
  if (fun == "chebysev") {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = max(abs(df_norm[,i] - df_norm[,j]))
        distM[j,i] = distM[i,j]
      }
    }
  }

  # canberra distance
  if (fun == "canberra") {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = sum(abs(df_norm[,i] - df_norm[,j])/(abs(df_norm[,i]) + abs(df_norm[,j])))
        distM[j,i] = distM[i,j]
      }
    }
  }

  # Minkowski distance with user specified order
  if (fun == "minkowski") {
    if (is.null(p_mink)) {print("Order is not provided. The order of 4 is used."); p_mink = 4}
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = sum(abs(df_norm[,i] - df_norm[,j])^p_mink)^(1/p_mink)
        distM[j,i] = distM[i,j]
      }
    }
  }

  # Euclidean distance with user specified order
  if (fun == "euclidean") {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = sum(abs(df_norm[,i] - df_norm[,j])^2)^(1/2)
        distM[j,i] = distM[i,j]
      }
    }
  }


  # Correlation measure
  if (fun %in% c("pearson", "spearman", "kendall")) {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = stats::cor(cbind.data.frame(df_norm[,i],df_norm[,j]), method = fun)[1,2]
        distM[j,i] = distM[i,j]
      }
    }
  }

  # KST divergence
  if (fun == "kst") {
    for (i in 1:(dim(df_norm)[2]-1)) {
      for (j in (i+1):dim(df_norm)[2]) {
        distM[i,j] = stats::ks.test(df_norm[,i], df_norm[,j])$statistic
        distM[j,i] = distM[i,j]
      }
    }
  }

  # provide column and row names
  rownames(distM) = colnames(distM) = colnames(df_norm)

  returns = list("data_normalised" = df_norm, "dist_matrix" = distM)
  return(returns)

}
