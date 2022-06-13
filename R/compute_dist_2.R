#' @title Compute distance between two-dimensional outlines
#'
#' @importFrom stats xtabs
#' @importFrom SimilarityMeasures DTW
#' @importFrom SimilarityMeasures Frechet
#' @importFrom SimilarityMeasures EditDist
#' @importFrom SimilarityMeasures LCSSCalc
#'
#' @param data a dataframe.
#' @param fun a character vector. The distance method that should be used, which is one of the following: "dtw", "jaccard".
#' @param pS a numeric scalar.
#' @param pD a numeric scalar.
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
#' load('~/myRpacks/morphotype/data/data_2D.RData')
#' compute_dist_2(data = mat_circumference_scaled[1:2], fun = "edit")
compute_dist_2 = function(data, fun, pS = 10, pD = 10){

  df_norm = data # !!! df_norm ----> user chooses registration point !

  distM = matrix(nrow = length(df_norm), ncol = length(df_norm), 0)

  if (fun == "dtw") {
    for (i in 1:(length(df_norm)-1)) {
      mat = as.data.frame(df_norm[i])
      mat$Val = 1
      mat1 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
      for (j in (i+1):length(df_norm)) {
        mat = as.data.frame(df_norm[j])
        mat$Val = 1
        mat2 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
        distM[i,j] = SimilarityMeasures::DTW(mat1, mat2, pointSpacing = pS)
        distM[j,i] = distM[i,j]
      }
    }
  }

  if (fun == "frechet") {
    for (i in 1:(length(df_norm)-1)) {
      mat = as.data.frame(df_norm[i])
      mat$Val = 1
      mat1 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
      for (j in (i+1):length(df_norm)) {
        mat = as.data.frame(df_norm[j])
        mat$Val = 1
        mat2 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
        distM[i,j] = SimilarityMeasures::Frechet(mat1, mat2, testLeash = pS)
        distM[j,i] = distM[i,j]
      }
    }
  }


  if (fun == "edit") {
    for (i in 1:(length(df_norm)-1)) {
      mat = as.data.frame(df_norm[i])
      mat$Val = 1
      mat1 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
      for (j in (i+1):length(df_norm)) {
        mat = as.data.frame(df_norm[j])
        mat$Val = 1
        mat2 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
        distM[i,j] = SimilarityMeasures::EditDist(mat1, mat2, pointDistance = pD)
        distM[j,i] = distM[i,j]
      }
    }
  }


  if (fun == "lcss") {
    for (i in 1:(length(df_norm)-1)) {
      mat = as.data.frame(df_norm[i])
      mat$Val = 1
      mat1 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
      for (j in (i+1):length(df_norm)) {
        mat = as.data.frame(df_norm[j])
        mat$Val = 1
        mat2 = as.matrix(stats::xtabs(Val ~ X1 + X2, mat))
        distM[i,j] = SimilarityMeasures::LCSSCalc(mat1, mat2, pointSpacing = pS, pointDistance = pD)
        distM[j,i] = distM[i,j]
      }
    }
  }

  # provide column and row names
  rownames(distM) = colnames(distM) = colnames(df_norm)

  returns = list("data_normalised" = df_norm, "dist_matrix" = distM)
  return(returns)


}
