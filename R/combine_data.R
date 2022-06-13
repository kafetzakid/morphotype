#' @title Combine two sources of morphological data
#'
#' @importFrom stats prcomp
#' @importFrom factoextra get_eigenvalue
#'
#' @param data1 a distance matrix. Computed using compute_dist_0, compute_dist_1 or compute_dist_2.
#' @param data2 a dataframe. Includes the key column 'filename' and the columns with the morphological information.
#' @param PCs a numeric scalar. The number of principal components to be retained. The default is to select the components that explain more than 1% of the variance.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item df_eig.val - a dataframe. Includes the eigenvalues and variances of the principal dimensions.
#'   \item df_combined - a dataframe. The new dataframe which combines the two input data sources.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' data1 = read.csv('~/myRpacks/morphotype/inst/extdata/data_1D.csv')
#' data2 = read.csv('~/myRpacks/morphotype/inst/extdata/data_0D.csv')
#' data2 = as.data.frame(data2, stringsAsFactors = FALSE)
#' data2 = data2[,c("filename","sherd_rim.diameter","Below.Rim_incl_sin_min")]
#' combine_data(data1 = data1, data2 = data2)
#' combine_data(data1 = data1, data2 = data2, PCs = 3)
combine_data = function(data1, data2, PCs) {

  obs_order_initial = as.character(rownames(data1))

  # order data
  obs_order_initial_id = match(obs_order_initial, data2$filename)
  data2 = data2[obs_order_initial_id, ]

  df_labels = data2$filename
  data2 = data2[,-which(colnames(data2) == "filename")]

  # pca on distance matrix
  pca_d1 = stats::prcomp(data1, scale = F, center = T) # variables are measured on the same scale and have the same unit, so we do not scale
  df_eig.val = factoextra::get_eigenvalue(pca_d1)

  if (missing(PCs)) {
    sel = which.max(df_eig.val$variance.percent > 1)
  } else {
    sel = PCs
  }
  df_pca_d1 = as.data.frame(pca_d1$x[,1:sel])

  df_combined = cbind.data.frame(df_labels, data2, df_pca_d1)

  for (i in 2:dim(df_combined)[2]) {
    df_combined[,i] = (df_combined[,i] - min(df_combined[,i]))/(max(df_combined[,i])-min(df_combined[,i]))
  }

  returns = list("df_eig.val" = df_eig.val, "df_combined" = df_combined)
  return(returns)
}





