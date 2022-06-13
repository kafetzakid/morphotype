#' @title Estimate graph from distance matrix
#'
#' @importFrom stats as.dist
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph as_data_frame
#' @importFrom igraph layout_nicely
#' @importFrom igraph mst
#' @importFrom MASS Shepard
#' @importFrom stats cor
#'
#' @param distM a distance matrix. Computed using compute_dist_0, compute_dist_1 or compute_dist_2.
#' @param filter_values stad parameter. Default is NULL.
#' @param num_intervals stad parameter. Default is NULL.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item graph_est - an igraph object. The estimated graph which is either the graps estimated based on the stad algorithm or the minimum spanning tree.
#'   \item df_links - a dataframe. Contains the links of the graph under the columns 'Source' and 'Target' and the edge weight under name 'Value2'.
#'   \item plot.shepard - a list of four. Shepard diagram data as provided by MASS::Shepard plus the Pearson correlation value as quality measure for the map estimation.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' distM = read.csv('~/myRpacks/morphotype/inst/extdata/distM.csv', row.names = 1)
#' map_data(distM)
map_data = function(distM, filter_values = NULL, num_intervals = NULL) {

  distM = distM/max(distM)
  rownames(distM) = NULL
  colnames(distM) = NULL

  if (requireNamespace("stad", quietly = TRUE)) { # if (rlang::is_installed("pkg")) {
    distM = stats::as.dist(distM)
    set.seed(611)
    graph_est = stad::stad(distM, filter_values = filter_values, num_intervals = num_intervals)

    # plot_graph(graph_est, layout = igraph::layout_nicely,
    #            vertex.color = 1,
    #            vertex.frame.color = 1,
    #            # vertex.size = 15*0.1*df_measures$sherd_height, edge.width = 0.2)
    #            # vertex.size = 15*df_measures$sherd_rim.diameter,
    #            edge.width = 0.2)

    df_links = igraph::as_data_frame(graph_est$graph)
    nicely_coord = igraph::layout_nicely(graph_est$graph)
  } else {
    message("Package 'stad' is not installed, the minimum spanning tree is used for netwrok embedding.")
    distM = as.matrix(distM)
    graph_complete = igraph::graph_from_adjacency_matrix(distM, mode = "undirected", weighted = TRUE)
    graph_est = igraph::mst(graph_complete)
    df_links = igraph::as_data_frame(graph_est)
    nicely_coord = igraph::layout_nicely(graph_est)
    distM = stats::as.dist(distM)
  }

  # links data
  names(df_links)[1] = "Source"; names(df_links)[2] = "Target"
  df_links$Source = as.numeric(df_links$Source)
  df_links$Target = as.numeric(df_links$Target)
  df_links$Value2 = ceiling(100*df_links$weight)
  df_links = df_links[, colnames(df_links) %in% c("Source" , "Target", "Value2")]

  # Shepard diagram
  plot.shepard = MASS::Shepard(distM, nicely_coord, p = 2)
  plot.shepard$cor = round(stats::cor(plot.shepard$yf, plot.shepard$y, method = "pearson"),3)
  # plot(plot.shepard, pch = ".", main = "Shepard diagram for network embedding (naive)")
  # lines(plot.shepard$x, plot.shepard$yf, type = "l", lwd = 3, col = "#ab7f07")
  # text(x = 0.35*mean(plot.shepard$x), y = 0.9*max(plot.shepard$y), cex = 1.2, bquote(paste('cor'[xy]*' = ',.(round(cor(plot.shepard$yf, plot.shepard$y),3)))))

  returns = list("graph_est" = graph_est, "df_links" = df_links, "plot.shepard" = plot.shepard)
  return(returns)
}
