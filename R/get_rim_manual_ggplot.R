#' @title Plot rim height, groove and ridge lines on profile
#'
#' @import reshape2
#' @import ggplot2
#'
#' @param filename a character vector. Name of the image file.
#' @param m20 output of get_exterior_lines.
#' @param m19 output of get_wall_lines.
#' @param m28 a numeric scalar. The point of rim end manual from the highest point of the profile, in pixels.
#' @param m9 output of get_height.
#'
#' @return ggplot of the profile and annotations for the rim height, groove and ridge lines
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_manual_ggplot(filename = 'SADR010324.jpg', m20 = m20, m19 = m19, m28 = 67, m9 = m9)
get_rim_manual_ggplot = function(filename, m20, m19, m28, m9) {
  check.points = NULL
  check.points.1 = NULL
  check.points.2 = NULL
  Var1 = Var2 = value = NULL # otherwise define globalVariables() ?
  xx = reshape2::melt(m19$profile.wall)
  colnames(xx) = c("Var1", "Var2", "value")
  if (length(as.numeric(m20$list_potGroov_ext)) > 0) {
    check.points.1 = cbind.data.frame(as.numeric(m20$list_potGroov_ext), "exterior", "#4287f5", stringsAsFactors = FALSE) # exterior
    colnames(check.points.1) = c("yp", "legend", "lab")
    check.points.1$xp = (dim(m19$profile.wall)[1] - 10)
    check.points.1$yp = as.numeric(check.points.1$yp)
    check.points.1
  }
  if (length(as.numeric(m19$list_potGroov_in)) > 0) {
    check.points.2 = cbind.data.frame(cbind(as.numeric(m19$list_potGroov_in), "interior", "#f5a442"), stringsAsFactors = FALSE) # interior
    colnames(check.points.2) = c("yp", "legend", "lab")
    check.points.2$xp = (dim(m19$profile.wall)[1] - 10)
    check.points.2$yp = as.numeric(check.points.2$yp)
  }
  check.points = rbind.data.frame(check.points.1, check.points.2)
  if (dim(check.points)[1] > 0) {
    p_input_profile = ggplot2::ggplot(data=xx, aes(x = Var1, y = Var2)) + geom_point(aes(color = as.factor(value)), alpha = 0.3, size = 1) +
      geom_hline(yintercept = m9 - m28, linetype="dashed", color = "darkgreen", size = 1) +
      geom_point(data = check.points, x = as.numeric(check.points$xp), y = as.numeric(check.points$yp), color = as.factor(check.points$lab), alpha = 0.6, size = 2) +
      scale_color_manual(values=c('#ffffff', '#000000')) + ggtitle(paste0("Input Image ", filename)) + theme_bw() +
      theme(panel.border = element_blank(), plot.title = element_text(size=12, hjust = 0.5), legend.position = "none")
  } else {
    p_input_profile = ggplot2::ggplot(data=xx, aes(x = Var1, y = Var2)) + geom_point(aes(color = as.factor(value)), alpha = 0.3, size = 1) +
      geom_hline(yintercept = m9 - m28, linetype="dashed", color = "darkgreen", size = 1) +
      scale_color_manual(values=c('#ffffff', '#000000')) + ggtitle(paste0("Input Image ", filename)) + theme_bw() +
      theme(panel.border = element_blank(), plot.title = element_text(size=12, hjust = 0.5), legend.position = "none")
  }
  return(p_input_profile)
}

# get_rim_manual_ggplot("filename" = "SADR011815.jpg",
# m20 = list("list_potGroov_ext" = 13),
# m19 = list("profile.wall" = cbind(matrix(0, nrow = 50, ncol = 3), matrix(1, nrow = 50, ncol = 10), matrix(0, nrow = 50, ncol = 20)),
# "outer.line" = rep(4, 50),
# "inner line" = rep(14, 50),
# "list_potGroov_in" = NULL,
# "listlines_in" = NULL,
# "list_corr" = NULL,
# "has.fullbase" = FALSE,
# "fullbase" = NULL,
# "base.height" = NULL,
# "is.full.profile" = FALSE),
# m28 = 5,
# m9 = 50)

