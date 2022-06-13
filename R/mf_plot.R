#' @title Plot membership functions for high and low fuzzy sets
#'
#' @import ggplot2
#'
#' @param dataplot_mfs a dataframe. The input data for the plot, with at least three variables: "xvals_mf", "yvals_mf" and "mf".
#'
#' @return a ggplot. The membership functions for high and low fuzzy sets.
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' mfl = mf_logistic(indMin = 0,indMax = 1, seqstep = 0.001, kval = 15, x0val = 0.5)
#' dataplot_mf_A=data.frame("xvals_mf"=mfl$xvals_mf,"yvals_mf"=mfl$yvals_mf_A,"mf"="high")
#' dataplot_mf_B=data.frame("xvals_mf"=mfl$xvals_mf,"yvals_mf"=mfl$yvals_mf_B,"mf"="low")
#' dataplot_mfs = rbind.data.frame(dataplot_mf_A, dataplot_mf_B)
#' mf_plot(dataplot_mfs = dataplot_mfs)
#'
#' mfl=mf_logistic_gaussian(indMin=-50,indMax=50,seqstep=0.1,kval=0.3,x0val=0,mean0=0,sd0=5)
#' dataplot_mf_A=data.frame("xvals_mf"=mfl$xvals_mf,"yvals_mf"=mfl$yvals_mf_A,"mf"="positive")
#' dataplot_mf_B=data.frame("xvals_mf"=mfl$xvals_mf,"yvals_mf"=mfl$yvals_mf_B,"mf"="zero")
#' dataplot_mf_C=data.frame("xvals_mf"=mfl$xvals_mf,"yvals_mf"=mfl$yvals_mf_C,"mf"="negative")
#' dataplot_mfs=rbind.data.frame(dataplot_mf_A, dataplot_mf_B, dataplot_mf_C)
mf_plot = function(dataplot_mfs) {

  xvals_mf = yvals_mf = mf = NULL # otherwise define globalVariables() ?

  ggp = ggplot2::ggplot(dataplot_mfs, aes(x = xvals_mf, y = yvals_mf, fill = mf), orientation = "x") +
        geom_line() + geom_ribbon(aes(ymin = 0, ymax = yvals_mf, fill = mf), alpha=0.4) +
        # geom_segment(aes(x = 0, xend = 0.5, y = 0.5, yend = 0.5), color = "black", linetype = "longdash", size = 1) +
        # geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 0.5), color = "black", linetype = "longdash", size = 1) +
        labs(x = "universe",
             y = 'degree of membership') +
        theme(legend.position = c(0.8, 0.2),
              legend.direction = "vertical",
              legend.text=element_text(size = 28, face = "italic"),
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "grey10"),
              legend.key.width = unit(1.8,"cm"),
              panel.background = element_rect(fill = "white",
                                              colour = "white",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "grey80"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                              colour = "grey80"),
              axis.text.x = element_text(color = "grey10", size = 24, face = "plain"),
              axis.text.y = element_text(color = "grey10", size = 24, face = "plain"),
              axis.title.x = element_text(color = "grey10", size = 26, face = "italic"),
              axis.title.y = element_text(color = "grey10", size = 26, face = "italic"))

  # @param xvals_mf a numeric 1D vector. The values defining the universe of discourse.
  # @param yvals_mf_A a numeric 1D vector. The values defining the fuzzy set high.
  # @param yvals_mf_B a numeric 1D vector. The values defining the fuzzy set low.

  return(ggp)

}

