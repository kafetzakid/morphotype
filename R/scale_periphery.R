#' @title Scale the rim periphery and compute scaling metadata
#'
#' @importFrom EBImage as.Image
#' @importFrom EBImage resize
#'
#' @param fl a character vector. Name of the image file including extension. The supported extensions are jpeg, png and tiff.
#' @param img a numeric 2D matrix. The binary image matrix as a result of get_input.
#' @param m9 a numeric scalar. The height of the profile.
#' @param m19 a list of 10. Output of get_wall_lines.
#' @param m28 a numeric scalar. The height of the rim profile.
#' @param m38 a list of 3. Output of get_circumference.
#' @param pscl a numeric scalar. The targeted length of the periphery after scaling.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item df_scaled_1D.outline - a dataframe.
#'   \item df_scaled_1D.outline.plot - a dataframe.
#'   \item scaled_1D_fpl - a numeric scalar. The scale factor.
#'   \item scaled_1D_height - a numeric scalar. The scaled rim height.
#'   \item pixel_sc_dif - a numeric scalar. Difference in targeted and actual length of the scaled periphery.
#'   \item scaled_1D_bottom_width - a numeric scalar. The width of the bottom in the scaled rim profile.
#'   \item scaled_1D_lip_width - a numeric scalar. The width of the top in the scaled rim profile.
#' }
#' @export
#'
#' @examples
#' scale_periphery(fl='SADR010324.jpg',img=image_denoised,m9=m9,m19=m19,m28=67,m38=m38,pscl=217)
scale_periphery = function(fl, img, m9, m19, m28, m38, pscl) {

  mlypos = m9 - m28
  df_img_check = EBImage::as.Image(m19$profile.wall[1:max(which(rowSums(m19$profile.wall[,mlypos:m9])>0)),mlypos:m9])
  df_old = m38$data_circumference
  df_old_bottom_width = m38$bottom_width

  # Scaling Process ####
  # compute the scale factor
  df_old_l = df_old[(df_old_bottom_width+1):length(df_old$y),]
  p_i_l = length(df_old_l$y)
  fpl = pscl/p_i_l # for equal periphery (excluding bottom line 1D outline)
  scaled_1D_fpl = fpl
  # multiply the scale factor with the periphery data
  df_old_scaled_line = fpl*df_old_l # for equal periphery (excluding bottom line)

  # 1D outline
  img_rescaled_check_line = EBImage::resize(df_img_check, h = (max(df_old_scaled_line$y) - min(df_old_scaled_line$y) + 1))
  scaled_1D_height = max(df_old_scaled_line$y) - min(df_old_scaled_line$y) + 1

  # 1D outline
  df_img_i_check_line = img_rescaled_check_line@.Data
  df_img_i_bin_check_line = ifelse(df_img_i_check_line>=0.5, 1, 0)

  df_outer_line = NULL
  for (outer in 1:dim(df_img_i_bin_check_line)[2]) {
    df_outer_line[outer] = min(which(df_img_i_bin_check_line[,outer]>0))
  }

  df_inner_line = NULL
  for (inner in 1:dim(df_img_i_bin_check_line)[2]) {
    df_inner_line[inner] = max(which(df_img_i_bin_check_line[,inner]>0))
  }

  m54scaled_line = get_circumference(df_outer_line, df_inner_line)
  scaled_1D_bottom_width = m54scaled_line$bottom_width # save bottom width of scaled data
  scaled_1D_lip_width = m54scaled_line$lip_width # save lip width of scaled data

  # new implementation (for plotting)
  midRrep = df_outer_line[length(df_outer_line)] + round(0.5*(df_inner_line[length(df_inner_line)] - df_outer_line[length(df_outer_line)]),0)
  df_i_sc_plot = c(df_outer_line, rep(midRrep, m54scaled_line$lip_width), df_inner_line[length(df_inner_line):1])
  # new implementation (for computing)
  df_i_sc = c(df_outer_line, df_outer_line[length(df_outer_line)]:df_inner_line[length(df_inner_line)], df_inner_line[length(df_inner_line):1])

  pixel_sc_dif = pscl - length(df_i_sc) # save the difference in the length
  p_sc_dif = ifelse(pixel_sc_dif>0, pixel_sc_dif, 0)

  if (p_sc_dif%/%2 > 0) {
    for (i in 1:(p_sc_dif%/%2)) {
      df_i_sc = c(df_i_sc[1], df_i_sc)
      df_i_sc = c(df_i_sc, df_i_sc[length(df_i_sc)])
      df_i_sc_plot = c(df_i_sc_plot[1], df_i_sc_plot)
      df_i_sc_plot = c(df_i_sc_plot, df_i_sc_plot[length(df_i_sc_plot)])
    }
  }

  if (p_sc_dif%%2 > 0) {
    if(length(df_outer_line) < length(df_inner_line)) {
      df_i_sc = c(df_i_sc, df_i_sc[length(df_i_sc)])
      df_i_sc_plot = c(df_i_sc_plot, df_i_sc_plot[length(df_i_sc_plot)])
    } else {
      df_i_sc = c(df_i_sc[1], df_i_sc)
      df_i_sc_plot = c(df_i_sc_plot[1], df_i_sc_plot)
    }
  }

  # data of computation 1D line
  df_scaled_1D.outline = as.data.frame(df_i_sc - midRrep)
  colnames(df_scaled_1D.outline) = paste0("Prh_Sc_O_",fl)
  df_scaled_1D.outline$Profile_scaled_periphery_data = 1:dim(df_scaled_1D.outline)[1]

  # data of plotting 1D line
  df_scaled_1D.outline.plot = as.data.frame(df_i_sc_plot - midRrep)
  colnames(df_scaled_1D.outline.plot) = paste0("Prh_Sc_O_",fl)
  df_scaled_1D.outline.plot$Profile_scaled_periphery_data_plot = 1:dim(df_scaled_1D.outline.plot)[1]

  returns = list("df_scaled_1D.outline" = df_scaled_1D.outline,
                 "df_scaled_1D.outline.plot" = df_scaled_1D.outline.plot,
                 "scaled_1D_fpl" = scaled_1D_fpl,
                 "scaled_1D_height" = scaled_1D_height,
                 "pixel_sc_dif" = pixel_sc_dif,
                 "scaled_1D_bottom_width" = scaled_1D_bottom_width,
                 "scaled_1D_lip_width" = scaled_1D_lip_width)

  return(returns)

}


