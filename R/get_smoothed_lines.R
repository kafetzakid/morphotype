#' @title Compute the smooth outlines of the profile
#'
#' @importFrom EBImage Image
#' @importFrom EBImage resize
#' @importFrom EBImage filter2
#' @importFrom EBImage combine
#' @importFrom EBImage makeBrush
#' @importFrom EBImage getFrames
#' @importFrom EBImage erode
#' @importFrom reshape2 melt
#' @importFrom stats smooth.spline
#'
#' @param data_profile a numeric 2D vector. The profile binary matrix.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item data_circumference - a numeric 2D vector. The circumference of the profile.
#'   \item data_circumference_eroded - a numeric 2D vector. The eroded circumference of the profile.
#'   \item outer.line.mod - a numeric 2D vector. The smoothed outer profile line.
#'   \item outer.line.mod.cor - a numeric 2D vector. The corrected smoothed outer profile line.
#'   \item bottom.line.mod - a numeric 2D vector. The smoothed bottom outer profile line.
#'   \item bottom.line.mod.cor - a numeric 2D vector. The corrected smoothed bottom outer profile line.
#'   \item fit_splines - an object of class "smooth.spline". The spline model for outer.line.mod.cor fitted with cross-validated knots.
#'   \item fit_splines_max - an object of class "smooth.spline". The spline model for outer.line.mod.cor fitted with maximum knots.
#'   \item fit_splines_bot - an object of class "smooth.spline". The spline model for bottom.line.mod.cor fitted with cross-validated knots.
#'   \item fit_splines_max_bot - an object of class "smooth.spline". The spline model for bottom.line.mod.cor fitted with maximum knots.
#'   \item df_bot - a melted dataframe. The melted data_circumference_eroded.
#'   \item fit_splines.inn - an object of class "smooth.spline". The spline model for df_mod.inn_cor fitted with cross-validated knots.
#'   \item fit_splines.inn_max - an object of class "smooth.spline". The spline model for df_mod.inn_cor fitted with maximum knots.
#'   \item fit_splines.inn_bot - an object of class "smooth.spline". The spline model for df_mod.inn_bot_cor fitted with cross-validated knots.
#'   \item fit_splines.inn_max_bot - an object of class "smooth.spline". The spline model for df_mod.inn_bot_cor fitted with maximum knots.
#'   \item df_mod.inn_cor - a numeric 2D vector. The corrected smoothed inner profile line.
#'   \item df_mod.inn_bot_cor - a numeric 2D vector. The corrected smoothed bottom inner profile line.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_smoothed_lines(data_profile = m19$profile.wall)
get_smoothed_lines = function(data_profile) {

  # add borders
  data_profile = cbind.data.frame(0 , data_profile, 0)
  data_profile = rbind.data.frame(0, data_profile, 0)

  # convert to EBImage
  data_image = EBImage::flip(EBImage::Image(as.matrix(data_profile)))
  img_periphery = EBImage::flip(EBImage::Image(as.matrix(data_profile)))

  ## step 1: from the profile extract the line of circumference
  # edges
  fhi = matrix(1, nrow = 3, ncol = 3)
  fhi[2, 2] = -8
  img_fhi = EBImage::filter2(data_image, fhi)		# make sense of that

  # threshold
  img_fhi = EBImage::Image(img_fhi, colormode = 'Grayscale') # EBImage::colorMode(img_fhi) = Grayscale ;;; then also @importFrom EBImage colorMode
  threshold = 0.5
  data_image_th = EBImage::combine( mapply(function(frame, th) frame > th, EBImage::getFrames(data_image), threshold, SIMPLIFY=FALSE) )


  img_periphery = EBImage::filter2(img_periphery, fhi)
  img_periphery = EBImage::Image(img_periphery, colormode = 'Grayscale') # EBImage::colorMode(img_periphery) = Grayscale
  # img_periphery = 1 - img_periphery
  df_periphery = img_periphery@.Data
  df_periphery[df_periphery[,] > 0.5] = 1
  df_periphery[df_periphery[,] <= 0.5] = 0
  colnames(df_periphery) = NULL
  df_periphery = df_periphery[,colSums(df_periphery)>0]


  ## step 2: form the line of circumference correct roughness using erosion
  # make the kernel
  kern = EBImage::makeBrush(5, shape='diamond') # consider another kernel? ?makeBrush shape = c('box', 'disc', 'diamond', 'Gaussian', 'line')

  # periphery
  img_neg = 1 - img_fhi
  circum_erode = EBImage::erode(img_neg, kern)

  # negate periphery
  input_fft2d = max(circum_erode) - circum_erode

  ## step 3: take the data of the first frame only (here, one frame, so take the data)
  input_fft2d_df = input_fft2d@.Data

  ## step 4: make the data image binary by thresholding
  df = input_fft2d_df
  df[df[,] > 0.5] = 1
  df[df[,] <= 0.5] = 0
  # set colnaems to null
  colnames(df) = NULL

  ## step 5a: clean empty columns
  df = df[,colSums(df)>0]

  ## step 6a: get the data of the outer line only based on the periphery
  df_mod = df[,dim(df)[2]:1] # previously: df_mod = df
  dfY_min = NULL
  dfY_max = NULL
  for (dy in 1:(dim(df_mod)[2])) {
    dfY_min[dy] = min(which((df_mod[,dy]>0) == TRUE))
    if (sum(df_mod[dfY_min[dy]:dim(df_mod)[1],dy]==0)>1) {
      dfY_max[dy] = min(which((df_mod[dfY_min[dy]:dim(df_mod)[1],dy]==0) == TRUE))
      df_mod[(dfY_min[dy]+dfY_max[dy]):dim(df_mod)[1],dy] = 0
    } else { dfY_max[dy] = dim(df_mod)[1] - dfY_min[dy] }
  }

  dfY_max_cor = as.numeric(summary(dfY_max)[5])
  df_mod_cor = df_mod
  for (dy in 1:(dim(df_mod)[2])) {
    if (dfY_max[dy] > dfY_max_cor) {
      df_mod_cor[(dfY_min[dy]+dfY_max_cor):dim(df_mod)[1],dy] = 0
    }
  }
  # dim(df_mod_cor)

  # get inner side projection data
  df_mod.inn_cor = abs(df[,dim(df)[2]:1] - df_mod)


  ## step 5b: clean empty rows    #    >>>>> consider adding triangle data in the bottom or maybe if is not full profile then discard the part after the outer line projection (which is anyway rejected later...?)
  df = df[rowSums(df)>0,]

  ## step 6b: get the data of the bottom line only based on the periphery
  df_bot = df[,dim(df)[2]:1]
  df_bot_mod = df_bot
  dfY_bot_min = NULL
  dfY_bot_max = NULL
  for (dy in 1:(dim(df_bot)[1])) {
    dfY_bot_min[dy] = min(which((df_bot[dy,]>0) == TRUE))
    if (sum(df_bot[dy, (dfY_bot_min[dy]:dim(df_bot)[2])]==0)>1) {
      dfY_bot_max[dy] = min(which((df_bot[dy, dfY_bot_min[dy]:dim(df_bot)[2]]==0) == TRUE))
      df_bot_mod[dy, (dfY_bot_min[dy]+dfY_bot_max[dy]):(dim(df_bot)[2])] = 0
    } else { dfY_bot_max[dy] = dim(df_bot)[2] - dfY_bot_min[dy] }
  }

  dfY_bot_max_cor = as.numeric(summary(dfY_bot_max)[5])
  df_bot_mod_cor = df_bot_mod
  for (dy in 1:(dim(df_bot_mod)[1])) {
    if (dfY_bot_max[dy] > dfY_bot_max_cor) {
      df_bot_mod_cor[dy, (dfY_bot_min[dy]+dfY_bot_max_cor):dim(df)[2]] = 0
    }
  }

  # get inner bottom projection data
  df_mod.inn_bot_cor = abs(df_bot - df_bot_mod)


  ## step 7a: melt data
  xx = reshape2::melt(as.matrix(df_mod_cor))

  ## step 8a: get splines
  data_splines = xx[xx$value > 0, ]
  fit3a = stats::smooth.spline(data_splines$Var2, data_splines$Var1, cv = FALSE) # cv = TRUE

  dfe = ceiling(0.5*length(fit3a$x))
  fit4a = stats::smooth.spline(data_splines$Var2, data_splines$Var1, df = dfe)


  ## step 7b: melt data
  xx = reshape2::melt(as.matrix(df_bot_mod_cor))

  ## step 8b: get splines
  data_splines_bot = xx[xx$value > 0, ]
  fit3b = stats::smooth.spline(data_splines_bot$Var1, data_splines_bot$Var2, cv = FALSE) # cv = TRUE

  dfe = ceiling(0.5*length(fit3b$x))
  fit4b = stats::smooth.spline(data_splines_bot$Var1, data_splines_bot$Var2, df = dfe)

  ## step 7c: melt data
  xx = reshape2::melt(as.matrix(df_mod.inn_cor))

  ## step 8c: get splines
  data_splines_inn = xx[xx$value > 0, ]
  fit3c = stats::smooth.spline(data_splines_inn$Var2, data_splines_inn$Var1, cv = FALSE) # cv = TRUE

  dfe = ceiling(0.5*length(fit3c$x))
  fit4c = stats::smooth.spline(data_splines_inn$Var2, data_splines_inn$Var1, df = dfe)


  ## step 7d: melt data
  xx = reshape2::melt(as.matrix(df_mod.inn_bot_cor))

  ## step 8d: get splines
  data_splines_inn_bot = xx[xx$value > 0, ]
  fit3d = stats::smooth.spline(data_splines_inn_bot$Var1, data_splines_inn_bot$Var2, cv = FALSE) # cv = TRUE

  dfe = ceiling(0.5*length(fit3d$x))
  fit4d = stats::smooth.spline(data_splines_inn_bot$Var1, data_splines_inn_bot$Var2, df = dfe)

  # # plot
  # par(mfrow=c(4,3))
  # # top
  # plot(data_splines$Var1, data_splines$Var2)
  # plot(fit3a$y, fit3a$x, type = "l")
  # plot(fit4a$y, fit4a$x, type = "l")
  # # bottom
  # plot(data_splines_bot$Var1, data_splines_bot$Var2)
  # plot(fit3b$x, fit3b$y, type = "l")
  # plot(fit4b$x, fit4b$y, type = "l")
  # # top inner
  # plot(data_splines_inn$Var1, data_splines_inn$Var2) # upside down but okay
  # plot(fit3c$y, fit3c$x, type = "l")
  # plot(fit4c$y, fit4c$x, type = "l")
  # # bottom inner
  # plot(data_splines_inn_bot$Var1, data_splines_inn_bot$Var2) # upside down but okay
  # plot(fit3d$x, fit3d$y, type = "l")
  # plot(fit4d$x, fit4d$y, type = "l")


  returns = list("data_circumference" = df_periphery, "data_circumference_eroded" = df,  "outer.line.mod" = df_mod, "outer.line.mod.cor" = df_mod_cor, "bottom.line.mod" = df_bot_mod,
                 "bottom.line.mod.cor" = df_bot_mod_cor, "fit_splines" = fit3a, "fit_splines_max" = fit4a, "fit_splines_bot" = fit3b, "fit_splines_max_bot" = fit4b,  "df_bot" = data_splines_bot,
                 "fit_splines.inn" = fit3c, "fit_splines.inn_max" = fit4c, "fit_splines.inn_bot" = fit3d, "fit_splines.inn_max_bot" = fit4d, "df_mod.inn_cor" = df_mod.inn_cor, "df_mod.inn_bot_cor" = df_mod.inn_bot_cor)

  return(returns)

}



