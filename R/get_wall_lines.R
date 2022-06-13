#' @title Return the main wall lines of the profile
#'
#' @param img a numeric 2D matrix. The binary matrix with the drawing cropped at its bounding box, as a result of get_vessel.bbox.
#' @param m a numeric scalar. The number of pixels checked for correction in the calculation.
#' @param mm a numeric scalar. The amount of pixels allowed to be missing in the potential groove line such that grooves that are not drawn to reach correctly the middle line are included.
#' @param rd a numeric scalar. The rim diameter.
#' @param m9 a numeric scalar. The height of the profile in pixels.
#' @param filenaam a character vector. Name of the image file including extension. The supported extensions are jpeg, png and tiff.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item profile.wall - a numeric 2D matrix. The binary matrix of the profile wall.
#'   \item outer.line - a numeric 1D vector. The outer profile line.
#'   \item inner.line - a numeric 1D vector. The inner profile line.
#'   \item list_potGroov_in - a character vector. The y index of the potential exterior grooves separated by comma.
#'   \item listlines_in - a character vector. The y index of the potential interior grooves separated by comma.
#'   \item list_corr - a character vector. The y index of all lines in the profile reaching the midline separated by comma.
#'   \item has.fullbase - a Boolean. Whether the profile has complete base.
#'   \item fullbase - a character vector. The profiles y index of the base beginning and end separated by a colon.
#'   \item base.height - a numeric scalar. The height of the base in pixels.
#'   \item is.full.profile - a Boolean. Whether the profile is complete.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_wall_lines(img=m7$image_profile,m=10,mm=3,rd=m13$sherd_rimDiam,m9=m9,filenaam='SADR010324.jpg')
get_wall_lines = function(img, m, mm, rd, m9, filenaam){

  xx_range = NULL
  for (ii in 1:(dim(img)[1])) {
    xx_range[ii] = sum(img[ii,1:dim(img)[2]])
  }
  X_min = min(which((xx_range>0) == TRUE))

  xx_pot_cut = which(xx_range == max(xx_range))
  xx_cut = xx_pot_cut[which.min(abs(xx_pot_cut - rd/2))]-1
  data_corrected = img[X_min:xx_cut,]

  corr_X = which(data_corrected[,dim(data_corrected)[2]-1]>0)[length(which(data_corrected[,dim(data_corrected)[2]-1]>0))]
  data_corrected[corr_X:dim(data_corrected)[1],dim(data_corrected)[2]] = 0

  sliceS = 0
  for (i in 1:m){
    slice = data_corrected[dim(data_corrected)[1]-i,]
    sliceS = sliceS + slice
  }
  list_corr = which(sliceS >= 0.9*m)

  is.full.profile = FALSE
  if (length(list_corr) > 0 && list_corr[1] == 1) {is.full.profile = TRUE}

  if (length(list_corr)> 1) {

    block = as.numeric(NULL)
    fullbase = as.numeric(NULL)
    listlines = list_corr

    if (is.full.profile) {
      # check for base.
      listlines1 = as.numeric(list_corr[which(diff(list_corr)>1)])
      listlines2 = as.numeric(list_corr[which(-diff(list_corr[length(list_corr):1])>1)+1])
      listlines = sort(as.numeric(c(listlines1,listlines2)))

      if (length(listlines) > 2) {

        blockcheck = NULL
        blockmargin = NULL
        block = NULL
        for (ci in 1:(length(listlines)-1)){
          blockcheck[ci] = sum(data_corrected[(dim(data_corrected)[1]-m-1):(dim(data_corrected)[1]-1),listlines[ci]:listlines[ci+1]])
          blockmargin[ci] = paste0(listlines[ci],":",listlines[ci+1])
        }
        block = blockmargin[0.5*blockcheck > 2*(m+1)]
        fullbase = as.numeric(unlist(strsplit(block,":")))
      }
    }

  } else {
    block = as.numeric(NULL)
    fullbase = as.numeric(NULL)
    listlines = list_corr
  }

  has.fullbase = FALSE
  base.height = NULL
  if (length(fullbase) > 0) {
    has.fullbase = TRUE
    base.height = fullbase[1]
    list_potGroov = listlines[!(listlines %in% (as.numeric(unlist(strsplit(block,":")))))]
  } else {
    list_potGroov = listlines
  }


  if (length(list_potGroov)>0) {
    for (ci in 1:length(list_potGroov)) {
      if ((list_potGroov[ci] == m9) | (list_potGroov[ci] == (m9-1))) {
        corr_X = which(data_corrected[,list_potGroov[ci]-2]>0)[length(which(data_corrected[,list_potGroov[ci]-2]>0))]
      } else {
        corr_X = which(data_corrected[,list_potGroov[ci]+2]>0)[length(which(data_corrected[,list_potGroov[ci]+2]>0))]
      }
      data_corrected[(corr_X+1):dim(data_corrected)[1],list_potGroov[ci]] = 0
    }
  }

  data_corrected[(dim(data_corrected)[1]-mm):dim(data_corrected)[1],] = 0

  df_outer = NULL
  for (outer in 1:dim(data_corrected)[2]) {
    df_outer[outer] = min(which(data_corrected[,outer]>0))
  }

  df_inner = NULL
  for (inner in 1:dim(data_corrected)[2]) {
    df_inner[inner] = max(which(data_corrected[,inner]>0))
  }

  returns = list("profile.wall" = data_corrected, "outer.line" = df_outer, "inner.line" = df_inner, "list_potGroov_in" = list_potGroov, "listlines_in" = listlines, "list_corr" = list_corr, "has.fullbase" = has.fullbase, "fullbase" = block, "base.height" = base.height, "is.full.profile" = is.full.profile)

  return(returns)

}
