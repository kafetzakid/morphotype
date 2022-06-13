#' @title Read input image
#'
#' @description Input a technical drawing as image file and transform it to a numerical matrix with black and white values.
#'
#' @importFrom magick image_read
#' @importFrom magick image_info
#' @importFrom magick image_crop
#' @importFrom magick image_rotate
#' @importFrom magick image_flip
#' @importFrom magick as_EBImage
#'
#' @param filename a character vector. Name of the image file including extension. The supported extensions are jpeg, png and tiff. The image should be provided in gray scale.
#' @param trim a numeric scalar. The pixels the image should be trimmed. This parameter should be larger than the image border. If there is no border trim can be zero.
#' @param thr a numeric scalar. The value of the threshold in (0,1]. A value of one means no threshold is applied. A value of zero means that all values are filtered out. A value of 0.9 is recommended.
#' @param wd a character vector. Directory in which the image file is read from.
#'
#' @details these are details ---- CHANGE and INCLUDE in each function that needs it ----
#'
#' @return A list with the following items:
#' \itemize{
#'   \item matrix - The 2D image matrix.
#'   \item alternative - Whether the image should be checked.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_input(filename='SADR010324.jpg',trim=10,thr=0.9,wd='~/myRpacks/morphotype/inst/extdata')
get_input = function(filename, trim, thr, wd){

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop(
      "Package \"magick\" must be installed to use this function.",
      call. = FALSE
    )
  }

  input_image = magick::image_read(paste0(wd,"/",filename))

  if (trim == 0) {
    input_image = input_image
  } else {
    dimCrop = paste0(magick::image_info(input_image)$width,"x", magick::image_info(input_image)$height, "+", trim, "+", trim)
    input_image_crop1 = magick::image_crop(input_image, dimCrop)
    dimCrop = paste0(magick::image_info(input_image_crop1)$width, "x", magick::image_info(input_image_crop1)$height, "+", trim, "+", trim)
    input_image_crop2 = magick::image_crop(magick::image_rotate(input_image_crop1, 180), dimCrop)
    input_image = magick::image_rotate(input_image_crop2, 180)
    # rm(input_image_crop1, input_image_crop2)
  }

  my_image = magick::as_EBImage(magick::image_flip(input_image))

  check_imgGray = NULL
  check = FALSE
  # check the 'check' code [, , 1] == [, , 2] && [, , 1] == [, , 2] and for [, , 1] == [, , 3]
  if (sum(sum(my_image@.Data[, , 1])) == sum(sum(my_image@.Data[, , 2])) && sum(my_image@.Data[, , 1]) == sum(my_image@.Data[, , 2])) {
    check  = (sum(sum(my_image@.Data[, , 1])) == sum(sum(my_image@.Data[, , 3])) && sum(my_image@.Data[, , 1]) == sum(my_image@.Data[, , 3]))
  }
  # the 'i' in else {check_imgGray = c(check_imgGray,i)} is changed - & - change again c(check_imgGray,*)?
  if (check == TRUE) {image_gray = my_image@.Data[, , 1]} else {check_imgGray = paste0("no grayscale image for ", filename)}

  # image denoised
  if (thr > 0) {
    image_denoised = 1*(image_gray < thr)
  } else {
    image_denoised = 1*(image_gray < 1)
  }

  returns = list("matrix" = image_denoised, "alternative" = check_imgGray)

  return(returns)
}
