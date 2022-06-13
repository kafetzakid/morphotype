#' @title Compute the angle between two points of the profile
#'
#' @param profile.wall a numeric 2D matrix. The binary matrix of the profile wall.
#' @param m9 a numeric scalar. The height of the profile in pixels.
#' @param m28 a numeric scalar. The height of the profile segment in pixels.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item Rim_incl_min - Sine value for the inclination measured at the most outer wall coordinates of the bottom and top of the profile segment.
#'   \item Rim_incl_max - Sine value for the inclination measured at the most inner wall coordinates of the bottom and top of the profile segment.
#'   \item Rim_incl_mean - Sine value for the inclination measured at the middle coordinates of the bottom and top of the profile segment.
#'   \item Rim_incl_min_sign - Sign of the Rim_incl_min, coded as -1 for negative, 1 for positive and 0 for 90 degrees angle.
#'   \item Rim_incl_max_sign - Sign of the Rim_incl_max, coded as -1 for negative, 1 for positive and 0 for 90 degrees angle.
#'   \item Rim_incl_mean_sign - Sign of the Rim_incl_mean, coded as -1 for negative, 1 for positive and 0 for 90 degrees angle.
#'   \item Rim_incl_distance_min - Actual length in pixels of the conceivable hypotenuse for the triangle measuring Rim_incl_min.
#'   \item Rim_incl_distance_max - Actual length in pixels of the conceivable hypotenuse for the triangle measuring Rim_incl_max.
#'   \item Rim_incl_distance_mean - Actual length in pixels of the conceivable hypotenuse for the triangle measuring Rim_incl_mean.
#'
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_angle_rim(profile.wall = m19$profile.wall, m9 = m9, m28 = 67)
get_angle_rim = function(profile.wall, m9, m28){

  incl_Y_top = m9
  incl_Y_bot = m9 - m28
  incl_X_top = min(which(profile.wall[,m9] > 0)) # changed dim(profile.wall)[2] to m9 in all three incl_X_top
  incl_X_bot = min(which(profile.wall[,incl_Y_bot] > 0))
  Rim_incl_distance_min = ceiling(sqrt((incl_X_top - incl_X_bot)^2 + (incl_Y_top - incl_Y_bot)^2))
  Rim_incl_min = m28/Rim_incl_distance_min # use sign as well?
  Rim_incl_min_sign = sign(incl_X_top - incl_X_bot) # if = 1 then there is inner inclination, if = -1 then there is outer inclination

  incl_X_top = max(which(profile.wall[,m9] > 0))
  incl_X_bot = max(which(profile.wall[,incl_Y_bot] > 0))
  Rim_incl_distance_max = ceiling(sqrt((incl_X_top - incl_X_bot)^2 + (incl_Y_top - incl_Y_bot)^2))
  Rim_incl_max = m28/Rim_incl_distance_max
  Rim_incl_max_sign = sign(incl_X_top - incl_X_bot)

  incl_X_top = mean(which(profile.wall[,m9] > 0))
  incl_X_bot = mean(which(profile.wall[,incl_Y_bot] > 0))
  Rim_incl_distance_mean = ceiling(sqrt((incl_X_top - incl_X_bot)^2 + (incl_Y_top - incl_Y_bot)^2))
  Rim_incl_mean = m28/Rim_incl_distance_mean
  Rim_incl_mean_sign = sign(incl_X_top - incl_X_bot)

  returns = list("Rim_incl_min" = Rim_incl_min, "Rim_incl_max" = Rim_incl_max, "Rim_incl_mean" = Rim_incl_mean, "Rim_incl_min_sign" = Rim_incl_min_sign, "Rim_incl_max_sign" = Rim_incl_max_sign, "Rim_incl_mean_sign" = Rim_incl_mean_sign, "Rim_incl_distance_min" = Rim_incl_distance_min, "Rim_incl_distance_max" = Rim_incl_distance_max, "Rim_incl_distance_mean" = Rim_incl_distance_mean)

  # > it was min() before so the outside inclination = 1
  # > we could also estimate a LS model
  # > for below the rim, we could also check whether there is direction change in the wall.

  return(returns)
  # return degrees: https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r

}
