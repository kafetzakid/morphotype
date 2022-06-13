#' @title Compute rim measures of protuberance and difference length
#'
#' @param m9 a numeric scalar. The height of the profile.
#' @param m28 a numeric scalar. The height of the rim profile.
#' @param outL a numeric 1D vector. The outer profile line.
#' @param innL a numeric 1D vector. The inner profile line.
#' @param all_WT a numeric 1D vector. The wall thickness in pixels across the height of the profile wall.
#'
#' @return A list with the following items:
#' \itemize{
#'   \item rim.height - a numeric scalar. The height of the rim profile.
#'   \item rim.out.prot.length - a numeric scalar. The outer protuberance length of the rim profile.
#'   \item rim.inn.prot.length - a numeric scalar. The inner protuberance length of the rim profile.
#'   \item rim.out.diff.length - a numeric scalar. The outer difference length of the rim profile.
#'   \item rim.inn.diff.length - a numeric scalar. The inner difference length of the rim profile.
#'   \item rim.out.max.diff.length - a numeric scalar. The maximum outer difference length of the rim profile.
#'   \item rim.inn.max.diff.length - a numeric scalar. The maximum inner difference length of the rim profile.
#'   \item rim.out.prot.length.2 - a numeric scalar. The alternative outer protuberance length of the rim profile.
#'   \item rim.inn.prot.length.2 - a numeric scalar. The alternative inner protuberance length of the rim profile.
#' }
#' @export
#'
#' @author Danai Kafetzaki
#'
#' @examples
#' get_rim_measures(m9=m9, m28=67, outL=m19$outer.line, innL=m19$inner.line, all_WT=m21$all_WT)
get_rim_measures = function(m9, m28, outL, innL, all_WT) {

  rim.height = 9999
  rim.out.diff.length = 9999
  rim.inn.diff.length = 9999
  rim.out.max.diff.length = 9999
  rim.inn.max.diff.length = 9999
  rim.out.prot.length = 9999
  rim.inn.prot.length = 9999
  rim.out.prot.length.2 = 9999
  rim.inn.prot.length.2 = 9999

  if (length(m28) > 0 && m28 != 9999 && m28 > 0) {            # m28 > 1 ?
    rim.height = m28  # m9 - m28
    rim.out.max.diff.length = max(outL[(m9 - rim.height):m9]) - min(outL[(m9 - rim.height):m9])
    rim.inn.max.diff.length = max(innL[(m9 - rim.height):m9]) - min(innL[(m9 - rim.height):m9])
  }

  # incurving rim? --> incl_X_top.inn > incl_X_bot.inn --> rim.inn.diff.length
  # thinning?

  rim.WT.all = all_WT[(m9 - m28):m9]
  indx.max.rim.WT = length(rim.WT.all) + 1 - which.max(rim.WT.all[length(rim.WT.all):1]) # the highest point on the rim with the maximum WT

  # outer protuberance
  incl_Y_top = m28
  incl_Y_bot = 1
  incl_X_top = outL[m9]
  incl_X_bot = outL[(m9 - rim.height)]
  Rim.out.base = sqrt((incl_X_top - incl_X_bot)^2 + (incl_Y_top - incl_Y_bot)^2) # base of triangle, equal to Rim_incl_distance

  rim.out.diff.length = incl_X_bot - incl_X_top

  # Protuberance using the outest point
  incl_X_out = min(outL[(m9 - rim.height):m9])
  incl_Y_out = max(which(outL[(m9 - rim.height):m9] ==  incl_X_out)) # by using max, the most bottom point is used.

  Rim.out.side.top = sqrt((incl_X_top - incl_X_out)^2 + (incl_Y_top - incl_Y_out)^2) # side of triangle that connects top and outest point (top vertex)
  Rim.out.side.bot = sqrt((incl_X_bot - incl_X_out)^2 + (incl_Y_bot - incl_Y_out)^2) # side of triangle that connects bottom and outest point (top vertex)

  half.P.out = (Rim.out.base + Rim.out.side.top + Rim.out.side.bot)/2
  if (round(half.P.out,3) == round(Rim.out.base,3) | round(half.P.out,3) == round(Rim.out.side.top,3) | round(half.P.out,3) == round(Rim.out.side.bot,3)) {
    Rim.out.area = 0
  } else {
    Rim.out.area = sqrt((half.P.out*(half.P.out - Rim.out.base)*(half.P.out - Rim.out.side.top)*(half.P.out - Rim.out.side.bot)))
  }

  rim.out.prot.length = (2*Rim.out.area)/Rim.out.base # height of the triangle

  # Protuberance using the maximum WT point
  incl_X_out = outL[(m9 - m28):m9][indx.max.rim.WT]
  incl_Y_out = indx.max.rim.WT # top point with max WT

  Rim.out.side.top = sqrt((incl_X_top - incl_X_out)^2 + (incl_Y_top - incl_Y_out)^2) # side of triangle that connects top and outest point (top vertex)
  Rim.out.side.bot = sqrt((incl_X_bot - incl_X_out)^2 + (incl_Y_bot - incl_Y_out)^2) # side of triangle that connects bottom and outest point (top vertex)

  half.P.out = (Rim.out.base + Rim.out.side.top + Rim.out.side.bot)/2
  if (round(half.P.out,3) == round(Rim.out.base,3) | round(half.P.out,3) == round(Rim.out.side.top,3) | round(half.P.out,3) == round(Rim.out.side.bot,3)) {
    Rim.out.area = 0
  } else {
    Rim.out.area = sqrt((half.P.out*(half.P.out - Rim.out.base)*(half.P.out - Rim.out.side.top)*(half.P.out - Rim.out.side.bot)))
  }

  rim.out.prot.length.2 = (2*Rim.out.area)/Rim.out.base # height of the triangle


  # inner protuberance
  incl_Y_top = m28
  incl_Y_bot = 1
  incl_X_top = innL[m9]
  incl_X_bot = innL[(m9 - rim.height)]
  Rim.inn.base = sqrt((incl_X_top - incl_X_bot)^2 + (incl_Y_top - incl_Y_bot)^2) # base of triangle, equal to Rim_incl_distance

  rim.inn.diff.length = incl_X_bot - incl_X_top # indication for incurving rim / also work with the inclination?

  # Protuberance using the most inner point
  incl_X_inn = max(innL[(m9 - rim.height):m9])
  incl_Y_inn = max(which(innL[(m9 - rim.height):m9] ==  incl_X_inn))

  Rim.inn.side.top = sqrt((incl_X_top - incl_X_inn)^2 + (incl_Y_top - incl_Y_inn)^2) # side of triangle that connects top and outest point (top vertex)
  Rim.inn.side.bot = sqrt((incl_X_bot - incl_X_inn)^2 + (incl_Y_bot - incl_Y_inn)^2) # side of triangle that connects bottom and outest point (top vertex)

  half.P.inn = (Rim.inn.base + Rim.inn.side.top + Rim.inn.side.bot)/2
  if (round(half.P.inn,3) == round(Rim.inn.base,3) | round(half.P.inn,3) == round(Rim.inn.side.top,3) | round(half.P.inn,3) == round(Rim.inn.side.bot,3)) {
    Rim.inn.area = 0
  } else {
    Rim.inn.area = sqrt((half.P.inn*(half.P.inn - Rim.inn.base)*(half.P.inn - Rim.inn.side.top)*(half.P.inn - Rim.inn.side.bot)))
  }

  rim.inn.prot.length = (2*Rim.inn.area)/Rim.inn.base # height of the trangle

  # Protuberance using the maximum WT point
  incl_X_inn = innL[(m9 - m28):m9][indx.max.rim.WT]
  incl_Y_inn = indx.max.rim.WT # top point with max WT

  Rim.inn.side.top = sqrt((incl_X_top - incl_X_inn)^2 + (incl_Y_top - incl_Y_inn)^2) # side of triangle that connects top and outest point (top vertex)
  Rim.inn.side.bot = sqrt((incl_X_bot - incl_X_inn)^2 + (incl_Y_bot - incl_Y_inn)^2) # side of triangle that connects bottom and outest point (top vertex)

  half.P.inn = (Rim.inn.base + Rim.inn.side.top + Rim.inn.side.bot)/2
  if (round(half.P.inn,3) == round(Rim.inn.base,3) | round(half.P.inn,3) == round(Rim.inn.side.top,3) | round(half.P.inn,3) == round(Rim.inn.side.bot,3)) {
    Rim.inn.area = 0
  } else {
    Rim.inn.area = sqrt((half.P.inn*(half.P.inn - Rim.inn.base)*(half.P.inn - Rim.inn.side.top)*(half.P.inn - Rim.inn.side.bot)))
  }

  rim.inn.prot.length.2 = (2*Rim.inn.area)/Rim.inn.base # height of the trangle


  returns = list("rim.height" = rim.height, "rim.out.prot.length" = rim.out.prot.length, "rim.inn.prot.length" = rim.inn.prot.length, "rim.out.diff.length" = rim.out.diff.length, "rim.inn.diff.length" = rim.inn.diff.length, "rim.out.max.diff.length" = rim.out.max.diff.length, "rim.inn.max.diff.length" = rim.inn.max.diff.length, "rim.out.prot.length.2" = rim.out.prot.length.2, "rim.inn.prot.length.2" = rim.inn.prot.length.2)

  return(returns)

}

