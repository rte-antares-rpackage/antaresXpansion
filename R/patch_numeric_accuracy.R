#' This function update the bender cuts so as to deal with the numeric
#' inaccuracy of the ANTARES output
#' 
#' Marginal costs are returned in ANTARES with a 10^0 accuracy (without any decimal).
#' This can cause numeric imprecisions in the cut of the benders decomposition and
#' make it converge to a non-optimal solution.
#' 
#' This function (tries to) corrects this by updating the constant term of the cuts
#' 
#' @param x_current
#'   vector of current invested capacities
#' @param x_max
#'   vector of maximum possible investments
#' @return 
#' Return correct term to substract to the b of \code{ax+b}
#' 
#' @import assertthat antaresRead
#' @export
#' 
#' 
numeric_patch_cut <- function(x_current, x_max)
{
  # a few checks
  assert_that(length(x_current) == length(x_max))

  # compute max distance between current point (x_current) and boundary of the investment space
  each_max <- function(i,vec1, vec2){max(vec2[i] - vec1[i], vec1[i])}
  d_max <- norm(sapply(1:length(x_current), FUN = each_max, vec1 = x_current, vec2 = x_max), type = "2")
  
  # as error can be up to +/- 0.5 
  return (d_max * 0.5 + 1)
  
}