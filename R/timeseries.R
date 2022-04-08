#' Centered moving average to smooth out a time series
#'
#' @param y a vector with time series data
#' @param r the number of observations to the left of the center in the average,
#' i.e. the function computes a 2r+1 point average.
#' @param plotfig if TRUE then a figure is plotted with data and moving average.
#' @return a vector of the same length as y with moving averages (NA at boundaries)
#' @export
#' @examples
#' library(regkurs)
#' M = moving_average(c(AirPassengers), 2)
moving_average <- function(y, r, plotfig = TRUE){
  n = length(y)
  M = rep(NA,n)
  for (t in (r+1):(n-r)){
    M[t] = mean(y[(t-r):(t+r)])
  }
  if (plotfig){
    plot(y, type = "l", col = prettycol[2])
    lines(M, type = "l", col = prettycol[4], lwd = 2)
  }
  return(M)
}

#' Moving average to smooth out seasonal time series
#'
#' @param y a vector with time series data
#' @param season season = 12 for montly, season = 4 for quarterly etc
#' @param plotfig if TRUE then a figure is plotted with data and moving average.
#' @return a vector of the same length as y with moving averages (NA at boundaries)
#' @export
#' @examples
#' library(regkurs)
#' M = moving_average_seasonal(c(AirPassengers), season = 12)
moving_average_seasonal <- function(y, season, plotfig = TRUE){
  w = rep(1/season, season + 1)
  w[1] = w[1]/2
  w[season + 1] = w[season + 1]/2
  r = season/2
  n = length(y)
  M = rep(NA,n)
  for (t in (r+1):(n-r)){
    M[t] = sum(w*y[(t-r):(t+r)])
  }
  if (plotfig){
    plot(y, type = "l", col = prettycol[2])
    lines(M, type = "l", col = prettycol[4], lwd = 2)
  }
  return(M)
}

#' Manipulate version of centered moving average to smooth out a time series
#'
#' @param y a vector with time series data
#' @export
#' @examples
#' library(regkurs)
#' moving_average_manip(c(AirPassengers))
moving_average_manip <- function(y){
  manipulate::manipulate(
    moving_average(y, r),
    r = manipulate::slider(1, 20, step=1, initial = 1)
  )
}
