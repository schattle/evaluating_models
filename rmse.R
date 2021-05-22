#' Root Mean Squared Function
#'
#' Compute RMS to find square root of the average of errors. The RMSE is the standard deviation of the prediction (modeled) errors, so it tell you how concentrated the data is around the line of best fit.
#' 
#' @param  m  model estimates
#' @param  o  observations
#' @return rmse


rmse = function(m,o) {
  rmse = sqrt((sum(o) - sum(m))^2 / length(o))

  return(rmse)
}
