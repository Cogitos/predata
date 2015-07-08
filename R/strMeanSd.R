#' Format means and standard deviations
#' 
#' Very simple function to compute the mean and the standard
#'  deviation of a vector of numbers and format them as 
#'  mean (standard deviation). The number of digits to be kept
#'  could be set for the mean and the standard deviation.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param x A vector of numbers.
#' @param dmean Optional, the number of digits to use in rounding the mean.
#'  Default to 2.
#' @param dsd Optional, the number of digits to use in rounding the
#'  standard deviation. Default to 1.
#' @return Return a string formatted as mean (standard deviation).
#' @keywords data, descriptive, mean, std
#' @export
#' @examples
#' age = c(18, 19, 22, 19, 26, 23, 18, 20, 22, 19)
#' strMeanSd(age)
strMeanSd = function(x, dmean=2, dsd=1){
    m = round(mean(x, na.rm=T), dmean)
    std = round(sd(x, na.rm=T), dsd)
    return(paste(m, ' (', std, ')', sep=''))
}
