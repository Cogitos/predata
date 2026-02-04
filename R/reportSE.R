#' Report descriptive statistics with standard errors
#' 
#' A function cannibalized and slightly simplified from
#' \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/}
#' 
#' The function compute simple descriptive statistics: number of observations (N), mean, 
#' standard deviation (sd), confident intervals (ci) and standard error (se).
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param data A data frame in the long format
#' @param wid A string indicating the column name of the subjects (or observations)
#' @param within A vector of string of column names of the within variables (repeated within
#'  the same observation)
#' @param between A vector of string of column names of the between variables (across observations)
#' @param dv A string indicating the column name of the dependent variable (what is measured, recorded)
#' @param conf.interval The rate to use to compute the confident interval. Defaults to .95
#' @return Return a data frame of the number of observation (N), mean, standard deviation (sd), 
#' corrected standard error (se), and confident interval (ci) by conditions.
#' @keywords summary
#' @seealso \code{\link{reportWithin}} 
#' @export
#' @examples
#' reportWithin()
reportSE <- function(data=NULL, dv, groupvars=NULL, conf.interval=.95){

  # Compute the N, mean and the standard deviation (sd) per condition
  dots <- list(interp(~ length(na.omit(var)), var = as.name(dv)),
               interp(~ sd(var, na.rm=T), var = as.name(dv)),
               interp(~ mean(var, na.rm=T), var = as.name(dv)))
  data.sum = data %>% 
              group_by(.dots = groupvars) %>%
              summarise(.dots = setNames(dots, c("N", 'sd', dv)))
    
  # Calculate standard error of the mean
  data.sum$se <- data.sum$sd / sqrt(data.sum$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, data.sum$N-1)
  data.sum$ci <- data.sum$se * ciMult
  
  return(data.sum)
}
