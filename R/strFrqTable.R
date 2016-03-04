#' Format Frequency Tsables
#' 
#' Simple function to compute the frequency table of a given factor
#'  according to some variables (optional). 
#' Return a data frame with the frequencies concatenate together.
#' It is of your responsability to identify (or rename) the levels
#'  of the target variable. 
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param dt A data frame to process.
#' @param target A string of the column name to target 
#'   (i.e. the one used to compute the frequency table).
#' @param vars Optional, a string or vector of strings 
#'   of the column names of the variables to consider when
#'   computing the frequency table.
#' @param sep_symbol Optional, the symbol to use in the 
#'   concatenation of the frequencies. Default to "/".  
#' @return Return a data frame with the frequencies concatenated
#'   in a string separated by the sep_symbol argument (default 
#'   to '/').
#' @keywords data, descriptive, frequency table
#' @export
#' @examples
#' # Create data frame faking a group of young and elderly adults. 
#' data = rbind(
#'      data.frame(subject=1:10, 
#'          age=round(rnorm(10, mean=23, sd=1.5), 0), 
#'          education=round(rnorm(10, mean=15, sd=2.1), 0),
#'          sex=sample(1:2, size=10, replace=T, prob=c(.6,.4)),
#'          group='young'),
#'      data.frame(subject=11:20, 
#'          age=round(rnorm(10, mean=71, sd=2), 0), 
#'          education=round(rnorm(10, mean=14, sd=1.9), 0), 
#'          sex=sample(1:2, size=10, replace=T, prob=c(.4,.6)),
#'          group='elderly'))
#' # Compute the frequency table for the gender variable.
#' strFrqTable(data, 'sex', sep_symbol="-")
#' # Compute the frequency table for the gender variable by group.
#' strFrqTable(data, target='sex', vars='group')
strFrqTable = function(dt, target, vars=NULL, sep_symbol="/"){
  # Ensure that target column is a factor 
  if( !is.factor(dt[,target]) ){
    dt[,target] = factor(dt[,target])
    warning("Converting the target column into a factor")
  }
  
  if( is.null(vars) ){
    tb = table( dt[,target] )
    tb = paste( tb, collapse=sep_symbol )
    names(tb) = paste( levels(dt[,target]), collapse=sep_symbol, sep="" )
  }else{
    if( length(vars)==1 ){  
      tb = aggregate( dt[,target], by=list(dt[,vars]), FUN=table )
    }else{
      tb = aggregate(dt[,target], by=as.list(dt[,vars]), FUN=table)  
    }
    frqtb = apply( tb, 1, function(x) paste(x[-c(1:length(vars))], collapse=sep_symbol, sep="") ) 
    tb[,length(vars)+1] = gsub( " ", "", frqtb)
    names(tb) = c( vars, paste(levels(dt[,target]), collapse=sep_symbol) )
  }
  return( tb )
}