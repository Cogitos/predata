#' Format Descriptive Table
#' 
#' Compute and format a data frame into a table suitable for 
#'   publication with means and standard deviations into parentheses.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param data A data frame with only the columns to use to compute the
#'  mean and the standard deviation as well as the column to use to
#'  compute by group.
#' @param grp Optional, the name of the column to use to compute by group.
#' @param frq Optional, the name of the column to use to compute frequency
#'  tables, typically for gender. Default to NULL.
#' @param n Optional, logical value to compute or not the number of 
#'  observations per group. Default to TRUE.
#' @return Return a data frame by group (if any) with the mean and the
#'  standard deviation into parentheses.
#' @keywords data, descriptive, summary, table
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
#'          
#'  # Compute the mean and standard deviation of their age and 
#'  #   education. Exclude the subject column to run the function.
#' descTab(data[,-1], grp='group')
#' 
#' # Addition of the frequency table for the gender
#' descTab(data[,-1], grp='group', frq='sex')
#' 
#' # Improve the label of the gender factor    
#' data$sex = factor(data$sex, labels=c('M','F'))
#' descTab(data[,-1], grp='group', frq='sex')
descTab = function(data, grp=NULL, frq=NULL, n=T){
    # Ensure data is a data frame
    data = as.data.frame(data)
    # Compute the number of observations if any
    if( n ){
      if( length(grp) > 1 ){
        tb_n = aggregate(data[,1], by=as.list(data[, grp]), FUN='length')  
        names(tb_n) = c(grp, "n")
      }else{
        if( is.null(grp)  ){
          tb_n = nrow(data)
          names(tb_n) = 'n'
        }else{
          tb_n = aggregate(data[,1], by=list(data[, grp]), FUN='length')      
          names(tb_n) = c(grp, "n")
        }
      }
    }
    # Compute the frequency tables if any
    if( !is.null(frq) ){
        # Compute the frequency tables for each frq variable
        frqtb = lapply(frq, function(x) strFrqTable(data, x, vars=grp) )
        names(frqtb) = frq
        # Reduce the list into one data frame
        frqtb = Reduce(merge, frqtb)
        # Remove frq variable from the data frame for the next steps
        data = data[ , -which(names(data) %in% frq), drop=FALSE ]
    }
    # Compute the mean and sd for all other variables
    if( is.null(grp) ){
        tbl = apply( data, 2, strMeanSd )
     }else{
        if( length(grp) > 1 ){
          tbl = aggregate(data[, !(names(data) %in% grp)], by=as.list(data[,grp]), FUN='strMeanSd')  
        }else{
          tbl = aggregate(data[, !(names(data) %in% grp)], by=list(data[,grp]), FUN='strMeanSd')      
        }
        names(tbl) = c(grp, names(data)[-which(names(data) %in% grp)])
     }
    tb_demo = as.data.frame(tbl, stringsAsFactors=F)
    if( !is.null(frq) ){ tb_demo = merge(tb_demo, frqtb) }
    if( n ){ tb_demo = merge(tb_n, tb_demo) }
    if( is.null(grp) ){
        rownames(tb_demo) = names(data)
        names(tb_demo) = c("n", "Moy (ET)", "F/M")
    }
    return(tb_demo)
}
