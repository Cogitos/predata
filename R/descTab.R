#' Compute and Format a data frame into a table suitable for 
#'   publication with mean and standard deviation into parentheses
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, Universit\'{e} de Montr\'{e}al (Canada)
#' 
#' @param data. A data frame with only the columns to use to compute the
#'  mean and the standard deviation as well as the column to use to
#'  compute by group.
#' @param grp. Optional, the name of the column to use to compute by group.
#' @return Return a data frame by group (if any) with the mean and the
#'  standard deviation into parentheses.
#' @keywords data, descriptive
#' @export
#' @examples
#' # Create data frame faking a group of young and elderly adults. 
#' data = rbind(
#'      data.frame(subject=1:10, 
#'          age=round(rnorm(10, mean=23, sd=1.5), 0), 
#'          education=round(rnorm(10, mean=15, sd=2.1), 0), 
#'          group='young'),
#'      data.frame(subject=11:20, 
#'          age=round(rnorm(10, mean=71, sd=2), 0), 
#'          education=round(rnorm(10, mean=14, sd=1.9), 0), 
#'          group='elderly'))
#'  # Compute the mean and standard deviation of their age and 
#'  #   education. Exclude the subject column to run the function.
#' descTab(data[,-1], grp='group')
descTab = function(data, grp=NULL, toname=NULL){
    if( is.null(grp) ){
        tb_demo = apply( data, 2, strMeanSd )
     }else{
        if( length(grp) > 1 ){
          tb_demo = aggregate(data[, !(names(data) %in% grp)], by=as.list(data[,grp]), FUN='strMeanSd') 
          tb_demo = t(tb_demo) 
        }else{
          tb_demo = aggregate(data[, !(names(data) %in% grp)], by=list(data[,grp]), FUN='strMeanSd')      
        }
    }
    tb_demo = as.data.frame(tb_demo, stringsAsFactors=F)
    return(tb_demo)
}