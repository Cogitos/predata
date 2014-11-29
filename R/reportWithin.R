#' Report descriptive statistics with within subjects' standard errors
#' 
#' A function cannibalized and slightly simplified from
#' \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/}
#' 
#' As reportSE, the function compute simple descriptive data: number of observations (N), mean, 
#'  standard deviation (sd), confident intervals (ci) and standard error (se). 
#'  The main difference is that reportWithin compute the standard error to handle within subjects
#'  design. The function normalized the data to remove subjects variability and report intra-subject
#'  variability (see Loftus & Masson, 1994). 
#' 
#' Within standard errors are computed according the method described by Cousineau (2005) 
#'  and corrected with the method of Morey (2008).
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, University of Montreal (Canada)
#' 
#' @param data A data frame in the long format
#' @param wid A string indicating the column name of the subjects (or observations)
#' @param within A vector of string of column names of the within variables 
#'  (repeated within the same observation)
#' @param between A vector of string of column names of the between variables (across observations)
#' @param dv A string indicating the column name of the dependent variable 
#'  (what is measured, recorded)
#' @param conf.interval The rate to use to compute the confident interval. Defaults to .95
#' @return Return a data frame of the number of observation (N), mean, standard deviation (sd), 
#'  corrected standard error (se), and confident interval (ci) by conditions.
#' @keywords summary
#' @export
#' @seealso \code{\link{reportSE}} 
#' @examples
#' reportWithin()
#' @references 
#' Loftus, G.R., & Masson, M.E.J. (1994). Using confidence intervals in within-subject designs. Psychonomic Bulletin & Review, 1(4), 476–490.
#' 
#' Cousineau, D. (2005). Confidence intervals in within‐subject designs:A simpler solution to Loftus and Masson’s method. Tutorials in Quantitative Methods for Psychology, 1, 42-45.
#' 
#' Morey, R. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). Tutorial in Quantitative Methods for Psychology, 4(2), 61–64.

reportWithin  <- function(data=NULL, wid=NULL, within=NULL,
                          between=NULL, dv=NULL, conf.interval=.95){
    
    ### PREPARE THE DATA ------------------------------------------------------
    # Ensure that the between and within are factors 
    factorvars <- vapply(data[, c(between, within), drop = FALSE],
                         FUN = is.factor, FUN.VALUE = logical(1))

    # If not convert them to factor
    if (!all(factorvars)) {
      nonfactorvars <- names(factorvars)[!factorvars]
      message("Automatically converting the following non-factors to factors: ",
              paste(nonfactorvars, collapse = ", "))
      data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
    }

    # Compute the N and mean per condition
    dots <- list(interp(~ length(na.omit(var)), var = as.name(dv)),
                 interp(~ mean(var, na.rm=T), var = as.name(dv)))
    data.sum = data %>% 
        group_by_(.dots = c(between,within)) %>%
        summarise_(.dots = setNames(dots, c("N", dv)))


    ### NORMALIZE DATA --------------------------------------------------------
    # Compute the mean per subject across the between conditions
    subjMean = data %>%
        group_by_(.dots = c(wid, between)) %>%
        summarise_( subjMean = interp(~ mean(var, na.rm=T), var = as.name(dv)))

    # Put the subject means with original data
    ndata <- merge(data, subjMean)

    # Get the normalized data in a new column
    dv_norm <- paste(dv, "_norm", sep="")
    ndata[,dv_norm] <- ndata[,dv] - ndata[,"subjMean"] +
      mean(ndata[,dv], na.rm=T)

    # Remove the subject mean column
    ndata$subjMean <- NULL
  
    # Collapse the normed data - now we can treat between and within vars the same
    ndata.sum <- reportSE(ndata, dv_norm, groupvars = c(between, within),
                           conf.interval = conf.interval)

    
    ### CORRECTION OF MOREY (2008) FOR WITHIN DATA STANDARD ERRORS ------------
    # Get the product of the number of conditions of within-S variables
    ndata.sum = as.data.frame(ndata.sum)
    nWithinGroups    <- prod(vapply(ndata.sum[,within, drop = FALSE], FUN = nlevels,
                                    FUN.VALUE = numeric(1)))
    correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

    # Apply the correction factor
    ndata.sum$sd <- ndata.sum$sd * correctionFactor
    ndata.sum$se <- ndata.sum$se * correctionFactor
    ndata.sum$ci <- ndata.sum$ci * correctionFactor

    
    # Combine the un-normed means with the normed results
    return(merge(data.sum, ndata.sum))
}