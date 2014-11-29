#' Load multiple csv files into a unique data frame
#' 
#' A function to load all csv files within a given directory and merge data into a unique data frame.
#' 
#' @author
#' Guillaume T. Vallet \email{gtvallet@@gmail.com}, Universit\'{e} de Montr\'{e}al (Canada)
#' 
#' @param folder A string indicating the relative path of the folder 
#'  in which are stored the csv files
#' @param targetCol A vector of numbers to precise which columns
#'  should be read. Defaults to all columns.
#' @param separator A string indicating the symbol use to
#'  differentiate the columns (, ; \\t). Defaults to ";".
#' @param toSkip A number to indicate how many lines (rows) should be
#'  ignored before reading the file. Defaults to 0.
#' @param rowToRead A number to indicate how many lines (rows) should
#'  be read from the starting reading line. Defaults to all lines.
#' @param addSubj Either a vector of 2 numbers to indicate the first 
#'  and last character index of the csv file name to extract the
#'   subject name or a boolean index to TRUE to add a subject number
#'   equal to the process order of the file. Defaults to NULL to
#'   indicate that a subject identification is present.
#' @return Return a data frame with all data bind by rows.
#' @keywords data, csv, files
#' @export
#' @examples
#' getDataFiles()

getDataFiles = function(folder, targetCol=NULL, separator=";",
                        toSkip=0, rowToRead=-1, addSubj=NULL){
  # GTV - 06/12/2012 - v01

  ### READ THE FILES NAMES ----------------------------------------------------
  files  = sort( list.files(path=folder) )
  # Check if there is some files in folder
  if(length(files) == 0) stop("No files found! Please check the folder path.")
  
  ### INITIATE THE FINAL DATAFRAME AND THE SUBJECT INDEX ----------------------
  idx  = 1
  data = NULL

  ### LOOP TO READ EACH CSV FILE INTO A UNIQUE DATA FRAME ---------------------
  for( cur_file in files ) 
  {
    file_path =  paste(folder, '/', cur_file, sep="")
    file = read.table(file_path, header= TRUE, sep= separator, dec= ".", skip=toSkip, nrows=rowToRead)
    if( is.null(targetCol) ){
      temp = file
    }else{
      temp = file[ , targetCol]
    }
    if( !is.null(addSubj) ){
      if( length(addSubj)==2 ){
          temp = cbind(substr(cur_file, addSubj[1], addSubj[2]), temp)    
      }else if( addSubj == TRUE ){
          temp = cbind(idx, temp)    
      }
    }
    data = rbind(data, temp)
    idx  = idx+1
  }
  if( !is.null(addSubj) ){
    colnames(data)[1] = "Subjects"
  }
  
  return( data )
}
