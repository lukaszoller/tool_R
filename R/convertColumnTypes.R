#' Titel
#'
#' Untertitel
#'
#' @description "Converts all columns of a data frame with type X to type Y."
#' @param inputDF A data.frame
#' @param searchType Type of the columns which have to be converted.
#' @param targetType Type to which the columns with type "searchType" will be converted
#' @return inputDF as a data.table
#' @examples \dontrun{dt=dfTodt(as.data.frame(matrix(1:100, ncol=10)))}



#
convertColumnTypes <- function(inputDF, searchType, targetType) {

  searchTypeList <- list(is.character, is.double, is.factor, is.integer, is.numeric, is.logical)
  targetTypeList <- list(as.character, as.double, as.factor, as.integer, as.numeric, as.logical)


  # Check correct input
  searchTypeOK <- FALSE
  targetTypeOK <- FALSE
  for (typeVar in searchTypeList) {
     searchTypeOK <- identical(searchType, typeVar) | searchTypeOK
  }
  if(!searchTypeOK) {stop("searchType input is not a correct Type")}

  for (typeVar in searchTypeList) {
    targetTypeOK <- identical(targetType, typeVar) | searchTypeOK
  }
  if(!searchTypeOK) {stop("targetType input is not a correct Type")}



  if (!is.data.frame(inputDF)) {stop("inputDF is not a data.frame.")}

  # Check wrong combinations
    # character --> numeric
    if(identical(searchType, is.character) && identical(targetType, as.numeric)) {stop("Cannot convert character to numeric.")}



  indx <- sapply(inputDF, searchType)
  inputDF[indx] <- lapply(inputDF[indx], function(x) targetType(x))

  return(inputDF)
  }
