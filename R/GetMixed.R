#' Create and return an object of class Mixed
#'
#' @param dataset A dataset with more than one level
#' @param validLetters How many letters in variable names that are significant
#' @param variableName Name of the variable having the level number
#' @param valueNamesP An optional list with variable names for the variables
#'
#' @return an object of class Mixed to use with lme4
#' @export
#'
#' @examples
#' obj <- GetMixed(MixedModelData[c("ID","Gender","NeAutT1","NeComT1","HWTeachT1",
#' "NeComT2","HWTeachT2","NeAutT3","NeComT2","HWTeachT3")], validLetters = 4,
#' level)

GetMixed <- function(dataset,
                     validLetters=2,
                     Ids = "ID",
                     variableName = "Levels",
                     valueNamesP = NULL)
{
  o <- list(data = as.data.frame(FromWideToLong(dataset, Ids, Nlevel = validLetters, variableName, valueNamesP)))
  class(o) <- "Mixed"
  return(o)
}

#' Save for use in other programs
#'
#' @param obj A Mixed object created by GetMixed
#' @param path A path to where the data should be saved
#'
#' @return an object of type Mixed
#' @export
save.Mixed <- function(obj, path)
{
  write.table(obj$data, path, sep = ",", col.names = T, row.names = F)
}

#' Get the the long data
#'
#' @param object a Mixed object created by GetMixed
#'
#' @return a dataframe with long format
#' @export
GetData.Mixed <- function(object)
{
  UseMethod("GetData.Mixed", object)
}

#' Get the the long data
#'
#' @param object a Mixed object created by GetMixed
#'
#' @return a dataframe with long format
#' @export
GetData.Mixed <- function(object)
{
  return(object$data)
}

