library(stringr)
library(lmerTest)
library(data.table)
library(haven)


FindSimilarVar <- function(dataFrame, Nlevel = 2)
{
  simVar <- list()
  Already <- function(v)
  {
    for (found in simVar)
      if (v %in% found)
        return(T)
    return(F)
  }
  Similar <- function(v, compare)
  {
    comparisons <- 0
    for (index in 1:min(3, (max(1,str_length(v)-Nlevel))))
    {
      comString <- substring(compare, index, (index+Nlevel)-1)
      if (str_detect(comString, substring(v, index, (index+min(Nlevel,(Nlevel-1))))))
        return(T)
      comparisons <- comparisons + 1
      if (comparisons > 2)
        return(F)

    }
    return(F)

  }
  for(v in names(dataFrame))
  {
    simVector <- c(v)
    if (!Already(v))
    {
      nameList <- names(dataFrame[, -which( colnames(dataFrame)==v)])
      for (compare in nameList)
      {
        if (Similar(v, compare))
        {
          simVector <- c(simVector, compare)
        }
      }
      if (length(simVector) > 1)
        simVar <- append(simVar, list(simVector))
    }

  }

  return(lapply(simVar, FUN = sort))
}
GetIndex <- function(v)
{
  if (is.list(v))
    n <- names(v)
  else
    n <- v
 return(as.numeric(substring(n, str_length(n),str_length(n))))
}

GetMaxLevels<- function(groups)
{
  levelMin <- 10
  levelMax <- 1
  mGroups <- NULL
  for (g in groups)
  {
    for(i in g)
    {
      index <- GetIndex(i)
      if (index < levelMin)
      {
        levelMin <- index
      }
      if (index > levelMax)
      {
        levelMax <-index
      }
    }
  }
  return(c(levelMin, levelMax))


}
AddMissingGroupLevel <- function(data, rows, group, maxLevels)
{
  vIndex <- 1
  newGroup <- group
  newData <- data

  for (index in 1:maxLevels[2])
  {
    level <- GetIndex(group[vIndex])
    if(is.na(level))
      break
    if (level != index)
    {
      d <- list(as.double(rep(NA, rows)))

      names(d) <- paste(substring(group[1], 1, stringr::str_length(group[1])-1), index, sep = "")
      newGroup <- sort(c(newGroup, names(d)))
      newData <- cbind(newData, d)
    }
    else
    {
      vIndex <- vIndex + 1
    }
  }



  return(list(newData, newGroup))
}
library(dplyr)
FindVarNotIn <- function(data, listVar, ids)
{
  varNames <- names(data)
  includedVar <- c()
  for(var1 in varNames)
  {
    for (var2 in listVar)
    {
      if (var1 %in% var2)
      {
        includedVar <- c(includedVar, var1)
        break
      }
    }
  }
  return(setdiff(varNames, includedVar))
}

CreateConstants <- function(data, vars, ids, maxLevels)
{
  for (v in vars)
  {
    newDataSet <- NULL
    newData <- list()
    if (!(v %in% ids))
    {
      newData <- NULL
      newNames <- c()
      for (index in maxLevels[1]:maxLevels[2])
      {
        name <- paste(v, index, sep = "")
        newData <- append(newData, data[v])
        newNames <- c(newNames,name)

      }
      names(newData) <- newNames
      if (is.null(newDataSet))
        newDataSet <- as.data.frame(newData)
      else
        newDataSet <- cbind(newDataSet, as.data.frame(newData))
      data <- select(data, -one_of(v))
      data <- cbind(data, newDataSet)
    }
  }
  return(data)
}

FromWideToLong <- function(mydata, ids,  Nlevel=2,
                           variableName = "Levels",
                           valueNamesP = NULL)
{

  groups <- FindSimilarVar(mydata, Nlevel)
  maxLevels <- GetMaxLevels(groups)
  varNotIn <- FindVarNotIn(mydata, groups, ids)
  mydata <- CreateConstants(mydata, varNotIn, ids, maxLevels)
  groups <-FindSimilarVar(mydata, Nlevel)

  newData <- mydata
  newGroup <- list()
  valueNames <- c()
  for (group in groups)
  {
    withAll <- AddMissingGroupLevel(newData,nrow(mydata), group, maxLevels)
    newData <- withAll[[1]]
    withAllGroup <- withAll[[2]]
    newGroup <- append(newGroup, list(withAllGroup))
      valueNames <- c(valueNames, paste(substring(withAllGroup[1], 1, Nlevel), "Var", sep = ""))
  }
  if (!is.null(valueNamesP))
    valueNames <- valueNamesP
   return(myReshape <-  data.table::melt(setDT(newData),
                                        id.vars = ids,
                                        measure = newGroup,
                                        variable.name = variableName,
                                        value.name = valueNames))

}

