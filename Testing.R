


names(MixedModelTomas)
names(MixedModelTomas) <- c("ID","Gender","Class","PartAge","NeAutT1","NeComT1","HWTeachT1","HWPeerT2","HWParT1",
                            "AutT1","ContT1","AMotT1","NeAutT2","NeComT2","HWTeachT2","HWPeerT1","HWParT2","AutT2",
                            "ContT2","AMotT2","NeAutT3","NeComT3","HWTeachT3","HWParT3","HWPeerT3","AutT3","ContT3",
                            "AMotT3","IdrottBetygT1","MatteBetygT1","SpråkBetygT1","SvenskaBetygT1","PoängT1","IdrottBetygT2","MatteBetygT2","SpråkBetygT2",
                            "SvenskaBetygT2","PoängT2","IdrottBetygT3","MatteBetygT3","SpråkBetygT3","SvenskaBetygT3","PoängT3")
debugonce(FindSimilarVar)
res <-FindSimilarVar(MixedModelTomas[c("ID","Gender","Class","Age","NeAutT1","NeComT1","HWTeacT1","HWPeerT2","HWParT1",
                                       "AutT1","ContT1","AMotT1","HWTeachT2","HWParT2","AutT2","AutT3",
                                       "ContT2","AMotT2","NeAutT3","NeComT3","HWTeachT3","HWParT3","HWPeerT3","ContT3",
                                       "AMotT3","IdrottBetygT1","MatteBetygT1","SpråkBetygT1","SvenskaBetygT1","PoängT1","IdrottBetygT2","MatteBetygT2","SpråkBetygT2",
                                       "SvenskaBetygT2","PoängT2","IdrottBetygT3","MatteBetygT3","SpråkBetygT3","SvenskaBetygT3","PoängT3")], Nlevel = 4)

debugonce(FromWideToLong)

ForMixedData <- FromWideToLong(MixedModelData[c("ID","Gender","Class","PartAge","NeAutT1","NeComT1","HWTeachT1","HWPeerT2","HWParT1",
                                                 "AutT1","ContT1","AMotT1","HWTeachT2","HWParT2","AutT2","AutT3",
                                                 "ContT2","AMotT2","NeAutT3","NeComT3","HWTeachT3","HWParT3","HWPeerT3","ContT3",
                                                 "AMotT3","IdrottBetygT1","MatteBetygT1","SpråkBetygT1","SvenskaBetygT1","PoängT1","IdrottBetygT2","MatteBetygT2","SpråkBetygT2",
                                                 "SvenskaBetygT2","PoängT2","IdrottBetygT3","MatteBetygT3","SpråkBetygT3","SvenskaBetygT3","PoängT3")],
                               "ID",  Nlevel = 4)

MixedModelData

View(ForMixedData)
GetMaxLevels(res)
View(MixedModelTomas[c("ID","Gender","Class","Age","NeAutT1","NeComT1","AHWTeacT1","HWParT1",
                       "AutT1","ContT1","AMotT1","NeComT2","BHWTeachT2","HWParT2","AutT2",
                       "ContT2","AMotT2","NeAutT3","NeComT3","CHWTeachT3","HWParT3","HWPeerT3","ContT3",
                       "AMotT3","IdrottBetygT1","MatteBetygT1","SpråkBetygT1","SvenskaBetygT1","PoängT1","IdrottBetygT2","MatteBetygT2","SpråkBetygT2",
                       "SvenskaBetygT2","PoängT2","IdrottBetygT3","MatteBetygT3","SpråkBetygT3","SvenskaBetygT3","PoängT3")])


testList <- list(c(1,3,5))
testList <- append(testList, list(c(2,4,6)))







testString <- "3412567"
testString <- stringr::str_remove(testString, str_extract("3412567","12" ))

library(utils)
utils::winDialog("OK")

f <- x ~ 1 + y + z
f

getParsed <- function(formulaTree)
{
  if (is.null(formulaTree) )
    return(NULL)
  if (is.symbol(formulaTree))
  {
    browser()
    return(as.character(formulaTree))

  }
  if (is.vector(formulaTree))
  {
    return(as.character(formulaTree))
  }
  print(formulaTree)
  return(paste(getParsed(formulaTree[[2]]), getParsed(formulaTree[[1]]),
               getParsed(formulaTree[[3]]),sep = " "))
 }
getParsedData <- function(formulaTree,data)
{
  if (is.null(formulaTree) )
    return(data.frame(Null = rep(0, nrow(data))))
  if (is.symbol(formulaTree))
  {
    browser()
    if (as.character(formulaTree) %in% names(data))
      return(data[as.character(formulaTree)])
    else
      return(data.frame(Sym = rep(as.character(formulaTree), nrow(data))))

  }
  if (is.vector(formulaTree))
  {
    browser()
    return(data.frame(Constant = rep(formulaTree, nrow(data))))
  }
  print(formulaTree)
  return(cbind(getParsedData(formulaTree[[2]], data), getParsedData(formulaTree[[1]], data),
               getParsedData(formulaTree[[3]], data)))
}
f <- NeAuVar ~ 1 + ContVar

data$data[as.character(f[[1]])]
View(data$data)

getParsedData(f, as.data.frame(ForMixedData))
