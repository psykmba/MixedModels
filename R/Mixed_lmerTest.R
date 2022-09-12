lmer.Mixed <- function(
  formula,
  REML = TRUE,
  control = lmerControl(),
  start = NULL,
  verbose = 0L,
  subset,
  weights,
  na.action,
  offset,
  contrasts = NULL,
  devFunOnly = FALSE
)
{
   dependent <- formula[[2]]
   independentList <- formula[[3]]
}
