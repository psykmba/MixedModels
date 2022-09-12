library(devtools)
create_package("~/Psychometric")
use_git()
use_r("FormatData")
use_r("GetMixed")
use_r("Mixed_lmeTest")
load_all()
exists("GetPsychometric", where = globalenv(), inherits = FALSE)
use_mit_license()
use_package("stringr")
use_package("dplyr")
use_package("lmerTest")

document()
debugonce(GetPsychometric)
dat <- as.data.frame(list(pItem1 = c(2,3,4,2,3,4,3,4), pItem2 = c(2,3,5,4,2,0,4,3)))

GetPsychometric(dat, "p", responseScale = list(c(0,4)), itemLength = 1)

install()
use_testthat()
use_test("GetPsychometric", a)

proj_sitrep()
use_r("ImputeMissing")

library(haven)
MixedModelData<- read_sav("~/Dropbox/Forskning/Tomas J/MixedModelTomas.sav")
names(MixedModelData) <- c("ID","Gender","Class","PartAge","NeAutT1","NeComT1","HWTeachT1","HWPeerT2","HWParT1",
                            "AutT1","ContT1","AMotT1","NeAutT2","NeComT2","HWTeachT2","HWPeerT1","HWParT2","AutT2",
                            "ContT2","AMotT2","NeAutT3","NeComT3","HWTeachT3","HWParT3","HWPeerT3","AutT3","ContT3",
                            "AMotT3","IdrottBetygT1","MatteBetygT1","SpråkBetygT1","SvenskaBetygT1","PoängT1","IdrottBetygT2","MatteBetygT2","SpråkBetygT2",
                            "SvenskaBetygT2","PoängT2","IdrottBetygT3","MatteBetygT3","SpråkBetygT3","SvenskaBetygT3","PoängT3")

usethis::use_data(MixedModelData, overwrite = T)

debugonce(GetMixed)
load_all()
data <- MixedModels::GetMixed(MixedModelData[c("ID","Gender","NeAutT1","NeComT1","HWTeachT1","NeComT2","HWTeachT2","NeAutT3","NeComT3","HWTeachT3")], validLetters = 4,
                      variableName = "level")
Mixed
use_r("Data")

debugonce(summary.Psychometric)
myobject <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"), responseScale = list(c(0,4)), itemLength = 4)
summary(myobject)
names(persData)

use_r("PsychometricHelper")
use_r("Plots")
use_r("BestFunctions")
