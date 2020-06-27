#' @title missingnessAnalysis function
#' @description This function computes an item nonresponse rate per interviewer.
#' @param dat dataframe
#' @export
#' @import dplyr
#' @return Returns the vector of item nonresponse rates for each interviewer.
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' mistest<-missingnessAnalysis(dat)
#' mistest

missingnessAnalysis<-function(dat){
  s.dat<-split(dat, dat$A5)
  naf<-sapply(s.dat, function(v){
    prop.nas<-sum(is.na(v))/(dim(v)[1]*dim(v)[2]);
  })
  mean.na<-sum(is.na(dat))/(dim(dat)[1]*dim(dat)[2]);

  res<-list(mean.na=mean.na, int.nas=naf)

  return(res)
}
