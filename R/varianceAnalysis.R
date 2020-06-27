#' @title varianceAnalysis function
#' @description This function computes the variance per interviewer.
#' @param dat dataframe
#' @export
#' @import dplyr
#' @return Returns the vector of variances for each interviewer.
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' dat<-dat[,grepl("^Q|A5$", colnames(dat))]
#'
#' var_test<-formatData(dat, rescalev=FALSE,
#'                           removepercentNAshigher=0.80,
#'                           cleanfromtxtconstants=TRUE,
#'                           keepunscaled="A5")%>%
#'                           varianceAnalysis()
#' var_test


varianceAnalysis<-function(dat){
  s.dat<-split(dat, dat$A5)

  var.int<-lapply(s.dat, function(v){
    apply(v, 2, function(x) var(x, na.rm=TRUE))
  })

  mean.var.int<-  sapply(var.int, function(x) mean(x, na.rm=TRUE))

  return(int.var=mean.var.int)}
