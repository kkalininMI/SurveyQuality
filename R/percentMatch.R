#' @title percentMatch function
#' @description This function calculates the percent match, meaning the maximum percentage of variables that an observation shares with any other observation in the data set.
#' @param dat dataframe.
#' @param comparerows determines if the duplicates or rows or columns needto be calculated (TRUE, by default)
#' @export
#' @import dplyr
#' @return Returns the list elections and pipe.table objects.
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#'
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' pmatch<-formatData(dat, rescalev=FALSE, removepercentNAshigher=0.90,
#'                    cleanfromtxtconstants=TRUE)%>%percentMatch()

percentMatch <- function(dat, comparerows=TRUE){

  dat[is.na(dat)]<-999  #to compare NAs appropriately

  if(isFALSE(comparerows)) dat <- t(dat)

  pmatch<-apply(dat,1,function(x){
    sort(
      apply(dat, 1, function(y){sum(x==y, na.rm=TRUE)/ncol(dat)}), decreasing=TRUE)[2]
  })

  apply(dat,2,function(x) sum(is.na(x)))
  if(comparerows) colnames(pmatch)
  return(pmatch)}
