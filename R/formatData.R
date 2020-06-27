#' @title formatData function
#' @description This function preformats the data.
#' @param dat dataframe.
#' @param rescalev scales the columns of a numeric matrix (TRUE, by default)
#' @param removepercentNAshigher remove columns with percent of NAs exceeding given proportion (1, by default).
#' @param cleanfromtxtconstants  remove columns with text/ constant columns percent of NAs exceeding given proportion (TRUE, by default).
#' @param keepunscaled vector of variables to keep  unscaled (NULL, by default).
#' @export
#' @import dplyr
#' @return Returns the formatted data.
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#'
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' pmatch<-formatData(dat, rescalev=FALSE, removepercentNAshigher=0.90,
#'                    cleanfromtxtconstants=TRUE)%>%percentMatch()


formatData<-function(dat, rescalev=TRUE, removepercentNAshigher=1, cleanfromtxtconstants=TRUE, keepunscaled=NULL){
  if(!is.null(keepunscaled)){
    keepunscaled.vars<-dat[,colnames(dat)%in%keepunscaled]
  }

  removeCols<-function(dat){
    rmCols<-apply(dat,2,function(x){(sum(is.na(x)|x==999)/length(x))<=removepercentNAshigher})
    return(dat[, rmCols])}


  if (removepercentNAshigher<1) {
    dat<-removeCols(dat=dat)

  }

  if(cleanfromtxtconstants){
    dat<-dat[unlist(lapply(dat,class))!="character"]
    dat<-dat[,!apply(dat,2, function(x) var(x, na.rm=TRUE)==0)]
  }

  if (rescalev) {
    dat[dat==999]<-NA

    var.factor<-sapply(dat, is.factor)
    dat.factor<-data.frame(dat[, var.factor]); colnames(dat.factor)<-colnames(dat)[var.factor]
    dat.stand <- data.frame(scale(dat[, !var.factor]))
    datN<-data.frame(dat.factor, dat.stand)
    dat<-datN[, colnames(dat)]
    dat<-removeCols(dat=dat)

    if(!is.null(keepunscaled)){
      dat[,colnames(dat)%in%keepunscaled]<-keepunscaled.vars
    }
  }
  dat<-data.frame(dat)

  return(dat)}

