#' @title shortestPath function
#' @description This function computes the percentage of cases falling into the shortest path.
#' @param dat dataframe
#' @param vec vector of variables:values belonging to the shortest path
#' @export
#' @import dplyr
#' @return Returns the list of results: the global shortcuts mean, vector of shortcuts per interviewer, the matrix of shortcuts per interviewer-question (raw), the matrix of shortcuts per interviewer-question (proportion).
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#'
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' shortcut_vars<-c("Q1:1", "Q11:9")
#' shortpath<-shortestPath(dat, shortcut_vars)
#' shortpath$var.cuts.prop

shortestPath<-function(dat, vec){

  s.dat<-split(dat, dat$A5)
  vars_values<-strsplit(vec, ":")
  selected_vars<-unlist(lapply(vars_values, `[[`, 1))
  var.cuts<-lapply(s.dat, function(v){
    red.dat<- v[,selected_vars]
    ninterviews<-nrow(red.dat)
    res.cuts<-unlist(lapply(1:ncol(red.dat), function(x){
      checkvalues<-paste(vars_values[[x]][-1], collapse="|")
      sum(grepl(checkvalues, red.dat[,x]))}))
    prop.interviews<-res.cuts/ninterviews
    return(list(raw=res.cuts, prop=prop.interviews))})

  var.cuts.raw <- do.call(rbind, lapply(1:length(var.cuts), function(x) var.cuts[[x]]$raw))
  var.cuts.prop <- do.call(rbind, lapply(1:length(var.cuts), function(x) var.cuts[[x]]$prop))

  cuts.per.int<-apply(var.cuts.raw,1,sum)
  qs.per.int<-unlist(lapply(s.dat, function(x) dim(x)[1]))
  int.shortcuts<-cuts.per.int/(qs.per.int*length(selected_vars))

  mean.shortcut<-sum(cuts.per.int)/(sum(qs.per.int)*length(selected_vars))
  colnames(var.cuts.raw)<-colnames(var.cuts.prop)<-selected_vars
  rownames(var.cuts.raw)<-rownames(var.cuts.prop)<-
    paste("Interviewer ", 1:nrow(var.cuts.raw), sep="")

  res=list(int.shortcuts.mean=mean.shortcut,
           int.shortcuts=int.shortcuts,
           var.cuts.raw=var.cuts.raw,
           var.cuts.prop=var.cuts.prop)

  return(res)}
