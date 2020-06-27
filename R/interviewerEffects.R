#' @title interviewerEffects function
#' @description This function calculates the interviewer effects.
#' @param dat dataframe
#' @param f lme formula
#' @param separateffects  separate effects by group or not (FALSE, by default)
#' @export
#' @import dplyr
#' @import lme4
#' @import performance
#' @return Returns the list of results: the matrix of ICCs (intraclass correlation coefficients), the matrix of coefficients.
#' @examples
#' library(SurveyQuality)
#' library(dplyr)
#' dat <- read.csv(system.file("elites_survey.csv", package="SurveyQuality"))
#'
#' dat[dat==98|dat==99|dat==95|dat==998|dat==999]<-NA
#' dat$A6<-as.factor(dat$A6)
#' intEffects<-formatData(dat, rescalev=TRUE,
#'             removepercentNAshigher=0.80, cleanfromtxtconstants=TRUE)%>%
#'             interviewerEffects(f="y ~ 1 + A6 + (1|A5)")
#' intEffects$ICC



interviewerEffects<-function(dat, f, separateffects=FALSE){
  levelsV<-paste(
    gsub("\\||\\)", "",
         unlist(
           regmatches(f,
                      gregexpr("\\|.+?\\)", f)))), collapse="|")

  res<-lapply(colnames(dat), function(y){
    iccM<-NULL; coefM<-NULL; VarName<-NULL
    if(!grepl(levelsV, y)){
      xmodel<-tryCatch(eval(parse(text=paste("lmer(formula = ", y, "~",
                                             regmatches(f, regexpr("(?<=~).+$", f, perl=TRUE)), ", data=dat)", sep=""))),
                                                        error = function(e) e)

      if(inherits(xmodel,  "error")) return()

      if(!isSingular(xmodel)) {
        if (separateffects){
          iccM<-c(performance::icc(xmodel, by_group = TRUE))}else{
            iccM<-performance::icc(xmodel, by_group = FALSE)
          }
        coefM<-coef(xmodel)
        VarName<-y
      }
    }
    return(list(VarName=VarName, iccM=iccM, coefM=coefM))})

  levelsV<-unlist(strsplit(levelsV, "\\|"))

  if(length(levelsV)>=2){
    iccM <- do.call(rbind, lapply(1:length(res), function(y) res[[y]]$iccM))}else{
      iccM <- do.call(rbind, lapply(1:length(res), function(y) res[[y]]$iccM[2]))
    }
  rownames(iccM) <- unlist(lapply(1:length(res), function(y) res[[y]]$VarName))

  coefM <- do.call(rbind, lapply(1:length(res), function(y){
                  if(!is.null(res[[y]]$coefM[[1]])){res[[y]]$coefM[[1]][,1]}else{NA}}))
  coefM.t <- data.frame(t(coefM))
  colnames(coefM.t) <- names(dat)
  list(ICC=iccM, Coef=coefM.t)
}
