#' initialiseC
#'
#' This internal function is used to initialise the cherryC function
#' @param tipC numeric index of a tip
#' @param readSeq a list of vectors of same length
#' @return bestConfiguration, tipM, tipF, tipT, bestConfiguration, flag, caseLength
#' @export
initialiseC=function(tipC, readSeq) {

	N=length(readSeq)	
	
	all=1:N
	tipsToCheck=all[all!=tipC]
	
	#check where 4 branches away from
	quartet=c(tipC,sample(tipsToCheck,3))
	
	bestConfiguration=checkQuartet(tips=quartet,readSeq=readSeq)

	if (bestConfiguration$bestConfiguration==1) {
		#here C and 2 may be a cherry, so we set m as 2
		tipM=quartet[2]
		tipF=quartet[4]
	} else if (bestConfiguration$bestConfiguration==2) {
		#here C and 4 may be a cherry, so we set m as 4
		tipM=quartet[4]
		tipF=quartet[3]
	} else if (bestConfiguration$bestConfiguration==3) {
		#here C and 3 may be cherry, so we set m as 2
		tipM=quartet[3]
		tipF=quartet[2]
	} else {
		stop(sprintf("tips %s, %s, %s and %s cannot be resolved with the chosen method", tipC,tipsToCheck[1],tipsToCheck[2],tipsToCheck[3]))
	}
	return(list('tipM'=tipM, 'tipF'=tipF, 'tipT'=quartet[! quartet%in%c(tipC,tipM,tipF)], 'bestConfiguration'=bestConfiguration$bestConfiguration, 'caseLength'=bestConfiguration$caseLength, flag=bestConfiguration$flag))
}