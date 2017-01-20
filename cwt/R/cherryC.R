#' cherryC
#'
#' This function determines if tipC is in a cherry and che other tips in the cherry
#' @param tipC numeric index of the tip
#' @param readSeq a list of vectors of same length
#' @return bestConfiguration, flag, caseLength
#' @export
cherryC=function(tipC, readSeq) {

	N=length(readSeq)

	initC=initialiseC(tipC=tipC,readSeq)
	tipM=initC$tipM
	tipF=initC$tipF
	tipT=initC$tipT
	diagnostic=data.frame(tipC=tipC,tipM=tipM,tipF=tipF,tipT=initC$tipT,tipQ=NA,bestConf=initC$bestConfiguration, best5Conf=NA, L=initC$caseLength, mOk=1, flag=initC$flag)

	pastTipF=c(tipF)

	#this gives the tips we need to try:
	all=1:N
	tipsToCheck=all[! all%in%c(tipC,tipM,tipF,tipT)]

	#this is 1 if tipM is a real maybe and 0 if not
	mOk=1

	while(length(tipsToCheck)>0) {
		
		tipT=if (length(tipsToCheck)==1) {tipsToCheck} else {sample(tipsToCheck,1)}
		quartet=c(tipC,tipM,tipF,tipT)
		bestConfiguration=checkQuartet(tips=quartet, readSeq=readSeq)

		tipsToCheck=tipsToCheck[tipsToCheck!=tipT]		

		best5Conf=NA
		tipQ=NA

		possibleTipF=c( all[!all%in%c(tipsToCheck,pastTipF,tipC,tipT,tipM)] , pastTipF)
		moveOn=length(possibleTipF)-1
		tempBestConfiguration=bestConfiguration
		while (tempBestConfiguration$flag && moveOn>0) {
			tempTipF=possibleTipF[moveOn]
			tempQuartet=c(tipC,tipM,tempTipF,tipT)
			tempBestConfiguration=checkQuartet(tips=tempQuartet, readSeq=readSeq)
			moveOn=moveOn-1
		}
		if (moveOn>0 && moveOn!=(length(possibleTipF)-1)) {
			tipF=tempTipF
				quartet=tempQuartet
			bestConfiguration=tempBestConfiguration
		}

		update=switch(bestConfiguration$bestConfiguration, 
			list('tipF'=tipF,'tipM'=tipM,'mOk'=mOk, 'updateF'=c()),
			list('tipF'=tipM,'tipM'=tipT,'mOk'=1, 'updateF'=tipM),
			list('tipF'=tipF,'tipM'=tipT,'mOk'=0, 'updateF'=c())
		)				
	
		tipM=update$tipM
		tipF=update$tipF
		mOk=update$mOk
		pastTipF=c(pastTipF,update$updateF)

		diagnostic=rbind(diagnostic,unlist(c(quartet[1],quartet[2],quartet[3],quartet[4],tipQ, bestConfiguration$bestConfiguration, best5Conf, bestConfiguration$caseLength,mOk, bestConfiguration$flag)))
	}

	if (mOk==1) {
		#here tipM is a cherry
		return(list('isThereCherry'=1, 'cherryCM'=c(tipC,tipM), 'diagnostic'=diagnostic))
	} else {
		return(list('isThereCherry'=0, 'cherryCM'=c(), 'diagnostic'=diagnostic))
	}
}