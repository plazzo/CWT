#' checkQuartet
#'
#' This function finds the best quartet given three tip indices and a phyDat list of sequences
#' @param tips Defaults to c(1,2,3,4)
#' @param readSeq a list of vectors of same length
#' @return bestConfiguration, flag, caseLength
#' @export

checkQuartet=function(tips=c(1,2,3,4), readSeq) {
	
	tipC=readSeq[[tips[1]]]
	tipM=readSeq[[tips[2]]]
	tipF=readSeq[[tips[3]]]
	tipT=readSeq[[tips[4]]]
	
	CF=genDist(tipC,tipF)
	MT=genDist(tipM,tipT)
	CM=genDist(tipC,tipM)
	FT=genDist(tipF,tipT)
	CT=genDist(tipC,tipT)
	FM=genDist(tipM,tipF)
	
	#print(c(CF+MT-CT-FM,CF+MT-CM-FT,CT+FM-CM-FT))
	
	cases=abs(c(CF+MT-CT-FM,CF+MT-CM-FT,CT+FM-CM-FT))
	
	config=which.min(cases)
	flag=(cases[config]>min(cases[1:3!=config])/5)
	caseLength=list('CF+MT-CT-FM'=CF+MT-CT-FM,'CF+MT-CM-FT'=CF+MT-CM-FT,'CT+FM-CM-FT'=CT+FM-CM-FT)

	return(list('bestConfiguration'=config, 'flag'=flag, 'caseLength'=caseLength))
}