#' parcwt
#'
#' This function finds the number of the cherries given a phyDat list of sequences
#' @param sequences a list of vectors of same length
#' @return cherryNumber
#' @export
parcwt=function(sequences) {

	readSeq=sequences

	cherryC=cwt::cherryC
	initialiseC=cwt::initialiseC
	genDist=cwt::genDist
	checkQuartet=cwt::checkQuartet
	
	envir=environment()
	
	N=length(readSeq)
	
	cherryList=list()
	cherryNumber=0
	
	tipC=1
	all=1:N
	remaining=rep(TRUE,N)

	nCores=parallel::detectCores()-1
	cwtCluster=parallel::makeCluster(min(nCores,length(readSeq)))
	
	parallel::clusterExport(cwtCluster,"readSeq", envir=envir)
	parallel::clusterExport(cwtCluster,"cherryC")
	parallel::clusterExport(cwtCluster,"initialiseC")
	parallel::clusterExport(cwtCluster,"genDist")
	parallel::clusterExport(cwtCluster,"checkQuartet")
		
	C=parallel::parLapply(cwtCluster, 1:N, function(x) cherryC(x,readSeq))
	
	parallel::stopCluster(cwtCluster)
	
	cherryNumber=sum(sapply(1:length(readSeq),function(x) C[[x]]$isThereCherry))/2
		
	cherryList=lapply(1:length(readSeq),function(x) C[[x]]$cherryCM)
	cherryList=Filter(Negate(function(x) is.null(unlist(x))), cherryList)
	cherryList=unique(lapply(1:length(cherryList),function(x) sort(cherryList[[x]])))
		
	return(cherryNumber)	
	
}