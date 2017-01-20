#' genDist
#'
#' This function calculates the Jukes/Cantor distance between two vectors
#' @param tip1 a vector
#' @param tip2 another vector
#' @return distance
#' @export
genDist=function(tip1,tip2) {
	L=length(tip1)
	p=sum((tip1!=tip2))/L
	#print(p)
	return((-3/4)*log(1-(4/3)*p))
}