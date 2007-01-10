listToCharacterVector<-function(lst,sep=' /// ') {

	isna<-is.na(lst)
	v<-rep(NA,length(lst))
	v[!isna]<-sapply(lst[!isna],function(x) {paste(x,collapse=sep)})
	v	
}