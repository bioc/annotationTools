compactList<-function(lst,l) {

	stopifnot(length(lst)==sum(l))
	clst<-vector('list',length(l))
	j<-1
	for (i in 1:length(l)) {
		clst[[i]]<-unlist(lst[j:(j+l[i]-1)])
		j<-j+l[i]
	}
	clst
}