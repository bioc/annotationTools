getGENEONTOLOGY<-function(ps,annot,diagnose=FALSE,specifics=0,GOcol=31,noGOsymbol=NA,noGOprovidedSymbol='---',sep=' /// ') {

	if (!is.element(specifics,0:3)) stop('Argument specifics should be 0, 1, 2, or 3')

	empty<-(is.na(ps) | ps=='')
	ind<-match(ps,annot[,1])
	noentry<-(is.na(ind) & !empty)
	if (sum(empty)>0) warning('One or more empty probe sets in input')
	if (sum(noentry)>0) warning('One or more probe sets not found in annotation')

	go<-as.list(rep(noGOsymbol,length(ps)))
	go[complete.cases(ind)]<-annot[na.omit(ind),GOcol] 

	nogo<-go==noGOprovidedSymbol
	go[nogo]<-noGOsymbol 
	if (sum(nogo)>0) warning('One or more ps with no GO term provided in annotation')
	
	indMultGO<-grep('///',go)
	go[indMultGO]<-lapply(go[indMultGO],function(x) {strsplit(x,sep)[[1]]})

	if (specifics!=0) {
		indGOsymbol<-!empty & !noentry & !nogo
		go[indGOsymbol]<-lapply(go[indGOsymbol],function(x) {unlist(lapply(x, function(x) {strsplit(x,' // ')[[1]][specifics]}))})
	}
	if (diagnose) list(go,empty,noentry,nogo)
	else go

}


