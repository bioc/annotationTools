getPROBESET<-function(geneid,annot,uniqueID=FALSE,diagnose=FALSE,idCol=19,noPSsymbol=NA,noPSprovidedSymbol='---') {

	if (is.na(noPSprovidedSymbol)) stop('Argument noPSprovidedSymbol should be different from NA')
	if (!(is.na(noPSsymbol))) {if (noPSsymbol==noPSprovidedSymbol) stop('Argument noPSsymbol and noPSprovidedSymbol should be different')}

	empty<-(is.na(geneid) | geneid=='')
	if (sum(empty)>0) warning('one or more empty gene ID in input')

	ps<-as.list(rep(noPSsymbol,length(geneid)))

	if (uniqueID) pat<-paste('^',geneid,'$',sep='')
	else pat<-paste('(^| )',geneid,'($| )',sep='')

	ind<-which(!empty)
	for (i in 1:sum(!empty)) {
		ps[[ind[i]]]<-annot[grep(pat[ind[i]],annot[,idCol]),1]
	}

	psnb<-sapply(ps,function(x) {length(x)})
	noentry<-psnb==0
	if (sum(noentry)>0) warning('one or more gene ID not found in annotation')

	nops<-ps==noPSprovidedSymbol
	if (sum(nops)>0) warning('One or more gene ID with no probe set provided in annotation')

	ps[noentry | nops]<-noPSsymbol

	if (diagnose) list(ps,empty,noentry,nops)
	else ps
	
}


