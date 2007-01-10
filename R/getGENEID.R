getGENEID<-function(ps,annot,diagnose=FALSE,idCol=19,noIDsymbol=NA,noIDprovidedSymbol='---',sep=' /// ') {

	if (is.na(noIDprovidedSymbol)) stop('Argument noIDprovidedSymbol should be different from NA')
	if (!(is.na(noIDsymbol))) {if (noIDsymbol==noIDprovidedSymbol) stop('Argument noIDsymbol and noIDprovidedSymbol should be different')}
	
	empty<-(is.na(ps) | ps=='')
	ind<-match(ps,annot[,1])
	noentry<-(is.na(ind) & !empty)
	if (sum(empty)>0) warning('One or more empty probe sets in input')
	if (sum(noentry)>0) warning('One or more probe sets not found in annotation')

	geneid<-as.list(rep(noIDsymbol,length(ps)))
	geneid[complete.cases(ind)]<-annot[na.omit(ind),idCol]
	
	noid<-geneid==noIDprovidedSymbol
	geneid[noid]<-noIDsymbol 
	if (sum(noid)>0) warning('One or more probe sets with no gene ID provided in annotation')
	
	indMultID<-grep(sep,geneid)
	geneid[indMultID]<-lapply(geneid[indMultID],function(x) {strsplit(x,sep)[[1]]})
	
	if (diagnose) list(geneid,empty,noentry,noid)
	else geneid

}
