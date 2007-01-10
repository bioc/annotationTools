getGENETITLE<-function(ps,annot,diagnose=FALSE,TITLEcol=14,noTITLEsymbol=NA,noTITLEprovidedSymbol='---',sep=' /// ') {

	if (is.na(noTITLEprovidedSymbol)) stop('Argument noTITLEprovidedSymbol should be different from NA')
	if (!(is.na(noTITLEsymbol))) {if (noTITLEsymbol==noTITLEprovidedSymbol) stop('Argument noTITLEsymbol and noTITLEprovidedSymbol should be different')}
	
	empty<-(is.na(ps) | ps=='')
	ind<-match(ps,annot[,1])
	noentry<-(is.na(ind) & !empty)
	if (sum(empty)>0) warning('One or more empty probe sets in input')
	if (sum(noentry)>0) warning('One or more probe sets not found in annotation')

	genetitle<-as.list(rep(noTITLEsymbol,length(ps)))
	genetitle[complete.cases(ind)]<-annot[na.omit(ind),TITLEcol]
	
	notitle<-genetitle==noTITLEprovidedSymbol
	genetitle[notitle]<-noTITLEsymbol 
	if (sum(notitle)>0) warning('One or more ps with no gene TITLE provided in annotation')
	
	indMultTITLE<-grep(sep,genetitle)
	genetitle[indMultTITLE]<-lapply(genetitle[indMultTITLE],function(x) {strsplit(x,sep)[[1]]})
	genetitle<-lapply(genetitle, function(x) {unique(x)})
	
	if (diagnose) list(genetitle,empty,noentry,notitle)
	else genetitle

}



