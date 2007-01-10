getGENESYMBOL<-function(ps,annot,diagnose=FALSE,GScol=15,noGSsymbol=NA,noGSprovidedSymbol='---',sep=' /// ')  {

	if (is.na(noGSprovidedSymbol)) stop('Argument noGSprovidedSymbol should be different from NA')
	if (!(is.na(noGSsymbol))) {if (noGSsymbol==noGSprovidedSymbol) stop('Argument noGSsymbol and noGSprovidedSymbol should be different')}

	empty<-(is.na(ps) | ps=='')
	ind<-match(ps,annot[,1])
	noentry<-(is.na(ind) & !empty)
	if (sum(empty)>0) {cat('Warning: one or more empty probe sets in input\n')}
	if (sum(noentry)>0) {cat('Warning: one or more probe sets not found in annotation\n')}
	
	symbols<-as.list(rep(noGSsymbol,length(ps)))
	symbols[complete.cases(ind)]<-annot[na.omit(ind),GScol]
	
	nogs<-symbols==noGSprovidedSymbol
	symbols[nogs]<-noGSsymbol 
	if (sum(nogs)>0) warning('One or more probe sets with no gene symbol provided in annotation')
	
	indMultGS<-grep(sep,symbols)
	symbols[indMultGS]<-lapply(symbols[indMultGS],function(x) {strsplit(x,sep)[[1]]})

	if (diagnose) list(symbols,empty,noentry,nogs)
	else symbols

}

