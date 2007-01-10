getANNOTATION<-function(identifier,annot,diagnose=FALSE,identifierCol=1,annotationCol=15,noAnnotationSymbol=NA,noAnnotationProvidedSymbol='---',sep=' /// ')  {

	if (is.na(noAnnotationProvidedSymbol)) stop('Argument noAnnotationProvidedSymbol should be different from NA')
	if (!(is.na(noAnnotationSymbol))) {if (noAnnotationSymbol==noAnnotationProvidedSymbol) stop('Argument noAnnotationSymbol and noAnnotationProvidedSymbol should be different')}

	empty<-(is.na(identifier) | identifier=='')
	ind<-match(identifier,annot[,identifierCol])
	noentry<-(is.na(ind) & !empty)
	if (sum(empty)>0) {cat('Warning: one or more empty identifers in input\n')}
	if (sum(noentry)>0) {cat('Warning: one or more identifers not found in annotation\n')}
	
	annotation<-as.list(rep(noAnnotationSymbol,length(identifier)))
	annotation[complete.cases(ind)]<-annot[na.omit(ind),annotationCol]
	
	noannotation<-annotation==noAnnotationProvidedSymbol
	annotation[noannotation]<-noAnnotationSymbol 
	if (sum(noannotation)>0) warning('One or more identifers with no annotation provided')
	
	indMultAnnotation<-grep(sep,annotation)
	annotation[indMultAnnotation]<-lapply(annotation[indMultAnnotation],function(x) {strsplit(x,sep)[[1]]})

	if (diagnose) list(annotation,empty,noentry,noannotation)
	else annotation

}

