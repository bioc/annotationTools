getMULTIANNOTATION<-function(identifier,annot,diagnose=FALSE,identifierCol=19,annotationCol=1,noAnnotationSymbol=NA,noAnnotationProvidedSymbol='---') {

	if (is.na(noAnnotationProvidedSymbol)) stop('Argument noAnnotationProvidedSymbol should be different from NA')
	if (!(is.na(noAnnotationSymbol))) {if (noAnnotationSymbol==noAnnotationProvidedSymbol) stop('Argument noAnnotationSymbol and noAnnotationProvidedSymbol should be different')}

	empty<-(is.na(identifier) | identifier=='')
	if (sum(empty)>0) warning('one or more empty ID in input')

	annotation<-as.list(rep(noAnnotationSymbol,length(identifier)))

	ind<-which(!empty)
	for (i in 1:sum(!empty)) {
		annotation[[ind[i]]]<-annot[which(annot[,identifierCol]==identifier[ind[i]]),annotationCol]
	}

	annotationnb<-sapply(annotation,function(x) {length(x)})
	noentry<-annotationnb==0
	if (sum(noentry)>0) warning('one or more ID not found in annotation')

	noannotation<-annotation==noAnnotationProvidedSymbol
	if (sum(noannotation)>0) warning('One or more ID with no annotation provided in annotation')

	annotation[noentry | noannotation]<-noAnnotationSymbol

	if (diagnose) list(annotation,empty,noentry,noannotation)
	else annotation
	
}


