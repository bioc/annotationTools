getHOMOLOG<-function(geneid,targetspecies,homol,cluster=FALSE,diagnose=FALSE,noIDsymbol=NA,clusterCol=1,speciesCol=2,idCol=3) {
	
	empty<-(is.na(geneid) | geneid=='')
	if (sum(empty)>0) warning('One or more empty gene ID/cluster in input')	

	if (!cluster) {
	indgeneid<-match(geneid,homol[,idCol])
	cluster<-homol[indgeneid,clusterCol]
	}	
	else {
	indgeneid<-match(geneid,homol[,clusterCol])
	cluster<-geneid	
	}

	noentry<-(is.na(indgeneid) & !empty)
	if (sum(noentry)>0) warning('One or more gene input gene ID/cluster not found in homologue table')

	targetid<-as.list(rep(noIDsymbol,length(geneid)))
	indtargetspecies<-homol[,speciesCol]==targetspecies
	ind<-which(!empty & !noentry)

	if (sum(!empty & !noentry)>0) {
		for (i in 1:sum(!empty & !noentry)) {
			targetid[[ind[i]]]<-homol[which(homol[,clusterCol]==cluster[ind[i]] & indtargetspecies),idCol]
		}
	}
	targetidnb<-sapply(targetid,function(x) {length(x)})
	notargetid<-targetidnb==0
	if (sum(notargetid)>0) warning('One or more gene ID/cluster with no target provided in homologue table')

	targetid[noentry | notargetid]<-noIDsymbol

	if (diagnose) list(targetid,empty,noentry,notargetid)
	else targetid


}
