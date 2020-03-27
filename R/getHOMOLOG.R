getHOMOLOG<-function(geneid,targetspecies,homol,cluster=FALSE,diagnose=FALSE,noIDsymbol=NA,clusterCol=1,speciesCol=2,idCol=3,tableType="homologene") {

	if (tableType!="homologene" & tableType!="gene_orthologs") stop("Input argument tableType should be 'homologene' or 'gene_orthologs'.")

	if (tableType=="gene_orthologs") {
        	message("Using a 'gene_orthologs' type of file as ortholog table.")
        	homol<-homol[homol[,3]=="Ortholog",]
        	cluster=FALSE
        	clusterCol=2
        	speciesCol=4
        	idCol=5

        	duplOthergeneid<-intersect(geneid,homol[duplicated(homol[,idCol]),idCol])
        	if (length(duplOthergeneid)>0) {
                	warning(paste(duplOthergeneid,": belongs to more than one ortholog group in 'gene_orthologs'. Only orthologs in the first group will be returned."))
        	}

        	anchors<-homol[!duplicated(homol[,clusterCol]),c(1,2,3,1,2)]
        	multipleOrthogroup<-intersect(geneid,intersect(anchors[,idCol],homol[,idCol]))
        	if (length(multipleOrthogroup)>0) {
                	warning(paste(multipleOrthogroup,": is used as anchor of an ortholog group and also belongs to another ortholog group. Only orthologs of the group where it is the anchor will be returned."))
        	}
        	homol<-rbind(setNames(anchors,names(homol)),homol)
	}

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
