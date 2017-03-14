ps2ps<-function(annotation_1,annotation_2,ortholog,target_species,probesets=NULL,tableType="homologene") {
	
	if (is.null(probesets)) ps_1<-annotation_1[,1]
	else ps_1<-probesets
	cat('Getting source gene IDs...\n')
	gid_1<-getGENEID(ps_1,annotation_1)
	length_gid_1<-sapply(gid_1,function(x) {length(x)})
	cat('Getting orthologous genes...\n')
	gid_2<-getHOMOLOG(unlist(gid_1),target_species,ortholog,tableType=tableType)
	length_gid_2<-sapply(gid_2,function(x) {length(x)})
	cat('Getting orthologous probe sets...\n')
	ps_2<-getPROBESET(unlist(gid_2),annotation_2)
	
	ps_2_1<-compactList(ps_2,length_gid_2)
	ps_2_2<-compactList(ps_2_1,length_gid_1)
	gid_2_1<-compactList(gid_2,length_gid_1)

	ps_2_2<-lapply(ps_2_2,function(x) {unique(x)})
	ps_2_2<-lapply(ps_2_2,function(x) {if (length(x)>1) na.omit(x) else x})
	gid_2_1<-lapply(gid_2_1,function(x) {unique(x)})
	gid_2_1<-lapply(gid_2_1,function(x) {if (length(x)>1) na.omit(x) else x})

	mappingTable<-data.frame(ps_1=I(ps_1),gid_1=I(listToCharacterVector(gid_1,sep=',')),gid_2=I(listToCharacterVector(gid_2_1,sep=',')),ps_2=I(listToCharacterVector(ps_2_2,sep=',')))
	mappingTable

}
