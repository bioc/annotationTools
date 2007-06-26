getOrthologousProbesets<-function(ps1,ps2,ps2ps,fct=function(x){x},forceProbesetSelection=FALSE) {


	ps1_length<-length(ps1)
	ps2_ind<-as.list(rep(NA,ps1_length))
	ps2_probe<-as.list(rep(NA,ps1_length))
	ps2_probeSel<-as.list(rep(NA,ps1_length))
	ps2_value<-as.list(rep(NA,ps1_length))
	

	ps2_probe<-ps2ps[match(ps1,ps2ps[,1]),4]
	ps2_orthoFound<-!is.na(ps2_probe)
	ps2_probe<-lapply(ps2_probe,function(x){strsplit(x,',')[[1]]})
	

	ps2_ind[ps2_orthoFound]<-lapply(ps2_probe[ps2_orthoFound],function(x){na.omit(match(x,ps2[,1]))})
	ps2_orthoFoundandExisting<-!is.na(ps2_ind)


	ps2_value[ps2_orthoFoundandExisting]<-lapply(ps2_ind[ps2_orthoFoundandExisting],function(x){ps2[x,2]})
	ps2_value[ps2_orthoFoundandExisting]<-lapply(ps2_value[ps2_orthoFoundandExisting],fct)

	if (!forceProbesetSelection) ps2_probeSel[ps2_orthoFoundandExisting]<-lapply(ps2_ind[ps2_orthoFoundandExisting],function(x){ps2[x,1]})

	else {
		for (i in (1:ps1_length)[ps2_orthoFoundandExisting]) {
			ps2_probeSel[[i]]<-ps2[ps2_ind[[i]][na.omit(match(ps2_value[[i]],ps2[ps2_ind[[i]],2]))],1]
		}
	}
	
	list(ps2_probeSel,ps2_value)

}

