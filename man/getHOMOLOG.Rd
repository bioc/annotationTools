\name{getHOMOLOG}
\alias{getHOMOLOG}
\title{Find homologous/orthologous gene (ID)}
\description{
Takes a vector of gene IDs, a table of homologs/orthologs, and a target species and returns gene IDs corresponding to homologous/orthologous genes.
}
\usage{
getHOMOLOG(geneid, targetspecies, homol, cluster = FALSE, diagnose = FALSE, noIDsymbol = NA, clusterCol = 1, speciesCol = 2, idCol = 3)
}
\arguments{
  \item{geneid}{character vector containing gene IDs.}
  \item{targetspecies}{identifier of the target species in the homology/orthology table.}
  \item{homol}{homology/orthology table (data frame) listing gene IDs (1 per line) along with the species and the homology/orthology cluster they belong to.}
  \item{cluster}{logical. If TRUE, the identifiers provided in 'geneid' are homology/orthology cluster IDs. If FALSE, they are gene IDs.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{noIDsymbol}{character string to be used in output list 'targetid' if no homologous/orthologous gene is found or provided in the annotation table.}
  \item{clusterCol}{column in homology/orthology table containing homology/orthology cluster IDs.}
  \item{speciesCol}{column in homology/orthology table containing species IDs.}
  \item{idCol}{column in homology/orthology table containing gene IDs.}
}
\details{
The homology/orthology table lists gene IDs (from several species) and the homology/orthology cluster they belong to. Homologous and orthologous genes share a common cluster identifier. Given a certain gene ID, a target species, and a homology/orthology table, all gene IDs belonging to the same homology/orthology cluster and to the specified target species are returned.
If 'targetspecies' is the species 'geneid' belongs to, by definition, homologous genes are returned (if listed). On the contrary, specifying a 'targetspecies' different from the host species 'geneid' belongs to, results in orthologous genes to be returned.
Note that each gene ID is assumed to be unique and to belong to a single homology/orthology cluster.

Gene IDs of homologous/orthologous genes are returned as elements of list 'targetid'.
If multiple (homologous/orthologous) gene IDs are provided for 'geneid[i]', a vector containing all gene IDs is returned as the 'i-th' element of list 'targetid'. 

Default values for 'clusterCol', 'speciesCol', and 'idCol' are chosen to match the table provided by HomoloGene (homologene.data provided by www.ncbi.nlm.nih.gov/HomoloGene). Homology/orthology tables from other sources might require setting these values appropriately.

If 'cluster' is TRUE, cluster IDs can be provided in 'geneid' (instead of gene IDs) and the function will return all (homologous/orthologous) gene IDs belonging to a given cluster ID and a given 'targetspecies'.
This can be used to mine orthology tables provided by Affymetrix (e.g. 'Mouse430\_2\_ortholog.csv') for orthologous probe set IDs (see 'examples' below).
}
\value{
  \item{targetid}{list of length 'length(geneid)' the 'i'-th element of which contains the homologous/orthologous gene IDs for 'geneid[i]' and 'targetspecies'.}
  \item{empty}{logical vector of length 'length(geneid)'. 'empty[i]' is TRUE if 'geneid[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(geneid)'. 'noentry[i]' is TRUE if 'geneid[i]' cannot be found in column 'idCol' (default is column 3) of the homology/orthology table 'homol'.}
  \item{notargetid}{locial vector of length 'length(geneid)'. 'notargetid[i]' is TRUE if 'geneed[i]' is found in the homology/orthology table but no homolog/ortholog is listed for 'targetspecies'.}
}
\references{}
\author{Alexandre Kuhn, \email{alexandre.kuhn@isb-sib.ch}}
\examples{
##example Homologene file and its location
homologeneFile<-'homologene_part.data'
dataDirectory<-system.file('data',package='annotationTools')

##load Homologene file
homologene<-read.delim(paste(dataDirectory,homologeneFile,sep='/'),header=FALSE)

##get mouse (species ID 10090) orthologs of several human (species ID 9606) gene ID (among those: 5982, gene symbol RFC2 and 93587, gene symbol: RG9MTD2)
myGenes<-c(5982,93587,NA,100)
getHOMOLOG(myGenes,10090,homologene)

##track origin of annotation failure for the last 2 gene IDs
getHOMOLOG(myGenes,10090,homologene,diagnose=TRUE)

##get mouse gene belonging to homologene cluster IDs 6885 and 6886
myClusters<-c(6885,6886)
getHOMOLOG(myClusters,10090,homologene,cluster=TRUE)

##mine Affymetrix (example) ortholog file
affyOrthologFile<-'HG-U133_Plus_2_ortholog_part.csv'
affyOrthologs<-read.csv(paste(dataDirectory,affyOrthologFile,sep='/'),colClasses='character')

##get Mouse430_2 probe set IDs 'orthologous' to HG-U133_Plus_2 probe set IDs 1053_at and 121_at
myPS<-c('1053_at','121_at')
getHOMOLOG(myPS,'Mouse430_2',affyOrthologs,cluster=TRUE,clusterCol=1,speciesCol=4,idCol=3)
}
\keyword{manip}
