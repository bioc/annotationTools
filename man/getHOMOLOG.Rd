\name{getHOMOLOG}
\alias{getHOMOLOG}
\title{Find homologous/orthologous gene (ID)}
\description{
Takes a vector of gene IDs, a table of homologs/orthologs, and a target species and returns gene IDs corresponding to homologous/orthologous genes.
}
\usage{
getHOMOLOG(geneid, targetspecies, homol, cluster = FALSE, diagnose = FALSE, noIDsymbol = NA, clusterCol = 1, speciesCol = 2, idCol = 3, tableType = "homologene")
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
  \item{tableType}{character string specifying the type of homology/orthology table used. Either 'homologene' (default) or 'gene_group'.}
}
\details{
The homology/orthology table lists gene IDs (from several species) and the homology/orthology cluster they belong to. Homologous and orthologous genes share a common cluster identifier. Given a certain gene ID, a target species, and a homology/orthology table, all gene IDs belonging to the same homology/orthology cluster and to the specified target species are returned. Various homology/orthology databases can be used, in particular NCBI's HomoloGene and their 'Orthologs from Annotation pipeline', referred to as 'gene_group' database (see details below).
If 'targetspecies' is the species 'geneid' belongs to, by definition, homologous genes are returned (if listed). On the contrary, specifying a 'targetspecies' different from the host species 'geneid' belongs to, results in orthologous genes to be returned.
Note that each gene ID is assumed to be unique and to belong to a single homology/orthology cluster.

Gene IDs of homologous/orthologous genes are returned as elements of list 'targetid'.
If multiple (homologous/orthologous) gene IDs are provided for 'geneid[i]', a vector containing all gene IDs is returned as the 'i-th' element of list 'targetid'. 

Default values for 'clusterCol', 'speciesCol', and 'idCol' are chosen to match the table provided by HomoloGene (homologene.data provided by www.ncbi.nlm.nih.gov/HomoloGene). Homology/orthology tables from other sources might require setting these values appropriately.

Orthologs defined in NCBI's 'Orthologs from Annotation pipeline' database (available at ftp.ncbi.nlm.nih.gov/gene/DATA/gene_group.gz, and hence referred to as 'gene_group') can be mined by setting 'tableType' to 'gene_group' instead of 'homologene' (default). In this case, arguments 'clusterCol', 'speciesCol',and 'idCol' are overridden to fit the data structure used in 'gene_group'. In short, one difference between 'gene_group' compared to HomoloGene is that 'gene_group' does not use ortholog cluster IDs but anchors each ortholog groups using the human gene ID of the ortholog group. If a specific ortholog group does not contain a human gene, a gene ID from another species within the ortholog group may be used as anchor.

'gene_group' is a rich source of homologs/orthologs between selected vertebrate species and introduced by NCBI in 2014 (https://www.ncbi.nlm.nih.gov/news/03-13-2014-gene-provides-orthologs-regions/). Note that 'gene_group' does not list (and thus cannot be used to search for) homologs (i.e. only 'Ortholog' relationships, as specified in the 3rd column of 'gene_group' are considered).

Finally, if 'cluster' is TRUE, cluster IDs can be provided in 'geneid' (instead of gene IDs) and the function will return all (homologous/orthologous) gene IDs belonging to a given cluster ID and a given 'targetspecies'.
This can be used to mine orthology tables provided by Affymetrix (e.g. 'Mouse430\_2\_ortholog.csv') for orthologous probe set IDs (see 'examples' below).
}
\value{
  \item{targetid}{list of length 'length(geneid)' the 'i'-th element of which contains the homologous/orthologous gene IDs for 'geneid[i]' and 'targetspecies'.}
  \item{empty}{logical vector of length 'length(geneid)'. 'empty[i]' is TRUE if 'geneid[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(geneid)'. 'noentry[i]' is TRUE if 'geneid[i]' cannot be found in column 'idCol' (default is column 3) of the homology/orthology table 'homol'.}
  \item{notargetid}{locial vector of length 'length(geneid)'. 'notargetid[i]' is TRUE if 'geneed[i]' is found in the homology/orthology table but no homolog/ortholog is listed for 'targetspecies'.}
}
\references{Kuhn et al. Cross-species and cross-platform gene expression studies with the Bioconductor-compliant R package 'annotationTools'. BMC Bioinformatics, 9:26 (2008)}
\author{Alexandre Kuhn}
\examples{
##example Homologene file and its location
homologeneFile<-system.file('extdata','homologene_part.data',package='annotationTools')

##load Homologene file
homologene<-read.delim(homologeneFile,header=FALSE)

##get mouse (species ID 10090) orthologs of several human (species ID 9606) gene ID (among those: 5982, gene symbol RFC2 and 93587, gene symbol: RG9MTD2)
myGenes<-c(5982,93587,NA,100)
getHOMOLOG(myGenes,10090,homologene)

##track origin of annotation failure for the last 2 gene IDs
getHOMOLOG(myGenes,10090,homologene,diagnose=TRUE)

##get mouse gene belonging to homologene cluster IDs 6885 and 6886
myClusters<-c(6885,6886)
getHOMOLOG(myClusters,10090,homologene,cluster=TRUE)

##get mouse orthologs of human genes using 'gene_group'
gene_groupFile<-system.file('extdata','gene_group_part.data',package='annotationTools')
group_gene<-read.delim(gene_groupFile,header=TRUE)
getHOMOLOG(myGenes,10090,gene_group,tableType='gene_group')

##mine Affymetrix (example) ortholog file
affyOrthologFile<-system.file('extdata','HG-U133_Plus_2_ortholog_part.csv',package='annotationTools')
affyOrthologs<-read.csv(affyOrthologFile,colClasses='character')

##get Mouse430_2 probe set IDs 'orthologous' to HG-U133_Plus_2 probe set IDs 1053_at and 121_at
myPS<-c('1053_at','121_at')
getHOMOLOG(myPS,'Mouse430_2',affyOrthologs,cluster=TRUE,clusterCol=1,speciesCol=4,idCol=3)
}
\keyword{manip}
