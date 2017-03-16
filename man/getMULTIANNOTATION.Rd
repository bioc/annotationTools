\name{getMULTIANNOTATION}
\alias{getMULTIANNOTATION}
\title{General (multiple) annotation function}
\description{
  Takes a vector of identifiers and an annotation table and matches the identifiers in the table to retrieve the corresponding annotation. Identifiers can occur on multiple records (i.e. lines) of the annotation table.
}
\usage{
getMULTIANNOTATION(identifier, annot, diagnose = FALSE, identifierCol = 19, annotationCol = 1, noAnnotationSymbol = NA, noAnnotationProvidedSymbol = "---")
}
\arguments{
  \item{identifier}{vector containing identifiers to be annotated.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{identifierCol}{column in annotation table where the provided identifiers are to be looked up.}
  \item{annotationCol}{column in annotation table containing the desired annotation.}
  \item{noAnnotationSymbol}{chacter string to be used in output list 'annotation' if no annotation is found or provided.}
  \item{noAnnotationProvidedSymbol}{character string used in annotation table and indicating missing annotation.}
}
\details{
The annotation is returned as elements of list 'annotation'.
If the 'i'-th identifier occur on multiple lines, all corresponding annotation are retrieved and output as a vector. The length of the 'i'-th element of 'annotation' thus equals the number of lines for 'identifier[i]' in the annotation table. 
}
\value{
  \item{annotation}{list of length 'length(identifier)' the 'i'-th element of which contains the annotation for 'identifier[i]'.}
  \item{empty}{logical vector of length 'length(identifier)'. 'empty[i]' is TRUE if 'identifier[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(identifier)'. 'noentry[i]' is TRUE if 'identifier[i]' cannot be found in 'annot[,identifierCol]'.}
  \item{noannotation}{locial vector of length 'length(identifier)'. 'noannotation[i]' is TRUE if 'a[i]==noAnnotationProvidedSymbol' is TRUE.}
}
\references{Kuhn et al. Cross-species and cross-platform gene expression studies with the Bioconductor-compliant R package 'annotationTools'. BMC Bioinformatics, 9:26 (2008)}
\author{Alexandre Kuhn}
\note{\code{getANNOTATION} runs faster and is to be used if the identifiers occur only once in the annotation table.
}
\seealso{\code{\link{getANNOTATION}}}
\examples{
##example annotation table
annotation<-cbind(gene=c('gene_1','gene_2','gene_2','gene_3','gene_4'),probe=c('probe_1','probe_2a','probe_2b','','probe_4'),sequence=c('sequence_1','sequence_2a','sequence_2b','','sequence_4'))
print(annotation)

##get sequences for gene_2, gene_3, gene_4 and gene_100
myGenes<-c('gene_2','gene_3','gene_4','gene_100', NA)
getMULTIANNOTATION(myGenes,annotation,identifierCol=1,annotationCol=2,noAnnotationProvidedSymbol='')

##track origin of annotation failure for the 3 last genes
getMULTIANNOTATION(myGenes,annotation,identifierCol=1,annotationCol=2,noAnnotationProvidedSymbol='',diagnose=TRUE)
}
\keyword{manip}
