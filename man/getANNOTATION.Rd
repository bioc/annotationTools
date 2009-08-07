\name{getANNOTATION}
\alias{getANNOTATION}
\title{General annotation function}
\description{
  Takes a vector of identifiers and an annotation table and matches the identifiers in the table to retrieve the corresponding annotation. Only the first occurence of each identifier in the annotation table is considered.
}
\usage{
getANNOTATION(identifier, annot, diagnose = FALSE, identifierCol = 1, annotationCol = 15, noAnnotationSymbol = NA, noAnnotationProvidedSymbol = "---", sep = " /// ")
}
\arguments{
  \item{identifier}{vector containing identifiers to be annotated.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{identifierCol}{column in annotation table where the provided identifiers are to be looked up.}
  \item{annotationCol}{column in annotation table containing the desired annotation.}
  \item{noAnnotationSymbol}{character string to be used in output list 'annotation' if no annotation is found or provided.}
  \item{noAnnotationProvidedSymbol}{character string used in annotation table and indicating missing annotation.}
  \item{sep}{character string used in annotation table to separate multiple annotation of a single identifier.}
}
\details{
The annotation is returned as elements of list 'annotation'.
If a single annotation is given for a particular identifier, the corresponding element of 'annotation' has length 1.
If multiple annotation is provided for a single identifier (i.e. character string with 'sep' separating multiple annotations), the mulitple annotation is split and the corresponding vector is returned as an element of list 'annotation'. 
}
\value{
  \item{annotation}{list of length 'length(identifier)' the 'i'-th element of which contains the annotation for 'identifier[i]'.}
  \item{empty}{logical vector of length 'length(identifier)'. 'empty[i]' is TRUE if 'identifier[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(identifier)'. 'noentry[i]' is TRUE if 'identifier[i]' cannot be found in 'annot[,identifierCol]'.}
  \item{noannotation}{locial vector of length 'length(identifier)'. 'noannotation[i]' is TRUE if 'a[i]==noAnnotationProvidedSymbol' is TRUE.}
}
\references{}
\author{Alexandre Kuhn}
\note{Use \code{getMULTIANNOTATION} if the identifiers occur on more than one line in the annotation table.

}
\seealso{\code{\link{getMULTIANNOTATION}}}
\examples{
##example annotation table
annotation<-cbind(gene=c('gene_1a, gene_1b','gene_2','gene_3','gene_4'),probe=c('probe_1','probe_2','probe_3','probe_4'),sequence=c('sequence_1','sequence_2a, sequence_2c','sequence_3',''))
print(annotation)

##get sequences for probe_2, probe_3, probe_4 and probe_100
myProbes<-c('probe_2','probe_3','probe_4','probe_100',NA)
getANNOTATION(myProbes,annotation,identifierCol=2,annotationCol=3,noAnnotationProvidedSymbol='',sep=', ')

##track origin of annotation failure for the last 3 probes
getANNOTATION(myProbes,annotation,identifierCol=2,annotationCol=3,noAnnotationProvidedSymbol='',sep=', ',diagnose=TRUE)
}
\keyword{manip}
