\name{getPROBESET}
\alias{getPROBESET}
\title{Find probe set IDs}
\description{
  Takes a vector of gene IDs (or identifiers of other types) and an annotation table and looks up the gene IDs in the table to retrieve the corresponding probe set identifiers. Each gene ID can occur multiple times (i.e. on mulitple lines) in the annotation table.
}
\usage{
getPROBESET(geneid, annot, uniqueID = FALSE, diagnose = FALSE, idCol = 19, noPSsymbol = NA, noPSprovidedSymbol = "---")
}
\arguments{
  \item{geneid}{character vector containing the gene IDs.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{uniqueID}{logical. If TRUE, only probe set IDs annotated with a single gene ID are returned. If FALSE, probe set IDs annotated with multiple gene IDs are returned too.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{idCol}{column in annotation table containing the gene identifiers.}
  \item{noPSsymbol}{character string to be used in output list 'ps' if no probe set ID is found or provided in the annotation table.}
  \item{noPSprovidedSymbol}{character string used in annotation table and indicating missing probe set ID.}
}
\details{
This function can be used with Affymetrix annotation files (e.g. 'HG-U133\_Plus\_2\_annot.csv'). It retrieves probe set IDs corresponding to particular gene identifiers. By default, the function takes gene IDs but any type of identifier (e.g. gene symbol) can be used (set 'idCol' accordingly).

Probe set IDs are returned as elements of list 'ps'. If multiple probe set IDs are found for 'geneid[i]', a vector containing all probe set IDs is returned as the 'i-th' element of list 'ps'. 

The default values for 'idCol', 'noPSsymbol', and 'noPSprovidedSymbol' are chosen to suit the format of Affymetrix annotation files. However, options can be set to look up any annotation table, provided the probe set identifiers are in the first column.
}
\value{
  \item{ps}{list of length 'length(geneid)' the 'i'-th element of which contains the probe set IDs for 'geneid[i]'.}
  \item{empty}{logical vector of length 'length(geneid)'. 'empty[i]' is TRUE if 'geneid[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(geneid)'. 'noentry[i]' is TRUE if 'geneid[i]' cannot be found in column 'idCol' (default is column 19) of the annotation table.}
  \item{noid}{locial vector of length 'length(geneid)'. 'noid[i]' is TRUE if 'ps[i]==noIDprovidedSymbol' is TRUE.}
}
\references{}
\author{Alexandre Kuhn}
\note{\code{getMULTIANNOTATION} provides a more flexible solution that can be used with arbitrary annotation tables.
}
\seealso{\code{\link{getMULTIANNOTATION}}}
\examples{
##example Affymetrix annotation file and its location
annotationFile<-system.file('extdata','HG-U133_Plus_2_annot_part.csv',package='annotationTools')

##load annotation file
annotation<-read.csv(annotationFile,colClasses='character',comment.char='#')

##genes of interest
myGenes<-c('DDR1','GUCA1A','HSPA6',NA,'XYZ')

##column 15 in annotation contains gene symbols
colnames(annotation)

##find probe sets probing for particular genes 
getPROBESET(myGenes,annotation,idCol=15)

##find probe sets probing only for the genes of interest (i.e. with unique annotation)
getPROBESET(myGenes,annotation,idCol=15,uniqueID=TRUE)

##track origin of annotation failure for the 2 last probe set IDs
getPROBESET(myGenes,annotation,idCol=15,diagnose=TRUE)
}
\keyword{manip}
