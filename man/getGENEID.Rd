\name{getGENEID}
\alias{getGENEID}
\title{Find gene IDs}
\description{
  Takes a vector of probe set identifiers and an annotation table and retrieves the corresponding gene IDs.
}
\usage{
getGENEID(ps, annot, diagnose = FALSE, idCol = 19, noIDsymbol = NA, noIDprovidedSymbol = "---", sep = " /// ")
}
\arguments{
  \item{ps}{character vector containing the probe sets identifiers.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{idCol}{column in annotation table containing the gene IDs.}
  \item{noIDsymbol}{character string to be used in output list 'geneid' if no gene ID is found or provided in the annotation table.}
  \item{noIDprovidedSymbol}{character string used in annotation table and indicating missing gene ID.}
  \item{sep}{character string used in annotation table to separate multiple gene IDs.}
}
\details{
This function can be used with Affymetrix annotation files (e.g. 'HG-U133\_Plus\_2\_annot.csv'). It retrieves gene IDs corresponding to particular probe set identifiers.

Gene IDs are returned as elements of list 'geneid'. If multiple gene IDs are provided for 'ps[i]' (with 'sep' separating gene IDs in the annotation table), a vector containing all gene IDs is returned as the 'i-th' element of list 'geneid'. 

The default values for 'idCol', 'noIDsymbol', 'noIDprovidedSymbol' and 'sep' are chosen to suit the format of Affymetrix annotation files. However, options can be set to look up any annotation table, provided the probe set identifiers are in the first column and occur only once.
}
\value{
  \item{geneid}{list of length 'length(ps)' the 'i'-th element of which contains the gene ID for 'ps[i]'.}
  \item{empty}{logical vector of length 'length(ps)'. 'empty[i]' is TRUE if 'ps[i]' is empty or NA.}
  \item{noentry}{logial vector of length 'length(ps)'. 'noentry[i]' is TRUE if 'ps[i]' cannot be found in the first column of the annotation table.}
  \item{noid}{logial vector of length 'length(ps)'. 'noid[i]' is TRUE if 'geneid[i]==noIDprovidedSymbol' is TRUE.}
}
\references{}
\author{Alexandre Kuhn, \email{alexandre.kuhn@isb-sib.ch}}
\note{\code{getANNOTATION} provides a more flexible solution to be used with arbitrary annotation tables.
}
\seealso{\code{\link{getANNOTATION}}}
\examples{
##example Affymetrix annotation file and its location
annotationFile<-'HG-U133_Plus_2_annot_part.csv'
dataDirectory<-system.file('data',package='annotationTools')

##load annotation file
annotation<-read.csv(paste(dataDirectory,annotationFile,sep='/'),colClasses='character')

##get gene IDs
myPS<-c('117_at','1007_s_at','1552288_at',NA,'xyz_at')
getGENEID(myPS,annotation)

##track origin of annotation failure for the 3 last probe set IDs
getGENEID(myPS,annotation,diagnose=TRUE)
}
\keyword{manip}
