\name{getGENETITLE}
\alias{getGENETITLE}
\title{Find gene titles}
\description{
  Takes a vector of probe set identifiers and an annotation table and retrieves the corresponding gene titles.
}
\usage{
getGENETITLE(ps, annot, diagnose = FALSE, TITLEcol = 14, noTITLEsymbol = NA, noTITLEprovidedSymbol = "---", sep = " /// ")
}
\arguments{
  \item{ps}{character vector containing the probe sets identifiers.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{TITLEcol}{column in annotation table containing the gene titles.}
  \item{noTITLEsymbol}{character string to be used in output list 'genetitle' if no gene title is found or provided in the annotation table.}
  \item{noTITLEprovidedSymbol}{character string used in annotation table and indicating missing gene title.}
  \item{sep}{character string used in annotation table to separate multiple gene titles.}
}
\details{
This function can be used with Affymetrix annotation files (e.g. 'HG-U133\_Plus\_2\_annot.csv'). It retrieves the gene titles corresponding to particular probe set identifiers.

Gene titles are returned as elements of list 'genetitle'. If multiple gene titles are provided for 'ps[i]' (with 'sep' separating gene titles in the annotation table), a vector containing all gene titles is returned as the 'i-th' element of list 'genetitle'.

The default values for 'TITLEcol', 'noTITLEsymbol', 'noTITLEprovidedSymbol' and 'sep' are chosen to suit the format of Affymetrix annotation files. However, these options can be set to look up any annotation table, provided the probe set identifiers are in the first column and occur only once.
}
\value{
  \item{genetitle}{list of length 'length(ps)' the 'i'-th element of which contains the gene title for 'ps[i]'.}
  \item{empty}{logical vector of length 'length(ps)'. 'empty[i]' is TRUE if 'ps[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(ps)'. 'noentry[i]' is TRUE if 'ps[i]' cannot be found in the first column of the annotation table.}
  \item{notitle}{locial vector of length 'length(ps)'. 'noid[i]' is TRUE if 'genetitle[i]==noTITLEprovidedSymbol' is TRUE.}
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

##get gene titles
myPS<-c('117_at','1007_s_at','1552288_at',NA,'xyz_at')
getGENETITLE(myPS,annotation)

##track origin of annotation failure for the 3 last probe set IDs
getGENETITLE(myPS,annotation,diagnose=TRUE)
}
\keyword{manip}
