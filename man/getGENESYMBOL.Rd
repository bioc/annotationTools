\name{getGENESYMBOL}
\alias{getGENESYMBOL}
\title{Find gene symbols}
\description{
  Takes a vector of probe set identifiers and an annotation table and retrieves the corresponding gene symbols.
}
\usage{
getGENESYMBOL(ps, annot, diagnose = FALSE, GScol = 15, noGSsymbol = NA, noGSprovidedSymbol = "---", sep = " /// ")
}
\arguments{
  \item{ps}{character vector containing the probe sets identifiers.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{GScol}{column in annotation table containing the gene IDs.}
  \item{noGSsymbol}{character string to be used in output list 'symbols' if no gene symbol is found or provided in the annotation table.}
  \item{noGSprovidedSymbol}{character string used in annotation table and indicating missing gene symbol.}
  \item{sep}{character string used in annotation table to separate multiple gene symbols.}
}
\details{
This function can be used with Affymetrix annotation files (e.g. 'HG-U133\_Plus\_2\_annot.csv'). It retrieves the gene symbols corresponding to particular probe set identifiers.

Gene symbols are returned as elements of list 'symbols'. If multiple gene symbols are provided for 'ps[i]' (with 'sep' separating gene symbols in the annotation table), a vector containing all gene symbols is returned as the 'i-th' element of list 'symbols'.

The default values for 'GScol', 'noGSsymbol', 'noGSprovidedSymbol' and 'sep' are chosen to suit the format of Affymetrix annotation files. However, these options can be set to look up any annotation table, provided the probe set identifiers are in the first column and occur only once.
}
\value{
  \item{symbols}{list of length 'length(ps)' the 'i'-th element of which contains the gene symbol for 'ps[i]'.}
  \item{empty}{logical vector of length 'length(ps)'. 'empty[i]' is TRUE if 'ps[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(ps)'. 'noentry[i]' is TRUE if 'ps[i]' cannot be found in the first column of the annotation table.}
  \item{nogs}{locial vector of length 'length(ps)'. 'nogs[i]' is TRUE if 'symbols[i]==noIDprovidedSymbol' is TRUE.}
}
\references{}
\author{Alexandre Kuhn}
\note{\code{getANNOTATION} provides a more flexible solution to be used with arbitrary annotation tables.
}
\seealso{\code{\link{getANNOTATION}}}
\examples{
##example Affymetrix annotation file and its location
annotationFile<-'HG-U133_Plus_2_annot_part.csv'
dataDirectory<-system.file('data',package='annotationTools')

##load annotation file
annotation<-read.csv(paste(dataDirectory,annotationFile,sep='/'),colClasses='character',comment.char='#')

##get gene symbols
myPS<-c('117_at','1007_s_at','1552288_at',NA,'xyz_at')
getGENESYMBOL(myPS,annotation)

##track origin of annotation failure for the 3 last probe set IDs
getGENESYMBOL(myPS,annotation,diagnose=TRUE)
}
\keyword{manip}
