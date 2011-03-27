\name{getGENEONTOLOGY}
\alias{getGENEONTOLOGY}
\title{Find Gene Ontology (GO) annotation}
\description{
  Takes a vector of probe set identifiers and an annotation table and retrieves the corresponding GO annotation.
}
\usage{
getGENEONTOLOGY(ps, annot, diagnose = FALSE, specifics = 0, GOcol = 31, noGOsymbol = NA, noGOprovidedSymbol = "---", sep = " /// ")
}
\arguments{
  \item{ps}{character vector containing the probe sets identifiers.}
  \item{annot}{annotation table (data frame) where each row is a record and each column is an annotation field.}
  \item{diagnose}{logical. If TRUE, 3 (logical) vectors used for diagnostic purpose are returned in addition to the annotation. If FALSE (default) only the annotation is returned.}
  \item{specifics}{can take value 0, 1, 2, 3, ... . If specifics=i with i>0, the GO biological process annotation is parsed (using " // " as separator) and the i-th part of the expression is returned. If specifics=0, the GO biological process annotation is not parsed.}
  \item{GOcol}{column in annotation table containing the GO biological processes.}
  \item{noGOsymbol}{character string to be used in output list 'go' if no GO biological process is found or provided in the annotation table.}
  \item{noGOprovidedSymbol}{character string used in annotation table and indicating missing GO biological process.}
  \item{sep}{character string used in annotation table to separate multiple GO biological processes.}
}
\details{
This function can be used with Affymetrix annotation files (e.g. 'HG-U133\_Plus\_2\_annot.csv'). It retrieves GO annotation corresponding to particular probe set identifiers. GO biological processes are returned by default ('GOcol'=31) but GO cellular components ('GOcol'=32) or GO molecular functions ('GOcol'=33) can be returned by setting 'GOcol' appropriately.

GO biological processes are returned as elements of list 'go'. If multiple GO biological processes are provided for 'ps[i]' (with 'sep' separating GO biological processes in the annotation table), a vector containing all GO biological processes is returned as the 'i-th' element of list 'go'. 

The default values for 'GOcol', 'noGOsymbol', 'noGOprovidedSymbol' and 'sep' are chosen to suit the format of Affymetrix annotation files. However, options can be set to look up any annotation table, provided the probe set identifiers are in the first column and occur only once.

Note that each GO annotation in Affymetrix annotation files contains 3 attributes: the GO biological process ID, term and quality, separated by " // ". Setting the option 'specifics' to 1, 2, or 3 allows to retrieve any of the 3 attributes separately.
}
\value{
  \item{go}{list of length 'length(ps)' the 'i'-th element of which contains the GO annotation for 'ps[i]'.}
  \item{empty}{logical vector of length 'length(ps)'. 'empty[i]' is TRUE if 'ps[i]' is empty or NA.}
  \item{noentry}{locial vector of length 'length(ps)'. 'noentry[i]' is TRUE if 'ps[i]' cannot be found in the first column of the annotation table.}
  \item{nogo}{locial vector of length 'length(ps)'. 'nogo[i]' is TRUE if 'go[i]==noIDprovidedSymbol' is TRUE.}
}
\references{}
\author{Alexandre Kuhn}
\note{\code{getANNOTATION} provides a more flexible solution to be used with arbitrary annotation tables.
}
\seealso{\code{\link{getANNOTATION}}}
\examples{
##example Affymetrix annotation file and its location
annotationFile<-system.file('extdata','HG-U133_Plus_2_annot_part.csv',package='annotationTools')

##load annotation file
annotation<-read.csv(annotationFile,colClasses='character',comment.char='#')

##get gene GO biological process (full information)
myPS<-c('117_at','1007_s_at','1552288_at',NA,'xyz_at')
getGENEONTOLOGY(myPS,annotation)

##get gene GO biological process terms only
getGENEONTOLOGY(myPS,annotation,specifics=2)

##track origin of annotation failure for the 3 last probe set IDs
getGENEONTOLOGY(myPS,annotation,diagnose=TRUE)

##GO molecular functions are contained in column 33 of the annotation
colnames(annotation)

##get gene GO molecular functions
getGENEONTOLOGY(myPS,annotation,GOcol=33)
}
\keyword{manip}
