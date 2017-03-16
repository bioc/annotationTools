\name{listToCharacterVector}
\alias{listToCharacterVector}
\title{Turn list into character vector}
\description{
  Takes a list and returns a character vector by (separately) concatenating the vectors in the list.
}
\usage{
listToCharacterVector(lst,sep=' /// ')
}
\arguments{
  \item{lst}{list object}
  \item{sep}{character string specifying the separator to concatenate each elements of vectors in the list}
}
\details{
The output vector has same length as the input list.
}
\value{
  \item{v}{vector of length 'length(lst)' where the 'i'-th element contains the concatenation of the 'i'-th element of 'lst'.}
}
\references{Kuhn et al. Cross-species and cross-platform gene expression studies with the Bioconductor-compliant R package 'annotationTools'. BMC Bioinformatics, 9:26 (2008)}
\author{Alexandre Kuhn}
\examples{
##an example list
lst<-vector('list',3)
lst[[1]]<-c('a1','a2')
lst[[2]]<-'b'
lst[[3]]<-c('c1','c2','c3')
##merge first 2 list elements
listToCharacterVector(lst,sep=', ')
}
\keyword{manip}
