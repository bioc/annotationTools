\name{compactList}
\alias{compactList}
\title{Concatenate list elements}
\description{
  Concatenates given elements of a list.
}
\usage{
compactList(lst,l)
}
\arguments{
  \item{lst}{list object}
  \item{l}{numeric vector specifying the number of list elements to be concatenated sequentially}
}
\details{
By definition, the output list is shorter than the input list.

Function stops if 'sum(l)' does not equal 'lenght(lst)'.
}
\value{
  \item{clst}{list of length 'length(l)' where the 'i'-th element has length 'l[i]' and contains concatenated elements of input list 'lst'.}
}
\references{Kuhn et al. Cross-species and cross-platform gene expression studies with the Bioconductor-compliant R package 'annotationTools'. BMC Bioinformatics, 9:26 (2008)}
\author{Alexandre Kuhn}
\examples{
##an example list
lst<-vector('list',3)
lst[[1]]<-c('aaa','bbb')
lst[[2]]<-'ccc'
lst[[3]]<-'zzz'
##merge first 2 list elements
compactList(lst,c(2,1))
}
\keyword{manip}
