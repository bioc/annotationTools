\name{getOrthologousProbesets}
\alias{getOrthologousProbesets}
\title{Find orthologous/homologous probe sets present in a target set using a mapping table}
\description{
Used for cross-species analysis of gene expression profiles. Takes a vector of probe sets (species 1), a data.frame containing a second set of probe sets (species 2) and associated values (e.g. log fold change, t-statistic, ...) , a mapping table of orthologous probe sets (species 1 to species 2) and returns the list of probe sets orthologous to those in the first vector and found in the second set.
}
\usage{
getOrthologousProbesets(ps1,ps2,ps2ps,fct=function(x){x},forceProbesetSelection=FALSE)
}
\arguments{
  \item{ps1}{A vector of probe sets identifiers.}
  \item{ps2}{A data.frame where the first column contains the target probe sets. The second column contains quantities associated with each probe set like e.g. log fold change, t-statistic, etc.}
  \item{ps2ps}{A data.frame containing a mapping table of orthologous probe sets. Can be generated with the function ps2ps.R.}
  \item{fct}{A function for probe sets selection. The function is applied to the values (second column of ps2) associated with all orthologous probe sets corresponding to a given probe set in ps1.}
  \item{forceProbesetSelection}{Logical. If FALSE (default), all orthologous probe sets found in ps2 are returned. If TRUE, only the probe sets associated with values selected with the function fct are returned.}
}
\details{
Each probe set in the first input vector (ps1) is looked up in the mapping table (ps2ps). Orthologous probe sets given in the mapping table and present in the second input argument (more precisely in the first column of ps2) are returned as a list (ps2\_probeSel). By default, values associated with these orthologous probe sets (and given in the second column of ps2) are returned as a list too (ps2\_value).

A function can be specified and applied to the values associated with the orthologous probe sets. This can for instance be used to assign a summary value associated with a set orthologous probe sets (e.g. fct=mean or fct=median in case the associated values are log fold changes). Alternatively, this can be used to filter a single probe set out of multiple orthologous probe sets (e.g. fct=min in case the associated value is a p-value) (see below).

By default, the function returns all orthologous probe sets found. You can force the function to return only the probe sets associated with the values selected by the application of the user-specified function (specify forceProbesetSelection=TRUE). For instance, if the values in the second column of ps2 are p-values and fct=min, for each probe set in ps1 the minimal p-value associated with orthologous probe sets in ps2 is returned (ps2\_value) as well as the probe set associated with the minimal p-value (ps2\_probeSel).
}
\value{
  \item{ps2\_probeSel}{A list of orthologous probe sets given by the mapping table (ps2ps) and present in ps2. By default, all orthologous probe sets are returned. If forceProbesetSelection=TRUE, only the probe sets whose values are matched after application of the user-defined function (fct) are returned.}
  \item{ps2\_value}{A list of values associated with the orthologous probe sets. By default, all values are returned. If a function is specified (fct), it is applied to the values before they are returned.}
}
\references{Kuhn et al. Cross-species and cross-platform gene expression studies with the Bioconductor-compliant R package 'annotationTools'. BMC Bioinformatics, 9:26 (2008)}
\author{Alexandre Kuhn}
\examples{
data(orthologs_example)

##select the first 3 probe sets listed in 'table_mouse' and their orthologs in 'table_human'
##note that no ortholog is found for the top mouse probe set
table_mouse[1:3,]
orthops<-getOrthologousProbesets(table_mouse[1:3,1],table_human,ortho)
orthops[[1]]

##the second item returned contains the values associated with orthologs (second column of 'table_human')
orthops[[2]]

##calculates, for each mouse probe set, the median orthologous log fold change (in case of multiple orthologs)
##(in this case log fold changes need to be in the second column of 'table_mouse')
orthops<-getOrthologousProbesets(table_mouse[1:3,1],table_human,ortho,'median')
orthops[[2]]

##for each mouse probe set having multiple orthologous human probe sets
##select the orthologous probe set with the smallest p-value (column 4 of 'table_human')
orthops<-getOrthologousProbesets(table_mouse[1:3,1],table_human[,c(1,4)],ortho,'min',forceProbesetSelection=TRUE)
orthops[[1]]

##orthologous probe set selection can based on arbitrary functions
##e.g. select the 2 orthologous probe sets with the smallest p-values
orthops<-getOrthologousProbesets(table_mouse[1:3,1],table_human[,c(1,4)],ortho,function(x){sort(x)[1:2]},forceProbesetSelection=TRUE)
orthops[[1]]
}
\keyword{manip}
