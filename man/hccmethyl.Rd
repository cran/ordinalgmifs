\name{hccmethyl}
\alias{hccmethyl}
\docType{data}
\title{
ExpressionSet for Liver Cancer Methylation Data
}
\description{
This is a BioConductor ExpressionSet version of the data in \code{hccframe}. These data are a subset of subjects and CpG sites reported in the original paper where liver samples were assayed using the Illumina GoldenGate Methylation BeadArray Cancer Panel I. Technical replicate samples
were removed to ensure all samples were independent. The matched cirrhotic samples from subjects with hepatocellular carcinoma (HCC, labeled Tumor) were also excluded. Therefore methylation levels in liver tissue are provided for independent subjects whose liver was Normal (N=20),
cirrhotic but not having HCC (N=16, Cirrhosis non-HCC), and HCC (N=20, Tumor). 
}
\usage{data(hccmethyl)}
\format{
  An ExpressionSet. CpG site methylation data can be extracted using \code{exprs(hccmethyl)} and yields a matrix with 45 rows (CpG sites) and   
  56 columns (samples). Phenotypic data can be extracted using \code{pData(hccmethyl)}. The variables in the \code{phenoData} are
  \describe{
    \item{\code{title}}{a unique sample identifier}
    \item{\code{geo_accession}}{the Gene Expression Omnibus sample identifier}
    \item{\code{group}}{an ordered factor with levels \code{Normal} < \code{Cirrhosis non-HCC} < \code{Tumor}}
  }
}
\source{
The full dataset is available as GSE18081 from Gene Expression Omnibus at \url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE18081}
}
\references{
Archer KJ, Mas VR, Maluf DG, Fisher RA. High-throughput assessment of CpG site methylation for distinguishing between HCV-cirrhosis and HCV-associated hepatocellular carcinoma. Molecular Genetics and Genomics, 283(4): 341-349, 2010.
}
\examples{
\donttest{
library("Biobase")
data(hccmethyl)
# To minimize processing time, only a subset of 
# two CpG sites (DDIT3_P1313_R and HDAC9_P137_R) are included as penalized covariates
# in this demonstration.
hcc.fit<-ordinal.gmifs(group~1, x=t(exprs(hccmethyl)[c("DDIT3_P1313_R","HDAC9_P137_R"),]), 
	data=pData(hccmethyl))
}
}
\keyword{datasets}
