%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotation.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setOptions.Annotation}
\alias{setOptions.Annotation}
\alias{Annotation.setOptions}
\alias{setOptions.Annotation}
\alias{setOptions,Annotation-method}

\title{Set the parameters for an annotation object}

\description{Set the parameters for an annotation object.}

\usage{\method{setOptions}{Annotation}(this, ...)}

\arguments{
\item{...}{Parameters to be set for a given annotation object.}
}

\examples{
\dontrun{
   #create Ensembl annotation object
   annObj<-AnnotationEnsemblCsv("EnsemblCsv");
   #set the name of the file the annotation data to be retrieved from
   setOptions(annObj,df_filename="ENSEMBL_biomart_export.txt");
}}

\seealso{For more information see \code{\link{Annotation}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
