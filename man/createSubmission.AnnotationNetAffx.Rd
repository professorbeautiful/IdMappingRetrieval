%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationNetAffx.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{createSubmission.AnnotationNetAffx}
\alias{createSubmission.AnnotationNetAffx}
\alias{AnnotationNetAffx.createSubmission}
\alias{createSubmission.AnnotationNetAffx}
\alias{createSubmission,AnnotationNetAffx-method}


\title{Create a set of files for NetAffx batch query}

\synopsis{createSubmission.AnnotationNetAffx(static, chunk=9999, folder="./NetAffx_submission", verbose=TRUE, ...)}
\usage{AnnotationNetAffx$createSubmission(chunk=9999, folder="./NetAffx_submission", verbose=TRUE, ...)}

\description{
This is a convinience function generating a set of files suitable
for a submission to the Affymetrix NetAffx batch service from the list of probe set
IDs obtained inside the this function through the call to AnnotationAffx.getProbesetList(),
allowing user to choose the array type interactively.
}

\arguments{
\item{chunk}{ maximum number of probeset IDs per submission file to comply with
submission size restrictions of the NetAffx batch query system (10000). Defaul is 9999.}
\item{folder}{ Name of the folder where generated files will be saved. Default is './NetAffx_submission'.}
\item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
\item{...}{Not used}
}

\examples{\dontrun{AnnotationNetAffx$createSubmission();}}

\seealso{For more information see \code{\link{AnnotationNetAffx}}.}

\author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
