%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationDavidCsv.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationDavidCsv}
\docType{class}
\alias{AnnotationDavidCsv}


\title{The AnnotationDavidCsv class}

\description{
 Package:   \cr
\bold{Class AnnotationDavidCsv}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{AnnotationDavid}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{AnnotationDavidCsv}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{AnnotationDavidCsv}\cr
extends \link{AnnotationDavid}\cr



The AnnotationDavidCsv class encapsulates the functionality allowing to retrieve data from the
DAVID backend annotation file system through the Annotation.getIdMap() and Annotation.getDataFrame()
calls on this object. The DAVID backend annotation file system requires to submit the request through the
e-mail for convertion of one ID type to another. Within a day, the user recieves an email reply containing the
link to the resulting conversion file.The results are returned in a form of a tab delimited headerless file containing
the match pairs, one pair per line. The AnnotationDavidCsv object encapsulates the functionality allowing to interactively choose
the resulting files and convert it into a data frame during the
Annotation.getIdMap() and Annotation.getDataFrame() calls on the AnnotationDavidCsv object.
}

\usage{AnnotationDavidCsv(cacheFolderName="DavidCsv", primaryColumn="AFFY_ID", secondaryColumn="UNIPROT_ACCESSION", swap=TRUE, df_filename=NULL, ...)}

\arguments{
\item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationDavidCsv object.}
\item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'AFFY_ID'.}
\item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'UNIPROT_ACCESSION'.}
\item{swap}{ Logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
retrieval during the getIdMap() call.Default is \code{\link[base:logical]{TRUE}}.}
\item{df_filename}{ Character string or \code{\link[base]{NULL}}. In the first case the character string contains the name of conversion results file
and in the second case the file name is determined interactively through the Open File dialog during the call to
getIdMap() or getDataFrame() on the AnnotationDavidCsv object.}
\item{...}{ Additional parameters, see \code{\link{Annotation}}.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\emph{No public methods defined}.


 \bold{Methods inherited from AnnotationDavid}:\cr
getColumns, readDF

 \bold{Methods inherited from Annotation}:\cr
getArrayType, getArrayTypes, getColumns, getCredentials, getDataFrame, getFolderName, getIdMap, getRoot, getServiceRoot, init, readDF, setCredentials, setOptions

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Alex Lisovich, Roger Day}
\keyword{classes}
