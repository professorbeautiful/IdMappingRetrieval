%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationNetAffx.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationNetAffx}
\docType{class}
\alias{AnnotationNetAffx}


\title{The AnnotationNetAffx class}

\description{
 Package:   \cr
\bold{Class AnnotationNetAffx}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{AnnotationAffx}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{AnnotationNetAffx}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{AnnotationNetAffx}\cr
extends \link{AnnotationAffx}\cr



The AnnotationNetAffx class encapsulates the functionality allowing to retrieve data from the
NetAffx batch query system through the Annotation.getIdMap() and Annotation.getDataFrame()
calls on this object. The NetAffx batch query system requires to submit the probeset IDs by providing
text files in a special format, maximum 10000 IDs per file. The results are returned in a form of a tab
delimited text file, one file per submission, so the query results for a whole array are presented by a set
of such files. The AnnotationNetAffx object encapsulates the functionality allowing to interactively choose
the set of result files and merge them into a single data frame during the
Annotation.getIdMap() and Annotation.getDataFrame() calls on the AnnotationNetAffx object.
}

\usage{AnnotationNetAffx(cacheFolderName="NetAffxCsv", primaryColumn="Probe.Set.ID", secondaryColumn="SwissProt", swap=TRUE, df_filename=NULL, ...)}

\arguments{
\item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationNetAffx object.}
\item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'Probe.Set.ID'.}
\item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix
annotation repository when getIdMap() is called. Default is 'SwissProt'.}
\item{swap}{A \code{\link[base]{logical}} indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
retrieval during the getIdMap() call.Default is \code{\link[base:logical]{TRUE}}.}
\item{df_filename}{ Character vector, character string or \code{\link[base]{NULL}}. In the first case the character vector contains the names
of a resulting file set, in the second, the character string contains the name of directory in which the files are stored,
and in the third the file set is determined interactively through the Open File dialog during the call to
getIdMap() or getDataFrame() on the AnnotationAffxCsv object.}
\item{...}{Additiional parameters, see \code{\link{Annotation}}.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
}


 \bold{Methods inherited from AnnotationAffx}:\cr
getColumns, getProbesetList, readDF, setCredentials

 \bold{Methods inherited from Annotation}:\cr
getArrayType, getArrayTypes, getColumns, getCredentials, getDataFrame, getFolderName, getIdMap, getRoot, getServiceRoot, init, readDF, setCredentials, setOptions

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Alex Lisovich, Roger Day}
\keyword{classes}
