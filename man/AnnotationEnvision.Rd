%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationEnvision.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationEnvision}
\docType{class}
\alias{AnnotationEnvision}


\title{The AnnotationEnvision class}

\description{
 Package:   \cr
\bold{Class AnnotationEnvision}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{AnnotationEnvision}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{AnnotationEnvision}\cr
extends \emph{\link{Annotation}}\cr



The AnnotationEnvision class encapsulates the functionality allowing to retrieve data from the
Envision online query system.The Envision online query system allows to retrieve the
ID matching information accompanied by multiple attributes
like species and the microarray chip type in the form of the xml file.
The AnnotationEnvision class encapsulates the functionality allowing to filter the Envision query results
on species and microarray type attributes and convert it into a data frame during the
 getIdMap() and getDataFrame() calls on the AnnotationEnvision object.
}

\usage{AnnotationEnvision(cacheFolderName="EnVision", primaryColumn=c("UniProt.SwissProt.Accession", "UniProt.TrEMBL.Accession"), secondaryColumn=NA, swap=TRUE, species="Homo sapiens", full.merge=TRUE, ...)}


\arguments{
\item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationEnsembl object.}
\item{primaryColumn}{ Primary column(s) to be retrieved from a data frame obtained from the Ensembl csv file
when getIdMap() is called. As the Ensembl returns the match results for SwissProt and Tremb accessions in separate columns,
it is possible to retrieve either or them or merge them together by explicetely specifying the set of columns to be merged.
Default is c('uniprot_swissprot_accession','uniprot_sptrembl').}
\item{secondaryColumn}{ secondaryColumn Secondary column (containing probeset IDs) to be retrieved from a data frame obtained from the Ensembl csv file
when getIdMap() is called. If \code{\link[base]{NA}} (default), the column name(s) derived automatically from the array type parameter during the getDataFrame() call.
It should be noted that the probeset ID column name in Ensembl data format is array specific ('Affy.HG.U133.PLUS.2' for example) and therefore needs
to be selected on per array basis if specified explicitely.}
\item{swap}{ A \code{\link[base]{logical}} indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
retrieval during the getIdMap() call.Default is \code{\link[base:logical]{TRUE}}.}
\item{species}{Character vector or \code{\link[base]{NA}} indicating if filtering of the results on a particular set of species
should be performed if a given service provides the species information.
If \code{\link[base]{NA}}, no filtering is performed. Default is 'Homo sapiens'.}
\item{full.merge}{ A \code{\link[base]{logical}} indicating which version of primary columns merging algorithm to use. If \code{\link[base:logical]{TRUE}} (default), all unique pairs
<probeset ID, SwissProt> and <probeset ID, Trembl> are generated, and if \code{\link[base:logical]{FALSE}}, only those pairs from <probeset ID, Trembl>
for which Uniparot ID is not present in <probeset ID, SwissProt> pairs are included.}
\item{...}{Additional parameters, see \code{\link{Annotation}}.}
}

\examples{
\dontrun{
 Annotation$init();
 #create Envision annotation object
 annObj<-AnnotationEnvision(species="Homo sapiens");
}}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\emph{No public methods defined}.


 \bold{Methods inherited from Annotation}:\cr
getArrayType, getArrayTypes, getColumns, getCredentials, getDataFrame, getFolderName, getIdMap, getRoot, getServiceRoot, init, readDF, setCredentials, setOptions

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Alex Lisovich, Roger Day}
\keyword{classes}
