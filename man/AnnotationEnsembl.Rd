%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  annotationEnsembl.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{AnnotationEnsembl}
\docType{class}
\alias{AnnotationEnsembl}


\title{The AnnotationEnsembl class}

\description{
 Package:   \cr
\bold{Class AnnotationEnsembl}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{Annotation}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{AnnotationEnsembl}\cr

\bold{Directly known subclasses:}\cr
\link{AnnotationEnsemblCsv}\cr

public static class \bold{AnnotationEnsembl}\cr
extends \emph{\link{Annotation}}\cr



The AnnotationEnsembl class encapsulates the functionality allowing to retrieve data from the
Ensembl BioMart online query system using biomaRt R package through the Annotation.getIdMap() and Annotation.getDataFrame()
calls on this object.
}

\usage{AnnotationEnsembl(cacheFolderName="Ensembl", primaryColumn=c("uniprot_swissprot_accession", "uniprot_sptrembl"), secondaryColumn=NA, swap=FALSE, species="hsapiens_gene_ensembl", full.merge=TRUE, ...)}


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
If \code{\link[base]{NA}}, no filtering is performed. Default is 'hsapiens_gene_ensembl'.}
\item{full.merge}{ A \code{\link[base]{logical}} indicating which version of primary columns merging algorithm to use. If@ TRUE (default), all unique pairs
<probeset ID, SwissProt> and <probeset ID, Trembl> are generated, and if \code{\link[base:logical]{FALSE}}, only those pairs from <probeset ID, Trembl>
for which Uniprot ID is not present in <probeset ID, SwissProt> pairs are included.}
\item{...}{Additional parameters, see \code{\link{Annotation}}.}
}

\examples{
\dontrun{
 Annotation$init();
 #create Ensembl annotation object
 annObj<-AnnotationEnsembl(species="hsapiens_gene_ensembl");
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
