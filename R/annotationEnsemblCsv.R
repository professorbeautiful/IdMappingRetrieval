

###########################################################################/**
# @RdocClass AnnotationEnsemblCsv 
#
# @title "The AnnotationEnsemblCsv class"
#
# \description{
#  @classhierarchy
#
# The AnnotationEnsemblCsv class encapsulates the functionality allowing to retrieve data from the
# Ensembl intercative online query system. The ID matching information fitered on
# species and the microarray chip type is retrieved as comma delimited csv file. 
# The AnnotationEnsemblCsv object encapsulates the functionality allowing to interactively choose
# the Ensembl query results csv file and convert it into a data frame during the
# getIdMap() and getDataFrame() calls on the AnnotationEnsemblCsv object.
# }
#
# @synopsis
#
#
# \arguments{
# \item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationEnsembl object.}
# \item{primaryColumn}{ Primary column(s) to be retrieved from a data frame obtained from the Ensembl csv file 
# when getIdMap() is called. As the Ensembl returns the match results for SwissProt and Tremb accessions in separate columns,
# it is possible to retrieve either or them or merge them together by explicetely specifying the set of columns to be merged.
# Default is c('uniprot_swissprot_accession','uniprot_sptrembl').}
# \item{secondaryColumn}{ secondaryColumn Secondary column (containing probeset IDs) to be retrieved from a data frame obtained from the Ensembl csv file 
# when getIdMap() is called. If @NA (default), the column name(s) derived automatically from the array type parameter during the getDataFrame() call.
# It should be noted that the probeset ID column name in Ensembl data format is array specific ('Affy.HG.U133.PLUS.2' for example) and therefore needs
# to be selected on per array basis if specified explicitely.}
# \item{swap}{ A @logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
# retrieval during the getIdMap() call.Default is @TRUE.}
# \item{full.merge}{ A @logical indicating which version of primary columns merging algorithm to use. If@ TRUE (default), all unique pairs 
# <probeset ID, SwissProt> and <probeset ID, Trembl> are generated, and if @FALSE, only those pairs from <probeset ID, Trembl> 
# for which Uniprot ID is not present in <probeset ID, SwissProt> pairs are included.}
# \item{df_filename}{ Character string or NULL. In the first case the character string contains the name of conversion results file
# and in the second case the file name is determined interactively through the Open File dialog during the call to 
# Annotation.getIdMap() or Annotation.getDataFrame() on the AnnotationEnsemblCsv object.}
# \item{...}{Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \examples{
# \dontrun{
#  Annotation$init();
#  #create Ensembl annotation object
#  annObj<-AnnotationEnsemblCsv(cacheFolderName="EnsemblCsv");
# }}
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("AnnotationEnsemblCsv",function(cacheFolderName="EnsemblCsv",
		primaryColumn=c("UniProt.SwissProt.Accession","UniProt.TrEMBL.Accession"),
		secondaryColumn=NA,swap=FALSE,full.merge=TRUE,df_filename=NULL,...){

	extend(AnnotationEnsembl(cacheFolderName,primaryColumn,secondaryColumn,swap,full.merge=full.merge,...),
			"AnnotationEnsemblCsv",
			.df_filename=df_filename
	)
})



###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the Ensembl online query system"
#
# \description{
#  See \code{\link[IdMappingRetrieval:getColumns.Annotation]{getColumns}} in \code{\link{Annotation}}.
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("getColumns","AnnotationEnsemblCsv",function(this,df,arrayType,verbose=FALSE,...){

	arrayType=as.character(arrayType);

	primaryColumn<-this$.primaryColumn;

	if(is.na(this$.secondaryColumn)){
		secondaryColumn<-gsub("-",".",arrayType,fixed=TRUE);
		secondaryColumn<-gsub("_",".",secondaryColumn,fixed=TRUE);
		secondaryColumn<-paste("Affy.",toupper(secondaryColumn),sep="");
	} else {
		secondaryColumn<-this$.secondaryColumn;
	}

	if(this$.full.merge){
		idMap<-getColumns2.AnnotationEnsembl(Object(),df,primaryColumn,secondaryColumn,verbose=verbose);
	} else {
		idMap<-getColumns1.AnnotationEnsembl(Object(),df,primaryColumn,secondaryColumn,verbose=verbose);
	}
	return(idMap);
},protected=TRUE)




###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame from the Ensembl csv query results file"
#
# \description{
#  See \code{\link[IdMappingRetrieval:readDF.Annotation]{readDF}} in \code{\link{Annotation}}.
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("readDF","AnnotationEnsemblCsv",function(this,arrayType,verbose=FALSE,...){
	filename<-this$.df_filename;

	if(is.null(filename) || !file.exists(filename)){
		filename<-rchoose.files(default=getwd(),
		caption=paste("Select input file for",this$.cacheFolderName,"service"),
		multi=FALSE,filters = c("CSV files (*.txt,*.csv,*.tsv)","*.txt;*.csv;*.tsv"));
	}

	if(length(filename)==0)
		return(NULL);

	df<-read.csv(filename,stringsAsFactors=FALSE);
	return(df);
},protected=TRUE)


