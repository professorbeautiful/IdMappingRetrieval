###########################################################################/**
# @RdocClass AnnotationDavidCsv
#
# @title "The AnnotationDavidCsv class"
#
# \description{
#  @classhierarchy
#
# The AnnotationDavidCsv class encapsulates the functionality allowing to retrieve data from the
# DAVID backend annotation file system through the Annotation.getIdMap() and Annotation.getDataFrame()
# calls on this object. The DAVID backend annotation file system requires to submit the request through the
# e-mail for convertion of one ID type to another. Within a day, the user recieves an email reply containing the
# link to the resulting conversion file.The results are returned in a form of a tab delimited headerless file containing
# the match pairs, one pair per line. The AnnotationDavidCsv object encapsulates the functionality allowing to interactively choose
# the resulting files and convert it into a data frame during the
# Annotation.getIdMap() and Annotation.getDataFrame() calls on the AnnotationDavidCsv object.
# }
#
# @synopsis
#
# \arguments{
# \item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationDavidCsv object.}
# \item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'AFFY_ID'.}
# \item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'UNIPROT_ACCESSION'.}
# \item{swap}{ Logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
# retrieval during the getIdMap() call.Default is @TRUE.}
# \item{df_filename}{ Character string or @NULL. In the first case the character string contains the name of conversion results file
# and in the second case the file name is determined interactively through the Open File dialog during the call to 
# getIdMap() or getDataFrame() on the AnnotationDavidCsv object.}
# \item{...}{ Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("AnnotationDavidCsv",function(cacheFolderName="DavidCsv",primaryColumn="AFFY_ID",secondaryColumn="UNIPROT_ACCESSION",
						swap=TRUE,df_filename=NULL,...){
	extend(AnnotationDavid(cacheFolderName,primaryColumn,secondaryColumn,swap,...),"AnnotationDavidCsv",
		.df_filename=df_filename
	)
})


###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the DAVID backend conversion results file"
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

setMethodS3("getColumns","AnnotationDavidCsv",function(this,df,arrayType,verbose=FALSE,...){
	idMap<-as.IdMap(df,verbose=verbose);
	rownames(idMap)<-idMap[,1];	
	return(idMap);
},protected=TRUE)



###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame from the DAVID backend conversion results file"
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

setMethodS3("readDF","AnnotationDavidCsv",function(this,arrayType,verbose=FALSE,...){
	filename=this$.df_filename;
	if(is.null(filename) || !file.exists(filename)){
		filename<-rchoose.files(default=getwd(),
			caption=paste("Select input file for",this$.cacheFolderName,"service"),
			multi=FALSE,filters=c("CSV files (*.txt,*.csv,*.tsv)","*.txt;*.csv;*.tsv"));
	}
	
	if(length(filename)==0)
		return(NULL);

	if (verbose)
		cat("Reading DAVID file",filename);

	df<-read.delim(filename,header=FALSE,stringsAsFactors=FALSE);
	df[,1]<-tolower(df[,1]);
	
	IDs<-AnnotationAffx$getProbesetList(arrayType,verbose);

	df<-subsetByColumn(df, IDs,1,verbose=FALSE);

	return(df);
},protected=TRUE)



