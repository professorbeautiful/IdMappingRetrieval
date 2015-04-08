
###########################################################################/**
# @RdocClass AnnotationNetAffx
#
# @title "The AnnotationNetAffx class"
#
# \description{
#  @classhierarchy
#
# The AnnotationNetAffx class encapsulates the functionality allowing to retrieve data from the
# NetAffx batch query system through the Annotation.getIdMap() and Annotation.getDataFrame()
# calls on this object. The NetAffx batch query system requires to submit the probeset IDs by providing 
# text files in a special format, maximum 10000 IDs per file. The results are returned in a form of a tab 
# delimited text file, one file per submission, so the query results for a whole array are presented by a set
# of such files. The AnnotationNetAffx object encapsulates the functionality allowing to interactively choose
# the set of result files and merge them into a single data frame during the
# Annotation.getIdMap() and Annotation.getDataFrame() calls on the AnnotationNetAffx object.
# }
#
#
# \arguments{
# \item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationNetAffx object.}
# \item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'Probe.Set.ID'.}
# \item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'SwissProt'.}
# \item{swap}{A @logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
# retrieval during the getIdMap() call.Default is @TRUE.}
# \item{df_filename}{ Character vector, character string or @NULL. In the first case the character vector contains the names
# of a resulting file set, in the second, the character string contains the name of directory in which the files are stored,
# and in the third the file set is determined interactively through the Open File dialog during the call to 
# getIdMap() or getDataFrame() on the AnnotationAffxCsv object.}
# \item{...}{Additiional parameters, see \code{\link{Annotation}}.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setConstructorS3("AnnotationNetAffx",function(cacheFolderName="NetAffxCsv",primaryColumn="Probe.Set.ID",secondaryColumn="SwissProt",
						swap=TRUE,df_filename=NULL,...){
	extend(AnnotationAffx(cacheFolderName,primaryColumn,secondaryColumn,swap,...),"AnnotationNetAffx",
		.df_filename=df_filename
	)
})

###########################################################################/**
# @RdocMethod createSubmission
#
# @title "Create a set of files for NetAffx batch query"
#
# @synopsis
#
# \description{
# This is a convinience function generating a set of files suitable
# for a submission to the Affymetrix NetAffx batch service from the list of probe set
# IDs obtained inside the this function through the call to AnnotationAffx.getProbesetList(),
# allowing user to choose the array type interactively.
# }
#
# \arguments{
# \item{chunk}{ maximum number of probeset IDs per submission file to comply with
# submission size restrictions of the NetAffx batch query system (10000). Defaul is 9999.}
# \item{folder}{ Name of the folder where generated files will be saved. Default is './NetAffx_submission'.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used}
# }
#
# \examples{\dontrun{AnnotationNetAffx$createSubmission();}}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################

setMethodS3("createSubmission","AnnotationNetAffx",function(static,chunk=9999,folder="./NetAffx_submission",verbose=TRUE,...){
	dir.create(folder,showWarnings = FALSE);

	idList<-AnnotationAffx$getProbesetList(verbose=verbose);
	len<-length(idList);

	start=1;
	ind=1;
	while(start<=len){
		end<-min(len,start+chunk-1);
		ids<-idList[start:end];
		filename<-paste("NetAffx_",ind,".txt",sep="");
		path<-paste(folder,filename,sep="/");
		if(verbose)
			cat("Writing chunk",ind,":",filename,"\n");
		write.table(ids,file=path,row.names=FALSE,col.names=FALSE,quote=FALSE);
		start<-end+1;
		ind<-ind+1;
	}
	invisible(NULL);
},static=TRUE)

###########################################################################/**
# @RdocMethod filesExist
#
# @title "Check if the AnnotationNetAffx contains the reference to an existing data file or directory"
# \description{@get "title".}
#
# @synopsis
# 
# \arguments{
# \item{...}{Not used}
# }
#
# \value{@TRUE if file(s) or directory exists, FALSE otherwise.}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################

setMethodS3("filesExist","AnnotationNetAffx",function(this,...){
	if(length(this$.df_filename)==0 ){
		return(FALSE)
	} else {
		for (fname in this$.df_filename)
			if(!file.exists(fname))
				return(FALSE);
	}
	return(TRUE);
},protected=TRUE)



###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame from the service data repository"
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

setMethodS3("readDF","AnnotationNetAffx",function(this,arrayType,verbose=FALSE,...){

	filenames<-this$.df_filename;
	
	if(!filesExist(this)) {
		filenames=rchoose.files(default=getwd(),
		caption ="Select NetAffx batch result files",
		multi=TRUE,filters=c("CSV files (*.txt,*.csv,*.tsv)","*.txt;*.csv;*.tsv"));
	}

	if(length(filenames)==0)
		return(NULL);

	
	if(length(filenames)==1 && file.info(filenames)$isdir)
		filenames<-dir(filenames,full.names=TRUE);

	df<-NULL;

	for (file in filenames) {
		if(!file.info(file)$isdir){
			if (verbose)
				cat("Adding file", file,"\n");
		
			chunk<-read.csv(file,sep="\t",stringsAsFactors=FALSE);
			df<-rbind(df,chunk);
			gc();
		}
	}

	return(df);
},protected=TRUE)




