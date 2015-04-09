
###########################################################################/**
# @RdocClass AnnotationAffx
#
# @title "The AnnotationAffx class"
#
# \description{
#  @classhierarchy
#
# The AnnotationAffx class encapsulates the functionality allowing to retrieve data from the
# Affymetrix annotation data online repository through the getIdMap() and getDataFrame() calls.
# }
#
#
# \arguments{
# \item{cacheFolderName}{ The path to a service cashing directory for a given AnnotationAffx object.
#  The path is relative to the caching subsystem root directory. Default is 'Affymetrix'}
# \item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'Probe.Set.ID'.}
# \item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'SwissProt'.}
# \item{swap}{ Logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
# retrieval during the getIdMap() call.Default is @TRUE.}
# \item{...}{ Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setConstructorS3("AnnotationAffx",function(cacheFolderName="Affymetrix",primaryColumn="Probe.Set.ID",secondaryColumn="SwissProt",
						swap=TRUE,...){

	extend(Annotation(cacheFolderName,primaryColumn,secondaryColumn,swap,...),"AnnotationAffx");
})

###########################################################################/**
# @RdocMethod setCredentials
# 
# @title "Create and store the credentials information for Affymetrix service"
#
# \description{@get "title".}
#
#
# \arguments{
# \item{verbose}{ if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional parameters (user ID and password)}
# }
#
# \examples{\dontrun{
# Annotation$init();
# #set credentials for 'Affymetrix' service
# AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438");
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setMethodS3("setCredentials","AnnotationAffx",function(static,verbose=FALSE,...){
	Annotation$setCredentials("Affymetrix",verbose,...);
},static=TRUE)




###########################################################################/**
# @RdocMethod getProbesetList
# @title "Get Affymetrix probeset ID list for a given array type"
#
# \description{@get "title".}
#
#
# \arguments{
# \item{arrayType}{ The Affymertix microarray name. If 'menu' (default), display a set of all available array types,
# allowing user to select the array of interest.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
#
# \value{A @character vector representing the list of Affymetrix probeset IDs for a given array type.}
#
# \examples{
# \dontrun{
#   Annotation$init();
#   AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
#   #retrieve probeset IDs
#   AnnotationAffx$getProbesetList(arrayType="HG-U133_Plus_2",verbose=TRUE);
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################
 

setMethodS3("getProbesetList","AnnotationAffx",function(static,arrayType="menu",verbose=FALSE,...){
	if(verbose)
		cat("Retrieving Affymetrix Probeset List for",arrayType,"...\n");

	annObj<-AnnotationAffx("Affymetrix",swap=FALSE);

	affxMap<-getIdMap(annObj,arrayType=arrayType,verbose=verbose);	

	IDs<-affxMap[,1];
	return(invisible(IDs));
},static=TRUE)



###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the Affymetrix online data repository"
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

setMethodS3("getColumns","AnnotationAffx",function(this,df,arrayType,verbose=FALSE,...){
	primaryColumn<-this$.primaryColumn;
	secondaryColumn<-this$.secondaryColumn;

	idMap<-df[,c(primaryColumn,secondaryColumn)];
	idMap[,secondaryColumn]<-gsub(" /// ",",",idMap[,secondaryColumn],fixed=TRUE);
	idMap[,secondaryColumn]<-gsub("---","",idMap[,secondaryColumn],fixed=TRUE);
	idMap[,primaryColumn]<-tolower(idMap[,primaryColumn]);
	rownames(idMap)<-idMap[,primaryColumn];
	return(idMap);
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


setMethodS3("readDF","AnnotationAffx",function(this,arrayType,verbose=FALSE,...){

	arrayType<-Annotation$getArrayType(arrayType);
	rsrc<-attr(arrayType,"rsrc");


	# select annotation file for given arrayType in CSV format
	anno <- rsrc[[arrayType, "Annotations, CSV format"]];

	# download annotation file if not present, but do not get a frame, 
	# as AffyCompatible does not read csv correctly (will do it by hand later)
	# df contains the path to the compressed file
	df <- readAnnotation(rsrc, annotation = anno,content=FALSE);
	
	#prepare for uncompressing: extract the file name within the .zip file
      splitPath<-strsplit(df,arrayType);
	suffixPath<-strsplit(splitPath[[1]][2],".zip");
	fileName<-paste(arrayType,suffixPath,sep="");
	
	conn<-unz(df,fileName)

	#workaround NetAffx format inconsistency: need to determine the line number N which does not start from "#" 
	#and then read as csv skipping N lines  
	open(conn);
	eofHead<-FALSE;
	skipLines<-0
	while(!eofHead) {
		csvLine<-readLines(conn,n=1);
		if (substr(csvLine,1,1)!="#"){	
			eofHead<-TRUE;
			close(conn);
		}else{
			skipLines<-(skipLines+1);
		}
	}
	conn<-unz(df,fileName)
	open(conn);

	# end of workaround

	if (verbose)
		cat("Reading compressed csv file...\n");	
	df<-read.csv(conn,header=TRUE,skip=skipLines,stringsAsFactors=FALSE);
	close(conn);
	return(df);
},protected=TRUE)


