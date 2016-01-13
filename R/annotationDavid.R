###########################################################################/**
# @RdocClass AnnotationDavid
#
# @title "The AnnotationDavid class"
#
# \description{
#  @classhierarchy
#
# The AnnotationDavid class encapsulates the functionality allowing to retrieve data from the
# DAVID online query system through the getIdMap() and getDataFrame()
# calls on this AnnotationDavid object.
# }
#
#
#
# \arguments{
# \item{cacheFolderName}{ The symbolic name of a service represented by a given AnnotationDavid object.}
# \item{primaryColumn}{ Primary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'AFFYMETRIX_3PRIME_IVT_ID'.}
# \item{secondaryColumn}{ Secondary column to be retrieved from a data frame obtained from the Affymetrix 
# annotation repository when getIdMap() is called. Default is 'UNIPROT_ACCESSION'.}
# \item{swap}{ A @logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
# retrieval during the getIdMap() call.Default is @TRUE.}
# \item{...}{Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \examples{
# \dontrun{
#  Annotation$init();
#  #create DAVID annotation object
#  annObj<-AnnotationDavid"DAVID",species="Homo sapiens");
# }}
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("AnnotationDavid",function(cacheFolderName="DAVID",primaryColumn="AFFYMETRIX_3PRIME_IVT_ID",secondaryColumn="UNIPROT_ACCESSION",
						swap=TRUE,...){
	extend(Annotation(cacheFolderName,primaryColumn,secondaryColumn,swap,...),"AnnotationDavid");
})


###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the DAVID online query system"
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


setMethodS3("getColumns","AnnotationDavid",function(this,df,arrayType,verbose=FALSE,...){

	if(!is.na(this$.species))
		df<-df[df[,"Species"] %in% this$.species,];

	df<-df[,c("From","To")];
	df[,1]<-tolower(df[,1]);

	idMap<-as.IdMap(df,verbose=verbose);
	rownames(idMap)<-idMap[,1];	
	return(idMap);
},protected=TRUE)


###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame from the DAVID online query system"
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

setMethodS3("readDF","AnnotationDavid",function(this,arrayType,verbose=FALSE,...){

	IDs<-AnnotationAffx$getProbesetList(arrayType,verbose);
	df<-convertIDList(IDs,fromType=this$.primaryColumn,toType=this$.secondaryColumn,
		graphicMenu=TRUE,verbose=verbose);


	return(df);
},protected=TRUE)

