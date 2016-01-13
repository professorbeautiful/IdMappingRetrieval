

###########################################################################/**
# @RdocClass AnnotationEnvision
#
# @title "The AnnotationEnvision class"
#
# \description{
#  @classhierarchy
#
# The AnnotationEnvision class encapsulates the functionality allowing to retrieve data from the
# Envision online query system.The Envision online query system allows to retrieve the 
# ID matching information accompanied by multiple attributes 
# like species and the microarray chip type in the form of the xml file. 
# The AnnotationEnvision class encapsulates the functionality allowing to filter the Envision query results
# on species and microarray type attributes and convert it into a data frame during the
#  getIdMap() and getDataFrame() calls on the AnnotationEnvision object.
# }
#
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
# \item{species}{Character vector or @NA indicating if filtering of the results on a particular set of species
# should be performed if a given service provides the species information. 
# If @NA, no filtering is performed. Default is 'Homo sapiens'.}
# \item{full.merge}{ A @logical indicating which version of primary columns merging algorithm to use. If @TRUE (default), all unique pairs 
# <probeset ID, SwissProt> and <probeset ID, Trembl> are generated, and if @FALSE, only those pairs from <probeset ID, Trembl> 
# for which Uniparot ID is not present in <probeset ID, SwissProt> pairs are included.}
# \item{...}{Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \examples{
# \dontrun{
#  Annotation$init();
#  #create Envision annotation object
#  annObj<-AnnotationEnvision(species="Homo sapiens");
# }}
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("AnnotationEnvision",function(cacheFolderName="EnVision",
		primaryColumn=c("UniProt.SwissProt.Accession","UniProt.TrEMBL.Accession"),
		secondaryColumn=NA,swap=TRUE,species="Homo sapiens",full.merge=TRUE,...){

	extend(Annotation(cacheFolderName,primaryColumn,secondaryColumn,swap,species,...),"AnnotationEnvision",
		.full.merge=full.merge
	)
})


###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the Envision online query system"
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

setMethodS3("getColumns","AnnotationEnvision",function(this,df,arrayType,verbose=FALSE,...){

	pairs<-unique(df[,1:2]);
	idMap<-as.IdMap(pairs,verbose=verbose);
	rownames(idMap)<-idMap[,1];	
	return(idMap);
},protected=TRUE)



###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame using Envision online query system"
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

setMethodS3("readDF","AnnotationEnvision",function(this,arrayType,verbose=FALSE,...){
	#if(!require(ENVISIONQuery))
	#	return(NULL);

	primaryColumn<-this$.primaryColumn;
	secondaryColumn<-this$.secondaryColumn;

	IDs<-AnnotationAffx$getProbesetList(arrayType=arrayType,verbose=verbose);

	filter<-list(organism.species=this$.species);
	
	df<-ENVISIONQuery(IDs,typeName="Affymetrix ID",serviceName="ID Conversion",toolName="Affy2Uniprot",
					formatIt=TRUE,filter=filter,compact=TRUE,
					chunk=1000,writeHTML=FALSE,verbose=verbose);
	return(df);
},protected=TRUE)


