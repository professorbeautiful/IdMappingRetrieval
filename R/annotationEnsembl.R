

###########################################################################/**
# @RdocClass AnnotationEnsembl 
#
# @title "The AnnotationEnsembl class"
#
# \description{
#  @classhierarchy
#
# The AnnotationEnsembl class encapsulates the functionality allowing to retrieve data from the
# Ensembl BioMart online query system using biomaRt R package through the Annotation.getIdMap() and Annotation.getDataFrame()
# calls on this object.
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
# If @NA, no filtering is performed. Default is 'hsapiens_gene_ensembl'.}
# \item{full.merge}{ A @logical indicating which version of primary columns merging algorithm to use. If@ TRUE (default), all unique pairs 
# <probeset ID, SwissProt> and <probeset ID, Trembl> are generated, and if @FALSE, only those pairs from <probeset ID, Trembl> 
# for which Uniprot ID is not present in <probeset ID, SwissProt> pairs are included.}
# \item{...}{Additional parameters, see \code{\link{Annotation}}.}
# }
#
# \examples{
# \dontrun{
#  Annotation$init();
#  #create Ensembl annotation object
#  annObj<-AnnotationEnsembl(species="hsapiens_gene_ensembl");
# }}
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("AnnotationEnsembl",function(cacheFolderName="Ensembl",
		primaryColumn=c("uniprot_swissprot_accession","uniprot_sptrembl"),
		secondaryColumn=NA,
		swap=FALSE,species="hsapiens_gene_ensembl",full.merge=TRUE,...){

	extend(Annotation(cacheFolderName,primaryColumn,secondaryColumn,swap,species,...),"AnnotationEnsembl",
		.full.merge=full.merge
	)
})

###########################################################################/**
# @RdocMethod getColumns1
#
# @title "Retrieve the ID match pairs"
#
# \description{
# @get "title"  using the algorithm corresponding to full.merge = FALSE
# flag of the given AnnotationEnsemblCsv object.
# }
#
# @synopsis
#
# \arguments{
# \item{df}{ The data frame from which ID pairs to be retrieved.}
# \item{primaryColumn}{ Primary column name.}
# \item{secondaryColumn}{ Secondary column name.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
#
# \value{return ID match pairs}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################

setMethodS3("getColumns1","AnnotationEnsembl",function(static,df,primaryColumn,secondaryColumn,verbose=FALSE,...){

	pairs<-df[,c(primaryColumn[1],secondaryColumn)];
	for (i in 1:length(primaryColumn)){
		empty_inds<-which(pairs[,1]=="");
		if (i<length(primaryColumn))
			pairs[empty_inds,]<-df[empty_inds,c(primaryColumn[i+1],secondaryColumn)];
	}
	pairs<-pairs[pairs[,1]!="",];
	pairs<-pairs[pairs[,2]!="",];
	pairs<-unique(pairs);

	idMap<-as.IdMap(pairs,verbose=verbose);
	rownames(idMap)<-idMap[,primaryColumn[1]];
	return(idMap);
},static=TRUE,protected=TRUE)


###########################################################################/**
# @RdocMethod getColumns2
#
# @title "Retrieve the ID match pairs"
#
# \description{
# @get "title" using the algorithm corresponding to full.merge = TRUE
# flag of the given AnnotationEnsemblCsv object.
# }
#
# @synopsis
#
# \arguments{
# \item{df}{ The data frame from which ID pairs to be retrieved.}
# \item{primaryColumn}{ Primary column name.}
# \item{secondaryColumn}{ Secondary column name.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
#
# \value{return ID match pairs}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################

setMethodS3("getColumns2","AnnotationEnsembl",function(static,df,primaryColumn,secondaryColumn,verbose=FALSE,...){
	pairs<-NULL;
	for (i in 1:length(primaryColumn)){
		chunk<-df[,c(primaryColumn[i],secondaryColumn)];
		colnames(chunk)<-c(primaryColumn[1],secondaryColumn);
		pairs<-rbind(pairs,chunk);
	}
	pairs<-pairs[pairs[,1]!="",];
	pairs<-pairs[pairs[,2]!="",];

	pairs<-unique(pairs);

	idMap<-as.IdMap(pairs,verbose=verbose);
	rownames(idMap)<-idMap[,primaryColumn[1]];
	return(idMap);
},static=TRUE,protected=TRUE)



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

setMethodS3("getColumns","AnnotationEnsembl",function(this,df,arrayType,verbose=FALSE,...){

	arrayType=as.character(arrayType);

	primaryColumn<-this$.primaryColumn;

	if(is.na(this$.secondaryColumn)){
		secondaryColumn<-gsub("-","_",arrayType,fixed=TRUE);
		secondaryColumn<-paste("affy_",tolower(secondaryColumn),sep="");
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
# @title "Retrieve the annotation data frame from the Ensembl online query system"
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

setMethodS3("readDF","AnnotationEnsembl",function(this,arrayType,verbose=FALSE,...){
	df<-NULL;
	tryCatch({
		ensembl<-useMart("ensembl");
		ensembl = useDataset(this$.species,mart=ensembl)
	
		if(is.na(this$.secondaryColumn)){
			arrayName<-gsub("-","_",arrayType,fixed=TRUE);
			arrayName<-paste("affy_",tolower(arrayName),sep="");
		} else {
			arrayName<-this$.secondaryColumn;
		}

		IDs<-AnnotationAffx$getProbesetList(arrayType,verbose);

		df<-getBM(attributes=c(arrayName,this$.primaryColumn),
			filters=arrayName,values=IDs,mart=ensembl);
	},
	error=function(e){
		cat("\n",as.character(e),"\n");
		flush.console();
	});

	return(df);
},protected=TRUE)


