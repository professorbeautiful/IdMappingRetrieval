

###########################################################################/**
# @RdocClass Annotation
# \encoding{latin1}
#
# @title "The Annotation class"
#
# \description{
#  @classhierarchy
#
# This is the base annotation class from which the concrete classes
# like AnnotationAffx, AnnotationEnvision etc. are derived
# }
# 
#
# \arguments{
#   \item{cacheFolderName}{ The path to a service cashing directory for a given Annotation object.
#     The path is relative to the caching subsystem root directory. Default is 'Affymetrix'}
#   \item{primaryColumn}{Primary column to be retrieved from a data frame obtained for a given service
#     when getIdMap() on a given annotation object is called. Default is 'From'.}
#   \item{secondaryColumn}{Secondary column(s) to be retrieved from a data frame obtained for a given service
#     when getIdMap on a given annotation object is called. Default is 'To'.}
#   \item{swap}{A @logical indicating if primary and secondary column(s) need to be swapped at the end of the IdMap
#     retrieval during the getIdMap() call.Default is @FALSE.}
#   \item{species}{A @character vector or @NA indicating if filtering of the results on a particular set of species
#     should be performed if a given service provides the species information. 
#     If @NA, no filtering is performed. Default is "Homo sapiens".}
#   \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
#   \item{...}{Additional parameters}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setConstructorS3("Annotation",function(cacheFolderName="",primaryColumn="From",secondaryColumn="To",swap=FALSE,species="Homo sapiens",verbose=FALSE, ...){
	extend(Object(),"Annotation",
		.cacheFolderName=cacheFolderName,
		.species=species,
		.primaryColumn=primaryColumn,
		.secondaryColumn=secondaryColumn,
		.swap=swap
	)
},abstract=TRUE)


###########################################################################/**
# @RdocMethod init
# 
# @title "Init annotation subsystem"
#
# \description{
#  @get "title". This function should be called before the annotation functionality could be used.
#  Creates the annotation file subsystem root directory if not present and initializes
#  the internal reference to the root directory.
# }
#
#
# \arguments{
# \item{directory}{The root directory of the annotation cashing subsystem. Default is './annotationData'.}
# \item{verbose}{if @TRUE (default) enables diagnostic messages.}
# \item{...}{additional parameters}
# }
#
# \value{
#  Returns (invisibly) the annotation root directory.
# }
#
# \seealso{@seeclass}
#
# \examples{\dontrun{Annotation$init();}}
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################
 
setMethodS3("init","Annotation",function(static,directory="./annotationData",verbose=TRUE,...){

	if (!file.exists(directory)) {
		if (verbose)
			cat("Creating annotation archive directory: ",directory,"...\n");
		dir.create(directory,recursive=FALSE);
	}

	if("IdMappingRetrieval" %in% .packages()){
		assignInNamespace("IdMappingAnnotationData.root",directory,"IdMappingRetrieval");
	}

	invisible(directory);
},static=TRUE)

###########################################################################/**
# @RdocMethod getRoot
#
# @title "Get the path to service caching directory"
# \description{@get "title".}
#
#
# \arguments{
# \item{cacheFolderName}{The path to service caching directory. If missing,
#  returns the path to caching root directory.}
# }
#
# \value{A @character string representing the path to the annotation service directory.}
#
# \examples{
# \dontrun{
#   Annotation$init(directory="./annotationData");
#   Annotation$getRoot()
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setMethodS3("getRoot","Annotation",function(static,cacheFolderName,...) {
	if(missing(cacheFolderName)){
		return(IdMappingAnnotationData.root);
	} else {
		return (paste(IdMappingAnnotationData.root,cacheFolderName,sep="/"));
	}
},static=TRUE)



###########################################################################/**
# @RdocMethod setCredentials
# 
# @title "Create and store the credentials information for given service"
#
# \description{@get "title",
# Some online query services require user to register
# on their web sites providing user ID, password etc. in order to access the data, 
# Affymetrix NetAffx being an example of such service. The Annotation.setCredentials functions
# allows to store the registration information in a file within the annotation cashing subsystem
# reusing this information during the subsiquent calls to the given query system.
# }
#
#
# \arguments{
# \item{cacheFolderName}{Caharacter string representing service name.}
# \item{verbose}{ if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional parameters (user ID, password etc.)}
# }
#
# \examples{\dontrun{
# Annotation$init();
# #set credentials for 'Affymetrix' service
# Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438");
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setMethodS3("setCredentials","Annotation",function(static,cacheFolderName,verbose=FALSE,...){

	credentials<-list(...);

	if (length(credentials)>0){
		directory<-paste(Annotation$getRoot(),cacheFolderName,sep="/");

		if (!file.exists(directory)) {
			if (verbose)
				cat("Creating service archive directory: ",directory,"...\n");
			dir.create(directory);
		}

		credentialsFileName<-paste(directory,"credentials",sep="/");
		save(credentials,file=credentialsFileName);
	}
},static=TRUE)



###########################################################################/**
# @RdocMethod getCredentials
# 
# @title "Retrieve credentials for a given service" 
#
# \description{@get "title",
# Retrieves an object containing the credentials info (user ID, password etc.)
# allowing to access the given service programmatically
# }
#
#
# \arguments{
# \item{serviceName}{@character string representing service name.}
# }
#
# \value{List containing credential info}
#
# \examples{
# \dontrun{
# Annotation$init();
# #set credentials for 'Affymetrix' service
# Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438");
# #get credentials
# Annotation$getCredentials("Affymetrix");
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/###########################################################################

setMethodS3("getCredentials","Annotation",function(static,serviceName,...){
	directory<-paste(Annotation$getRoot(),serviceName,sep="/");
	credentialsFileName<-paste(directory,"credentials",sep="/");
	if(file.exists(credentialsFileName)){
		name<-load(credentialsFileName,envir=environment());
		return(get(name,envir=environment()));
	} else {
		return(NULL);
	}
},static=TRUE)

###########################################################################/**
# @RdocMethod getArrayTypes
#
# @title "Get all available microarray chip types"
#
# \description{@get "title".}
#
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{A @character vector containing the names of all available microarrays.}
#
# \examples{
# \dontrun{
#  Annotation$init();
#  #get Affymetrix microarray array types
#  arrayTypes<-Annotation$getArrayTypes();
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("getArrayTypes","Annotation",function(static,...){
	credentials<-Annotation$getCredentials("Affymetrix");

	if (is.null(credentials)){
		warning("getArrayTypes.Annotation: credentials file not found!");
		return(NULL);
	}

	# creating NetAffx object 
	rsrc<-NULL;     
	rsrc<-NetAffxResource(user = credentials$user, 
					password = credentials$password,
					directory=Annotation$getRoot("Affymetrix"));
	if (is.null(rsrc)) {
		return(NULL);
	}
	arrayTypes<-names(rsrc);
	attr(arrayTypes,"rsrc")<-rsrc;
	return(arrayTypes);
},static=TRUE)

###########################################################################/**
# @RdocMethod getArrayType
#
# @title "Select the array type"
#
# \description{@get "title".}
#
#
# \arguments{
#  \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types
#   allowing user to select a chip. If arrayType is an array name, the function performs the name correctness check.
#   Default is 'menu'.}
#  \item{graphics}{ If @TRUE and the arrayType is 'menu', uses the graphics capabilities to display the available choices.
#   Default is getOption('menu.graphics').}
# }
#
# \value{The Affymetrix chip name. If arrayType='menu' and user cancels the selection or if arrayType is an incorrect
#   array name, returns @NULL, dysplaying the warning in the latter case.}
#
# \examples{
# \dontrun{
#   Annotation$init();
#   #interctively select an array type
#   arrayType<-Annotation$getArrayType("menu");
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("getArrayType","Annotation",function(static,arrayType="menu",graphics=getOption("menu.graphics"),...){

	
	arrayTypes<-Annotation$getArrayTypes();

	if(arrayType=="menu"){
		arrayType<-rselect.list(choices=arrayTypes,title = "Select Array Type",graphics=graphics);
		if(length(arrayType)==0)
			return(NULL);
	}
	if (!(arrayType %in% arrayTypes)){
		warning("Invalid array type");
		return(NULL)
	}

	attr(arrayType,"rsrc")<-attr(arrayTypes,"rsrc");
	return(arrayType);
},static=TRUE)


###########################################################################/**
# @RdocMethod getFolderName
#
# @title "Get caching folder name for a given Annotation object"
#
# \description{
#  @get "title". To retrieve a full path for a given service caching folder,
#  see \code{\link[IdMappingRetrieval:getServiceRoot.Annotation]{getServiceRoot}}.
# }
#
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{A @character string containing caching folder name.}
#
# \seealso{@seeclass}
#
# \author{
#  Alex Lisovich, Roger Day
# }
#*/##########################################################################

setMethodS3("getFolderName","Annotation",function(this,...){
	return(this$.cacheFolderName);
})



###########################################################################/**
# @RdocMethod getServiceRoot
# @title "Get a root directory for a particular annotation object"
#
# \description{
#  Retrieves a root directory for a particular service within the caching subsytem
#  file structure. If the static version of call used (no service object specified), returns the path to the
#  caching root directory.
# }
#
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{A @character string containing the path to the service caching directory.}
#
# \examples{
# \dontrun{
#   #get the path to service object caching folder
#   annObj<-AnnotationDavid("DAVID",species="Homo sapiens");
#   getServiceRoot(annObj);
#   #get the path to caching system root directory
#   Annotation$getServiceRoot();
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("getServiceRoot","Annotation",function(this,...){
	return (paste(Annotation$getRoot(),this$.cacheFolderName,sep="/"));
})


###########################################################################/**
# @RdocMethod setOptions
# @title "Set the parameters for an annotation object"
#
# \description{@get "title".}
#
#
# \arguments{
# \item{...}{Parameters to be set for a given annotation object.}
# }
#
# \examples{
# \dontrun{
#    #create Ensembl annotation object
#    annObj<-AnnotationEnsemblCsv("EnsemblCsv");
#    #set the name of the file the annotation data to be retrieved from
#    setOptions(annObj,df_filename="ENSEMBL_biomart_export.txt");
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("setOptions","Annotation",function(this,...){
	options<-list(...);
	for(name in names(options))
		annotationObj[[name]]<-options[[name]];
	invisible(NULL);
})

###########################################################################/**
# @RdocMethod readDF
#
# @title "Retrieve the annotation data frame from the service data repository"
#
# \description{
# This is an internal function called from within Annotation.getDataFrame() on a particular
# annotation object. The algorithm of the data frame extraction is annotation object type specific
# and reflects the format and content of the data returned by a particular query system.
# }
#
#
# \arguments{
# \item{arrayType}{ The microarray chip name.}
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{ Additional arguments.}
# }
#
# \value{@data.frame containing annotation data for a particular microarray.}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("readDF","Annotation",function(this,arrayType,verbose=FALSE,...){
	throw("readDF.Annotation: abstract function call");
},abstract=TRUE,protected=TRUE)



###########################################################################/**
# @RdocMethod getColumns
#
# @title "Extract ID match pairs from the data frame obtained from the annotation data repository"
#
# \description{
# This is an internal method called from within getIdMap() on
# Annotation object. The algorithm of the ID pairs extraction is annotation object type specific
# and reflects the format and content of the data frame retrieved by a prticular Annotation object.
# }
#
#
# \arguments{
#  \item{df}{The @data.frame from which ID pairs to be retrieved.}
#  \item{arrayType}{The microarray chip name.}
#  \item{verbose}{if @TRUE enables diagnostic messages.}
#  \item{...}{Not used.}
# }
#
# \value{ID match pairs}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#
#*/##########################################################################

setMethodS3("getColumns","Annotation",function(this,df,arrayType,verbose=FALSE,...){
	throw("getColumns.Annotation: abstract function call");
},abstract=TRUE,protected=TRUE)



###########################################################################/**
# @RdocMethod getDataFrame
#
# @title "Get the entire data set available from a particular service in a form of a data frame"
#
# \description{
# Get the entire data set available from a particular service in a form of a data frame caching the 
# data in a service directory allowing the fast retrieval of data next time the function is called.
# }
#
#
# \arguments{
#  \item{arrayType}{ Character string representing the array name. If 'menu', the list of available arrays is displayed
#  allowing to select the array type of interest.}
#  \item{force}{ If @TRUE forces the system to update the content of service data directory 
#  even if contained the data before. Default is @FALSE.}
#  \item{graphics}{ If @TRUE (default), the attempt to will be made to use graphical capabilities of the rChoiceDialogs package to
#  display selection choices (items, files etc.) in a graphical form.}
#  \item{verbose}{ if @TRUE enables diagnostic messages. Default is @FALSE.}
#  \item{...}{Not used.}
# }
#
# \value{A @data.frame containing entire data set available from a particular service.}
#
# \examples{
# \dontrun{
# Annotation$init();
# Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438",verbose);
# #create Affymetrix annotation object
# annObj=AnnotationAffx("Affymetrix");
# #retrieve the raw annotation data frame
# df<-getDataFrame(annObj,verbose=TRUE);
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
# 
#*/##########################################################################

setMethodS3("getDataFrame","Annotation",function(this,arrayType="menu",force=FALSE,
			graphics=TRUE, verbose=FALSE,...){

	if(is.null(arrayType))
		return(NULL);

	directory<-getServiceRoot(this);

	arrayType<-Annotation$getArrayType(arrayType);
	serviceName=getFolderName(this);
	
	#create CSV annotation .RData file if not present yet
	rDataPath<-paste(directory,"/",arrayType,sep="");
	rDataFile<-paste(rDataPath,"/",serviceName,".RData",sep="");

	if (!file.exists(rDataFile) || force) {
		if (verbose)
			cat("Creating directory for array specific RData...\n");
		if(!file.exists(rDataPath))
			dir.create(rDataPath,recursive=TRUE);

		serviceDF=readDF(this,arrayType,verbose);

		if(is.null(serviceDF)){
			warning(paste("unable to retrieve data for service",serviceName),immediate.=TRUE);
			return(NULL);
		}

		if (verbose)
			cat("Saving annotation data...\n");	
		save(serviceDF,file=rDataFile);
	} else {
		if (verbose)
			cat("Loading annotation data...\n");	
		load(rDataFile);
	}
	
	gc();
	return(serviceDF);
})



###########################################################################/**
# @RdocMethod getIdMap
#
# @title "Get an IdMap object using the data retrieved by a particular service represented by annotation object"
# \description{@get "title".}
#
#
# \arguments{
# \item{arrayType}{ A @character string representing the array name. If 'menu', the list of available arrays is displayed
# allowing to select the array type of interest.}
# \item{primaryKey}{ The name which will be assigned to the IdMap object primary column
# at the end of a data retrieval process.}
# \item{secondaryKey}{ The name which will be assigned to the IdMap object secondary column
# at the end of a data retrieval process.}
# \item{force}{ If @TRUE forces the system to update the content of service data directory even if contained the data before.
# Used to update the annotation data if necessary. Default is @FALSE.}
# \item{graphics}{ If @TRUE (default), the attempt to will be made to use graphical capabilities 
# of the rChoiceDialogs package to display selection choices (items, files etc.) in a graphical form.}
# \item{verbose}{ if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional parameters.}
# }
#
# \value{An ID Map object} 
#
# \examples{
# \dontrun{
#   Annotation$init();
#   Annotation$setCredentials("Affymetrix",user="alex.lisovich@gmail.com",password="125438",verbose);
#   #create Affymetrix annotation object
#   annObj=AnnotationAffx();
#   #retrieve the ID Map
#   idMap<-getIdMap(annObj,arrayType="HG-U133_Plus_2",primaryKey="Affy",secondaryKey="Uniprot",verbose=TRUE);
# }}
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/##########################################################################

setMethodS3("getIdMap","Annotation",function(this,arrayType="menu",primaryKey=NULL,secondaryKey=NULL,
						force=FALSE,graphics=TRUE, verbose=FALSE,...){

	swapVals<-function(doswap,A,B){
		if(doswap)
			return(B)
		else
			return(A);
	}

	getKey<-function(key,inName,resName,default){
		if(is.character(key))
			return(key)
		else{
			if(is.character(inName))
				return(paste(inName,collapse="+"))
			else if(is.character(resName))
				return(resName)
			else
				return(default);
		}
	}


	swap=this$.swap;



	if(is.null(secondaryKey))
		secondaryKey<-this$.secondaryColumn;


	arrayType<-Annotation$getArrayType(arrayType,graphics=graphics);

	directory<-getServiceRoot(this);

	if (!file.exists(directory)) {
		if (verbose)
			cat("Creating service archive directory: ",directory,"...\n");
		dir.create(directory);
		force=TRUE;
	}

	rDataPath<-paste(directory,arrayType,sep="/");
	if(this$.swap){
		rDataFile<-paste(rDataPath,"/",this$.secondaryColumn[1],"_",this$.primaryColumn[1],".idMap.RData",sep="");
	} else {
		rDataFile<-paste(rDataPath,"/",this$.primaryColumn[1],"_",this$.secondaryColumn[1],".idMap.RData",sep="");
	}


	# check if annotation index .RData object of type ID already exits
	if (force || !file.exists(rDataFile)){
		#retrieving master annotation table (includes the creation of subdirectory)
		if(verbose)
			cat("retrieving master annotation table...\n");

		df<-getDataFrame(this,arrayType,force=force,verbose=verbose);
		if(is.null(df))
			return(NULL);

		idMap<-getColumns(this,df,arrayType,verbose=verbose);
		if(is.null(idMap)){
			warning("unable to retrieve an ID Map");
			return(NULL);
		}


		colnames(idMap)[1]<-getKey(swapVals(swap,primaryKey,secondaryKey),this$.primaryColumn,colnames(idMap)[1],NULL);
		colnames(idMap)[2]<-getKey(swapVals(swap,secondaryKey,primaryKey),this$.secondaryColumn,colnames(idMap)[2],NULL);
		if(swap)
			idMap<-swapKeys(idMap,verbose=verbose);
		
		if (verbose)
			cat("Saving ID map\n");
		save(idMap,file=rDataFile);
	} else {
		if(verbose)
			cat("Loading ID map...\n");
		load(rDataFile);
	}

	return(idMap);
})

