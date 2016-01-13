
###########################################################################/**
# @RdocClass ServiceManager
# \encoding{latin1}
#
# @title "The ServiceManager class"
#
# \description{
#  @classhierarchy
#
# ServiceManager class serves as a container for a set of specialized service objects
# and provides the means for (optionally interactive) handling of such a set simplifying
# the process of data retrieval from a variety of resources in a batch mode.
# }
# 
#
#
# \arguments{
# \item{services}{ Annotation services to be contained within the ServiceManager object. 
# Default is @NULL.}
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("ServiceManager",function(services=list(),...){
	extend(Object(),"ServiceManager",
		.services=services
	);
})



###########################################################################/**
# @RdocMethod getServices
#
# @title "Get list of annotation services encapsulated within the particular ServiceManager object"
# \description{@get "title".}
# 
#
# \arguments{\item{...}{Not used.}}
#
# \value{List of \code{\link{Annotation}} derived services.}
#
# \examples{
# svm<-ServiceManager(ServiceManager$getDefaultServices());
# print(names(getServices(svm)));
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getServices","ServiceManager",function(this,...){
	return(this$.services);
})


###########################################################################/**
# @RdocMethod setServices
#
# @title "Set the list of services for a ServiceManager object"
# \description{@get "title".}
# 
#
# \arguments{
# \item{services}{A @list of services to be set within the ServiceManager object.}
# \item{...}{Not used.}
# }
#
# \value{List of \code{\link{Annotation}} derived services.}
#
# \examples{
# #create empty service manager object
# svm<-ServiceManager(ServiceManager$getDefaultServices());
# print(names(getServices(svm)))
#
# #redefine set of services
# setServices(svm,list(
#	NetAffx_F2=AnnotationAffx("Affymetrix_3_20_10"),
#	EnSembl_Q2=AnnotationEnsembl("Ensembl_3_20_10",species="Homo sapiens")
# ));
# print(names(getServices(svm)));
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("setServices","ServiceManager",function(this,services,...){
	this$.services<-list();
	addServices(this,services);
	invisible(getServices(this));
})



###########################################################################/**
# @RdocMethod addServices
#
# @title "Add services to the ServiceManager object"
# \description{@get "title".}
# 
# 
# \arguments{
# \item{services}{A @list of services to be set within the ServiceManager object.}
# \item{...}{Not used.}
# }
#
# \value{ Updated list of \code{\link{Annotation}} - derived  services.}
#
# \examples{
# #create empty service manager object
# svm<-ServiceManager(ServiceManager$getDefaultServices());
# print(names(getServices(svm)))
#
# #add services
# addServices(svm,list(
#	NetAffx_F2=AnnotationAffx("Affymetrix_3_20_10"),
#	EnSembl_Q2=AnnotationEnsembl("Ensembl_3_20_10",species="Homo sapiens")
# ));
# print(names(getServices(svm)));
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("addServices","ServiceManager",function(this,services,...){
	for (i in 1:length(services)){
		name<-names(services)[i];
		if(nchar(name)==0)
			name<-services[[i]]$.cacheFolderName;
		this$.services[[name]]<-services[[i]];	
	}
	invisible(getServices(this));
})



###########################################################################/**
# @RdocMethod getDefaultServices
#
# @title "Get the default services available within the annotation system"
# \description{@get "title".}
# 
#
# \arguments{
# \item{...}{Not used.}
# }
#
# \value{ A @list of \code{\link{Annotation}} - derived service objects containing the following items:
# \item{Affx_Q}{\code{\link{AnnotationAffx}} object retrieving data from the Affymetrix online annotation data repository} 
# \item{Affx_F}{\code{\link{AnnotationNetAffx}} object retrieving the file based data from NetAffx interactive batch query system}
# \item{EnSembl_Q}{\code{\link{AnnotationEnsembl}} object retrieving data from Ensembl/Biomart online query system}
# \item{EnSembl_F}{\code{\link{AnnotationEnsemblCsv}} object retrieving the file based data from Ensembl/Biomart interactive query system}
# \item{EnVision_Q}{\code{\link{AnnotationEnvision}} objectretriving data from the Envision online query system}
# }
#
# \examples{
# services<-ServiceManager$getDefaultServices();
# names(services);
# }
#
# \seealso{@seeclass}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getDefaultServices","ServiceManager",function(static,...){
	services<-list();

	services$Affx_Q<-AnnotationAffx();
	services$Affx_F<-AnnotationNetAffx();
	services$EnSembl_Q<-AnnotationEnsembl();
	services$EnSembl_F<-AnnotationEnsemblCsv();
	services$EnVision_Q<-AnnotationEnvision();

	return(services);

},static=TRUE)



###########################################################################/**
# @RdocMethod getIdMapList
#
# @title "Collect ID mapping data from various online query systems utilizing Annotation service functionality"
# \description{@get "title". If called as static i.e. ServiceManager$getIdMapList(...), a set of default services
# (see \code{\link[IdMappingRetrieval:getDefaultServices.ServiceManager]{getDefaultServices}}) will be used to collect data from.}
# 
#
# \arguments{
# \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types,
# allowing user to select one. If arrayType is an array name, the function performs the name correctness check.
# Default is 'menu'.}
# \item{selection}{ The character vector of service names which data are to be retrieved. If "menu" displays the available
# choices according to the names of services encapsulated within the ServiceManager object. Default is 'menu'.}
# \item{primaryKey}{ The name which will be assigned to each resulting IdMap object primary column
# at the end of a data retrieval process. Default is 'From'.}
# \item{secondaryKey}{ The name which will be assigned to each resulting IdMap object secondary column
# at the end of a data retrieval process. Default is 'To'.}
# \item{force}{ If @TRUE forces the system to update the content of service data directory even if contained the data before.}
# \item{graphics}{ If @TRUE and selection='menu', the attempt to will be made to use graphical capabilities of the rChoiceDialogs
# package to select a subset of services to collect data from.}  
# \item{verbose}{ if @TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
#
# \value{ The list of IdMap objects each of which is a result of data retrieval for a particular service.}
#
# \seealso{@seeclass}
#
# \examples{
# \dontrun{
#   Annotation$init();
#   AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
#   #create service manage object encapsulating default services
#   svm<-ServiceManager(ServiceManager$getDefaultServices());
#   #retrieve the ID Map list interactively selecting array type and services
#   idMapList<-getIdMapList(svm,arrayType="menu",selection="menu",verbose=TRUE);
#   # use a static call to collect data using default services
#   idMapList<-ServiceManager$getIdMapList(arrayType="menu",selection="menu",verbose=TRUE);
# }}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getIdMapList","ServiceManager",function(this,
					arrayType="menu", selection="menu",	primaryKey="From",secondaryKey="To",
					force=FALSE, graphics=TRUE, verbose=FALSE,...){


	services<-getServices(this);
	if(length(services)==0)
		services<-ServiceManager$getDefaultServices();
	serviceNames=names(services);

	arrayType<-Annotation$getArrayType(arrayType=arrayType);
	
	if(selection[1]=="menu"){
		selection<-rselect.list(serviceNames,preselect=serviceNames,multiple=TRUE,title="Select Services");
		if(length(selection)==0)
			return(NULL);
	}  else if (selection[1]=="all"){
		selection<-serviceNames;
	}

	selection<-selection[selection %in% serviceNames];
	if(length(selection)==0)
		return(NULL);

	idMapList<-list();
	for (name in selection){
		if(verbose)
			cat("\n\nretrieving data for service ***",getFolderName(services[[name]]),"*** :\n");
		idMapList[[name]]<-getIdMap(services[[name]],arrayType=arrayType,	
			primaryKey=primaryKey,secondaryKey=secondaryKey,force=force,verbose=verbose);

	}
	return(idMapList);
})



###########################################################################/**
# @RdocMethod getDataFrameList
#
# @title "Collect raw data data from various online query systems utilizing Annotation service functionality"
# \description{@get "title".If called as static i.e. ServiceManager$getIdMapList(...), a set of default services
# (see \code{\link[IdMappingRetrieval:getDefaultServices.ServiceManager]{getDefaultServices}})will be used to collect data from. }
# 
#
# \arguments{
# \item{arrayType}{ The Affymertix microarray name. If 'menu', display a set of all available array types
# allowing user to select one. If arrayType is an array name, the function performs the name correctness check.
# Default is 'menu'.}
# \item{selection}{ The character vector of service names which data are to be retrieved. If "menu" displays the available
# choices according to the names of services encapsulated within the ServiceManager object. Default is 'menu'.}
# \item{force}{ If TRUE forces the system to update the content of service data directory even if contained the data before.}
# \item{graphics}{ If @TRUE and selection='menu', the attempt to will be made to use graphical capabilities of the rChoiceDialogs
# package to select a subset of services to collect data from.}  
# \item{verbose}{ if TRUE enables diagnostic messages.}
# \item{...}{Not used.}
# }
#
# \value{A @list of data frames each of which is a result of data retrieval for a particular service.}
#
# \seealso{@seeclass}
#
# \examples{
# \dontrun{
#   Annotation$init();
#   AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);
#   #create service manage object encapsulating default services
#   svm<-ServiceManager(ServiceManager$getDefaultServices());
#   #retrieve the data frame list interactively selecting array type and services
#   dfList<-getDataFrameList(svm,arrayType="menu",selection="menu",verbose=TRUE);
#   # use a static call to collect data using default services
#   idMapList<-ServiceManager$getDataFrameList(arrayType="menu",selection="menu",verbose=TRUE);
# }}
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getDataFrameList","ServiceManager",function(this,
		arrayType="menu",selection="menu",force=FALSE, graphics=TRUE,verbose=FALSE,...){


	services<-getServices(this);
	if(length(services)==0)
		services<-ServiceManager$getDefaultServices();
	serviceNames=names(services);

	arrayType<-Annotation$getArrayType(arrayType=arrayType);

	if(selection[1]=="menu"){
		selection<-rselect.list(serviceNames,preselect=serviceNames,multiple=TRUE,title="Select Services");
		if(length(selection)==0)
			return(NULL);
	} else if (selection[1]=="all"){
		selection<-serviceNames;
	}

	selection<-selection[selection %in% serviceNames];
	if(length(selection)==0)
		return(NULL);

	dfList<-list();
	for (name in selection){
		if(verbose)
			cat("\n\nretrieving data for service ***",getFolderName(services[[name]]),"*** :\n");
		dfList[[name]]<-getDataFrame(services[[name]],arrayType=arrayType,force=force,verbose=verbose);

	}
	return(dfList);
})


