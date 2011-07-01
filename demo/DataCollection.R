#####################################################
#retrieve all IdMap's from various services
#####################################################

library(IdMappingRetrieval);

#####################################################
# setup verbosity level and DB keys
#####################################################

verbose=TRUE;
primaryKey="Uniprot";
secondaryKey="Affy";

#####################################################
# data retrieval from annotation services
#####################################################

#init annotation DB
IdMappingAnnotationData.root<-Annotation$init(verbose=verbose);

#init Affymetrix service
AnnotationAffx$setCredentials(user="alex.lisovich@gmail.com",password="125438",verbose);

#retrieve data from various services
idMapList<-ServiceManager$getIdMapList(primaryKey=primaryKey,secondaryKey=secondaryKey,verbose=TRUE);


