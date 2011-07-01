

#' Swap the primary and secondary key columns
#'
#' @name swapKeys
#' @param idMap the IdMap object or a list of IdMap objects which keys to be swapped
#' @param verbose if TRUE enables diagnostic messages
#' @return IdMap object or list of IdMap objects with swapped keys
#' @keywords internal
#' @author Roger Day, Alex Lisovich

swapKeys<-function(idMap,verbose=FALSE) {
  .swapKeys<-function(idMap,verbose){
	if(verbose)
		cat("Swapping ID Map keys\n");
	pairs<-as.UniquePairs(idMap,keepMissing=FALSE,verbose=verbose);
	pairs<-pairs[,c(2,1)];
	idMap<-as.IdMap(pairs,verbose=verbose);
	return(idMap);
  }

  if (inherits(idMap,"list")){
	res<-list();
	for (name in names(idMap)){
		if(verbose)
			cat(name,":");
		res[[name]]<-.swapKeys(idMap[[name]],verbose);
	}		
	return(res);
  }else{
	return(.swapKeys(idMap,verbose));
  }
}


#' Create a UniquePairs object by converting a single IdMap or a list of Id Maps into 
#' a single or list of unique pairs data structures optionally intersecting
#' the secondary ID set with an external secondary ID set if farther data 
#' size reduction is necessary
#'
#' The alternative representation of an IdMap suitable for performing
#' the correlation related processing. Contains a data frame with two columns, 
#' each row of which represents a unique pair <primary ID, secondary ID>
#' where primary ID corresponds to the primaryIDs of an ID Map and secondary ID 
#' corresponds to a single ID from a list of comma separated secondary IDs within
#' the corresponding ID Map. The column names correspond to the primary/secondary 
#' keys of an Id Map ('acc' and 'probeset' for example)
#'
#' @name as.UniquePairs
#' @param idMapData an IdMap object or a list of IdMap objects to be converted into UniquePairs
#' @param secondaryIDs optional secondary ID list on which the resulting UniquePairs is intersected. Default is NULL (not present).
#' @param keepMissing logical indicating if the rows with empty secondary IDs should removed 
#' from the resulting object. Default is FALSE (keep such rows)
#' @param verbose if TRUE enables diagnostic messages
#' @return UniquePairs object 
#' @keywords internal
#' @author Roger Day, Alex Lisovich

as.UniquePairs<-function(idMapData,secondaryIDs=NULL,keepMissing=FALSE,verbose=FALSE) {

	getUniquePairs<-function(idMap,secondaryIDs=NULL,keepMissing=FALSE,verbose=FALSE) {
		if (verbose)
			cat("unfolding ID Map ...\n");

		primaryList<-rownames(idMap);
		trimmedStrings = gsub(" ","",idMap[,2],fixed=TRUE);
		if(keepMissing)
			trimmedStrings[trimmedStrings==""]<-NA;
		secondaryIdList = strsplit(trimmedStrings , ",")

		lengths<-unlist(lapply(secondaryIdList , function(x) length(x)));
		res<-array("",dim=c(sum(lengths),2));
		res[,1]<-rep(idMap[,1],times=lengths);
		res[,2]<-unlist(secondaryIdList);
		if (!is.null(secondaryIDs))
			res<-subsetByColumn(res,secondaryIDs,2,verbose);
		colnames(res)<-colnames(idMap);
		return (as.data.frame(res,stringsAsFactors=FALSE));
	}


	if(!inherits(idMapData,"list")){
		return(getUniquePairs(idMapData,secondaryIDs,keepMissing,verbose));
	}

	if (verbose) {
		cat("retrieving unique pairs for <",names(idMapData),">\n");
	}
	res<-list();
	for (name in names(idMapData)){
		if (verbose)
			cat(name,":");
		res[[name]]<-getUniquePairs(idMapData[[name]],secondaryIDs,keepMissing,verbose);
	}
	return(res);


}


#' Perform the transformation of the UniquePairs object into the IdMap object
#'
#' @name as.IdMap
#' @param uniquePairs the UniquePairs object to be converted into IdMap
#' @param keepOrder logical indicating if the original order of primary IDs should be lept. Default is TRUE
#' @param verbose if TRUE enables diagnostic messages
#' @keywords internal
#' @author Roger Day, Alex Lisovich

as.IdMap<-function(uniquePairs,keepOrder=TRUE,verbose=FALSE){
	if (verbose)
		cat("converting unique pairs into the ID Map ...\n");

	f<-factor(uniquePairs[,1]);
	splits<-split(uniquePairs[,2],f);
	secondaryList<-lapply(splits,paste,collapse=",");
	secondaryIDs<-unlist(secondaryList);
	res<-cbind(levels(f),secondaryIDs);

	if(keepOrder) {
		primaryIDs<-unique(uniquePairs[,1]);
		res<-res[primaryIDs,];
	}
	
	colnames(res)<-colnames(uniquePairs);
	return(as.data.frame(res,stringsAsFactors=FALSE));
}


#' Extract subset of rows from a data frame or a list of data frames
#' by intersecting on a particular column
#'
#' This function outputs a data frame which set of rows is a product of intersecting on a particular column
#' of the imput data frame(s) rows with a given set of names (subset).
#'
#' @name subsetByColumn
#' @param frameData input data frame or a list of data frames.
#' @param subset character vector of names partially intersecting with given data frame 
#' column and defining the set on which merging is to be performed.
#' @param column The column on which the intersection is to be performed.
#' @return data frame or list of data frame which column set is a product of
#' intersecting of a particular column with a (partially intersecting) subset.
#' @param verbose if TRUE enables diagnostic messages.
#' @examples
#' \dontrun{
#' commonProteins<-subsetByRow(experimentSet,proteins,column=1);
#' }
#' @keywords internal
#' @author Roger Day, Alex Lisovich 


subsetByColumn<-function(frameData, subset,column,verbose=FALSE) {
	subsetByColumnInternal<-function(dataFrame, subset,column,no.case,verbose=FALSE) {
		if (verbose)
			cat("extracting subset by column...\n");

		subset<-as.data.frame(subset);
		subset<-as.character(as.matrix(subset));
		entries<-dataFrame[,column];

		subset<-intersect(entries,subset);

		indexes<-which(entries %in% subset);
		commons<-dataFrame[indexes,];
		return(as.data.frame(commons,stringsAsFactors=FALSE));
	}

	if(!inherits(frameData,"list"))
		return(subsetByColumnInternal(frameData,subset,column,verbose));

	res<-list();
	for (name in names(frameData)){
		if (verbose)
			cat("frame",name,": ");
		res[[name]]<-subsetByColumnInternal(frameData[[name]],subset,column,verbose);
	}
	return(res);

}



