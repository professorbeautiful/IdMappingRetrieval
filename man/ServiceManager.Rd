%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  serviceManager.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ServiceManager}
\docType{class}
\alias{ServiceManager}

 \encoding{latin1}

 \title{The ServiceManager class}

 \description{
  Package:   \cr
\bold{Class ServiceManager}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{ServiceManager}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{ServiceManager}\cr
extends \link[R.oo]{Object}\cr



 ServiceManager class serves as a container for a set of specialized service objects
 and provides the means for (optionally interactive) handling of such a set simplifying
 the process of data retrieval from a variety of resources in a batch mode.
 }

 \usage{ServiceManager(services=list(), ...)}


 \arguments{
 \item{services}{ Annotation services to be contained within the ServiceManager object.
 Default is \code{\link[base]{NULL}}.}
 \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingRetrieval:addServices.ServiceManager]{addServices}} \tab Add services to the ServiceManager object.\cr
 \tab \code{\link[IdMappingRetrieval:getDataFrameList.ServiceManager]{getDataFrameList}} \tab Collect raw data data from various online query systems utilizing Annotation service functionality.\cr
 \tab \code{\link[IdMappingRetrieval:getIdMapList.ServiceManager]{getIdMapList}} \tab Collect ID mapping data from various online query systems utilizing Annotation service functionality.\cr
 \tab \code{\link[IdMappingRetrieval:getServices.ServiceManager]{getServices}} \tab Get list of annotation services encapsulated within the particular ServiceManager object.\cr
 \tab \code{\link[IdMappingRetrieval:setServices.ServiceManager]{setServices}} \tab Set the list of services for a ServiceManager object.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
