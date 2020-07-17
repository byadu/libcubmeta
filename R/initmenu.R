md_id=NULL; md_name=NULL; md_table=NULL; md_column=NULL; md_where=NULL; md_having=NULL; md_timecol=NULL; md_sumcnt=NULL;md_type=NULL
mt_id=NULL; mt_parid=NULL; mt_role=NULL
#' @title getmeas
#' @description Get details of measures from a list of ids
#' @importFrom stats reshape
#' @import dplyr
#' @import lazyeval
#' @import magrittr
#' @import RMySQL
#' @import DBI
#' @param cfg is the configuration/metadata database connection
#' @param meas is the list of measure ids for which details are required
#' @details Returns details of the measures requested as a dataframe
#' @export
getmeas<- function(cfg, meas) {
	mdf<-list()
	for(i in 1:length(meas)) {
	#	mq<-paste0("select md_id,md_name,md_table,md_column,md_sumcnt,md_timecol,md_where,md_having from menu_dtls where md_id=", meas[i])
	#	mdf[[i]]<- dbGetQuery(configdb, mq)
		md<- dofilter(cfg$menu_dtls, "md_id", meas[i]) %>% select(md_id, md_name, md_table, md_column, md_sumcnt, md_timecol, md_where, md_having)
		mdf[[i]]<- as.data.frame(md)
		}
	return(mdf)
	}

#' @title getmeasures
#' @description Get details of measures from a list of measure group ids
#' @param cfg is the configuration/metadata database connection
#' @param meas is the list of measure group ids for which details are required
#' @details Returns details of all the measures in all the measure groups requested as a dataframe
#' @export
getmeasures<- function(cfg, meas) {
	measures<-NULL
	for(i in 1:length(meas)) {
		chld<- meas[[i]]$chld
		chld<- as.data.frame(chld)
		nchld<- nrow(chld)
		if(nchld > 0)
		for(j in 1:nchld) {
			measi<- as.data.frame(getmeas(cfg, chld[j,1]))
			if(is.null(measures))
				measures<- measi
			else
				measures<- rbind(measures,measi)
			}
		}
	rownames(measures)<- NULL
	measures
	}

#' @title getdims
#' @description Get details of dimensions from a list of ids
#' @param cfg is the configuration/metadata database connection
#' @param dims is the list of dimensions ids for which details are required
#' @details Returns details of the dimensions requested as a dataframe
#' @export
getdims<- function(cfg, dims) {
	if(is.null(dims)) return(NULL)
	mdd<-list()
	mddt<- list()
	m<- list()
	for(i in 1:length(dims)) {
	#	q<- paste0("select md_id,md_name,md_table,md_column,md_type from menu_dtls where md_id=", dims[i])
	#	mdd[[i]]<- dbGetQuery(configdb, q)
		dim<- dofilter(cfg$menu_dtls, "md_id", dims[i]) %>% select(md_id, md_name, md_table, md_column, md_type)
		mdd[[i]]<- as.data.frame(dim)
		}
	return(mdd)
	}
	
#' @title getdimensions
#' @description Get details of dimensions from a list of measure group ids
#' @param cfg is the configuration/metadata database connection
#' @param dims is the list of dimensions group ids for which details are required
#' @details Returns details of all the dimensions in all the measure groups requested as a dataframe
#' @export
getdimensions<- function(cfg, dims) {
	dimensions<-NULL
	for(i in 1:length(dims)) {
		chld<- dims[[i]]$chld
		chld<- as.data.frame(chld)
		nchld<- nrow(chld)
		if(nchld > 0)
		for(j in 1:nchld) {
			dimsi<- as.data.frame(getdims(cfg, chld[j,1]))
			if(is.null(dimensions))
				dimensions<- dimsi
			else
				dimensions<- rbind(dimensions,dimsi)
			}
		}
	rownames(dimensions)<- NULL
	dimensions
	}

#' @title initmenu
#' @description Initialise the session of a user with allowed configuration
#' @param cfg is the configuration/metadata database connection
#' @param mymt is a vector of ids which user is allowed access to
#' @details Returns a list containing the user's configuration (measures, dimensions)
#' @export
initmenu<-function(cfg, mymt) {
#	dimq<-"select distinct md_id,md_name from menu_dtls where md_id>0 and md_id<10000 and md_column='' and md_table!=''"
#	dimgrp<-dbGetQuery(configdb, dimq)
	dimgrp<- filter(cfg$menu_dtls, md_id %in% mymt & md_id>0 & md_id<10000 & md_column == '' & md_table != '') %>% select(md_id, md_name)
	dimgrp<- as.data.frame(dimgrp)
	dims<-chldtree(cfg, dimgrp)
	dilist<- list()
	for(i in 1:length(dims))
		dilist[[dims[[i]]$parname]]<- dims[[i]]$chld$md_name

#	measq<-"select distinct md_id,md_name from menu_dtls where md_id>10000 and md_column='' and md_table!=''"
#	measgrp<-dbGetQuery(configdb, measq)
	measgrp<- filter(cfg$menu_dtls, md_id %in% mymt & md_id>10000 & md_column == '' & md_table != '') %>% select(md_id, md_name)
	measgrp<- as.data.frame(measgrp)
	meas<-chldtree(cfg, measgrp)
	milist<- list()
	for(i in 1:length(meas))
		milist[[meas[[i]]$parname]]<- meas[[i]]$chld$md_name

	measures<- getmeasures(cfg, meas)
	dimensions<- getdimensions(cfg, dims)

	return(list(dimgrp=dimgrp, dims=dims, measgrp=measgrp, meas=meas, measures=measures, dimensions=dimensions, dilist=dilist, milist=milist))
	}
