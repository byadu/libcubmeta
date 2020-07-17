#' @title chldtree
#' @description Get all children of a tree
#' @param cfg is the configuration/metadata database connection
#' @param tree contains the list of ids which are the parent ids that have children
#' @details Returns ids of all the children of requested tree
#' @export
chldtree<- function(cfg, tree) {
	tree<- as.data.frame(tree)
	mtall<-list()
	mt<-list()
	for(i in 1:nrow(tree)) {
		#mtq<-paste0("select md_id,md_name from menu_dtls, menu_trees where md_id=mt_id and mt_parid=", tree[i,1])
		#d<-dbGetQuery(configdb, mtq)
		mdt<- dojoin(cfg$menu_dtls, cfg$menu_trees, "md_id", "mt_id")
		d<- dofilter(mdt, "mt_parid", tree[i,1])
		d<- arrange_(d, "md_id")
		d<- as.data.frame(d)

		mt$parid<-tree[i,1]
		mt$parname<-tree[i,2]
		mt$chld<-d
		mtall[[i]]<-mt
		}
	return(mtall)
	}

#' @title tabsofids
#' @description Get table names corresponding to ids
#' @param cfg is the configuration/metadata database connection
#' @param ids is the vector of ids for which table names are required
#' @details Returns the table names of the ids
#' @export
tabsofids<- function(cfg, ids) {
	if(is.null(ids) || length(ids) == 0) return(NULL)
	id1<-""
	for(i in 1:length(ids)) {
		id1<-paste0(id1, "'", ids[i], "'")
		if(i!=length(ids))
			id1<-paste0(id1, ",")
		}
#	q<- paste0("select distinct(md_table) from menu_dtls where md_id in (", id1, ")")
#	dbGetQuery(configdb, q)
	q<- sql(paste0("select distinct(md_table) from menu_dtls where md_id in (", id1, ")"))
	as.data.frame(tbl(cfg, q))
	}

#' @title getchld
#' @description Get children of a parent (by name)
#' @param mt is menu tree which contains all the ids as a tree
#' @param pname is the parent name for which child is being looked for
#' @details Returns child menu tree structure
#' @export
getchld<- function(mt, pname) {
	for(i in 1:length(mt)) {
		if(mt[[i]]$parname==pname)
			return(mt[[i]]$chld)
		}
	return(NULL)
	}

#' @title getchldbyid
#' @description Get children of a parent (by id)
#' @param mt is menu tree which contains all the ids as a tree
#' @param pid is the parent id for which child is being looked for
#' @details Returns child menu tree structure
#' @export
getchldbyid<- function(mt, pid) {
	for(i in 1:length(mt)) {
		if(mt[[i]]$parid==pid)
			return(mt[[i]]$chld)
		}
	return(NULL)
	}

#' @title getparent
#' @description Get the parent id of a child
#' @param cfg is the configuration/metadata database connection
#' @param cid is the child id for whom parent id is required
#' @details Returns parent id of the child id passed
#' @export
getparent<- function(cfg, cid) {
	parid<- filter(cfg$menu_trees, mt_id==cid) %>% select(mt_parid)
	as.data.frame(parid)[1,1]
	}

#' @title getmnames
#' @description Get names from ids
#' @param m is the menu details structure (measures / dimensions)
#' @param mid is the vector of ids for which names are requested
#' @details Returns names of ids
#' @export
getmnames<- function(m, mid) {
	m[m$md_id %in% mid,]$md_name
	}

formatdrills<- function(drills) {
	if(is.null(drills) || is.null(drills$drillval) || is.na(drills$drillid) || is.na(drills$drillval[1])) return(NULL)
	d<- NULL
	nd<- length(drills$drillname)
	for(i in 1:nd) {
		d<-paste0(d, drills$drillname[i], '=', drills$drillval[i])
		if(i != nd)
			d<- paste0(d, ', ') 
		}
	d
	}

#' @title lookupid
#' @description Look up ids of names
#' @param M is the configuration/metadata structrue
#' @param mnames is the list of names for which ids are required
#' @param grp is the id group - dimensions or measures
#' @details Returns ids corresponding to the names
#' @export
lookupid<- function(M, mnames, grp) {
	if(grp == 'd')
		lookup<- M$mt$dimensions
	else
		lookup<- M$mt$measures
	ids<- c()
	for(i in 1:length(mnames)) {
		ids[i]<- lookup[lookup$md_name==mnames[i],]$md_id
		}
	ids
	}

getdummymeas<- function(dims) {
	dims<- dims[[1]]
# change this
#	measgrp<- s$dmsel
measgrp=NULL
	meas<- chldtree(measgrp)
	dummymeas<- meas[[1]]$chld[1,]
	dummymeas<- dummymeas %>% select(md_id, md_name, md_table, md_column, md_sumcnt, md_timecol, md_where, md_having)
	dummymeas<- as.data.frame(dummymeas)
	dummymeas$md_column<- '*'
	dummymeas$md_sumcnt<- 'count'
	dummymeas$md_name<- 'Count'
	dummymeas$md_where<- ''
	dummymeas$md_having<- ''
	mdf<- list()
	mdf[[1]]<- dummymeas
	mdf
	}
