# filderids filters the ids further possible to be selected based on currently selected ids
# common function for xgetdims and xgetmeas
filterids<- function(cfg, ids, whichids) {
	tj<- tabsofids(cfg$configdb, ids)
	if(is.null(tj)) return(NULL)

	if(whichids=='dims') tji<- 1
	else tji<- 2

	tjother<- list()
	for(i in 1:nrow(tj)) {
# select tj_tab2 from table_joins where tj_tab1='dlf' union select tj_tab2 from table_joins where tj_tab1 in (select tj_tab2 from table_joins where tj_tab1='dlf')
# select tj_tab1 from table_joins,table_list where tj_tab2='Account' and tl_type=2 and tj_tab1=tl_tab union select tj_tab1 from table_joins where tj_tab2 in (select tj_tab1 from table_joins where tj_tab2='Account')
		if(tji==2) 
			q<- sql(paste0("select tj_tab2 from table_joins where tj_tab1='", tj[i,1], "' union select tj_tab2 from table_joins where tj_tab1 in (select tj_tab2 from table_joins where tj_tab1='", tj[i,1], "')"))
		else
			q<- sql(paste0("select tj_tab1 from table_joins,table_list where tj_tab2='", tj[i,1], "' and tl_type=2 and tj_tab1=tl_tab union select tj_tab1 from table_joins where tj_tab2 in (select tj_tab1 from table_joins where tj_tab2='", tj[i,1], "')"))
	#	q<- sql(paste0('select tj_tab', tji, ' from table_joins where tj_tab', ifelse(tji==1, 2, 1), '="', tj[i,1], '"'))
		tjo<- as.data.frame(tbl(cfg$configdb, q))[,1]
		if(is.null(tjo) || length(tjo) == 0)
			next
		if(tji == 2)
			tjo<- c(tjo,tj[i,1])
		tjother[[i]]<- tjo
		}

	if(length(tjother) > 0) {
		tabs<- tjother[[1]]
		if(length(tjother) > 1)
		for(i in 2:length(tjother)) {
			tabs<- intersect(tabs, tjother[[i]])
			}
		}
	else {
		tabs=tj
		}

	tc<- ""
	for(i in 1:length(tabs)) {
		tc<- paste0(tc, "'", tabs[i], "'")
		if(i!=length(tabs))
			tc<- paste0(tc, ",")
		}
	q<-sql(paste0("select distinct md_id,md_name from menu_dtls where md_column='' and md_id between ", ifelse(tji==1, "10000 and 100000", "0 and 9999"), " and md_table in (", tc, ") order by md_id"))
	filtgrp<- as.data.frame(tbl(cfg$configdb, q))

	filtids<- chldtree(cfg, filtgrp)
	return(list(grp=filtgrp, ids=filtids))
	}
filterdims<-function(cfg, meas) {
	f<- filterids(cfg, meas, 'meas')
	f
	}
filtermeas<-function(cfg, dims) {
	filterids(cfg, dims, 'dims')
	}
#' @title xmakepicklist
#' @description Create a list for pickerInput dropdown
#' @param cfg is the configuration/metadata structure
#' @param grp is the group of measures or dimensions
#' @details Returns the list with children of the group
#' @import shinyWidgets
#' @export
xmakepicklist<- function(cfg, grp) {
	ids<-as.character(grp[,1])
	names<- grp[,2]
	plist<- list()
	for(i in 1:length(names)) {
		ctree<- chldtree(cfg, ids[i])[[1]]$chld
		plist[[names[i]]]<- ctree[,2]
		}
	plist
	}
xmakepicklist2<- function(cfg, grp) {
	ids<-as.character(grp[,1])
	names<- grp[,2]
	plist<- list()
	for(i in 1:length(names)) {
		ctree<- chldtree(cfg, ids[i])[[1]]$chld
		cnamesv<- ctree[,2]
		cnamesl<- as.list(cnamesv)
		names(cnamesl)<- cnamesv
	#	plist[[names[i]]]<- ctree[,2]
		plist[[names[i]]]<- cnamesl
		}
	plist
	}
#' @title xgetmeas
#' @description Get the measures groups that can be accessed based on dimensions selected
#' @param cfg is the configuration/metadata structure
#' @param dimsel is selected dimension ids
#' @details Returns measure groups which can be selected based on dimension ids selected
#' @export
xgetmeas<- function(cfg, dimsel) {
	if(is.null(dimsel) || length(dimsel) == 0) {
		return(NULL)
		}
	else {
		fmeas<- filtermeas(cfg, dimsel)
		measgrp<- fmeas$grp
		return(measgrp)
		}
	}
#' @title xgetdims
#' @description Get the dimension groups that can be accessed based on measures selected
#' @param cfg is the configuration/metadata structure
#' @param metsel is selected measure ids
#' @details Returns dimension groups which can be selected based on measure ids selected
#' @export
xgetdims<- function(cfg, metsel) {
	if(is.null(metsel) || length(metsel) == 0) {
		return(NULL)
		}
	else {
		fdims<- filterdims(cfg, metsel)
		return(fdims$grp)
		}
	}
#' @title xmsel
#' @description perform a select/deselect operation of an id list
#' @param csel is the currently selected ids 
#' @param sel is the selected/deselected id
#' @details Returns the selected ids after the select/deselect operation
#' @export
xmsel<- function(csel, sel) {
	if(length(csel) > length(sel)) {
		thisel<- setdiff(csel, sel)
		csel<- csel[csel!=thisel]
		}
	else {
		thisel<- setdiff(sel, csel)
		csel<- c(csel, thisel)
		}
	csel
	}
