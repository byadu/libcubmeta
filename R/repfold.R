#' @title repfold
#' @description Get reports folders of a user id
#' @param cfg is the configuration/metadata database connection
#' @param uid is the logged in user id
#' @details Returns folder id and names of all folders user has access to
#' @export
repfold<- function(cfg, uid) {
	rootfold<- getrootfld(cfg, uid)
	# report folders for a user
	repfoldq<-paste0("select ft_id,ft_name from folder_tree where ft_parid=", rootfold, " and ft_id not in (select gf_id from graph_def) and ft_id != ", rootfold)
	repfold<- tbl(cfg$configdb, sql(repfoldq))
	repfold<- as.data.frame(repfold)
#	nf<- nrow(repfold)
#	myf<- as.data.frame(cbind(rootfold[1,1], 'My Reports'))
#	colnames(myf)<- c('ft_id', 'ft_name')
#	repfold<- rbind(repfold, myf)
	return(repfold)
	}

getrootfld<- function(cfg, uid) {
#	rootfoldq<-paste0("select lu_fldroot from login_users where lu_id='", uid, "'")
#	rootfold<-dbGetQuery(configdb, rootfoldq)
	rootfold<- dofilter(cfg$login_users, 'lu_id', uid) %>% selectcols('lu_fldroot')
	rootfold<- as.data.frame(rootfold)
	rootfold[1,1]
	}

#' @title replist
#' @description Get reports list within a folder
#' @param cfg is the configuration/metadata database connection
#' @param folderid is the folder id for which reports list is required
#' @details Returns report id and title of all reports within the folder id
#' @export
replist<- function(cfg, folderid) {
	# reports list for a folder
#	replistq<-paste0("select gf_id,gf_user_title from graph_def where gf_folder=", folderid)
#	replist<-dbGetQuery(configdb, replistq)
	replist<- dofilter(cfg$graph_def, 'gf_folder', folderid) %>% selectcols(c('gf_id', 'gf_user_title'))
	replist<- as.data.frame(replist)
	return(replist)
	}

#' @title foldername
#' @description Get report folder name from folder id
#' @param cfg is the configuration/metadata database connection
#' @param folderid is the folder id for which folder name is being asked
#' @details Returns report name
#' @export
foldername<- function(cfg, folderid) {
#	q<- paste("select ft_name from folder_tree where ft_id=", folderid)
#	fname<-dbGetQuery(configdb, q)
	fname<- dofilter(cfg$folder_tree, 'ft_id', folderid) %>% selectcols('ft_name')
	fname<- as.data.frame(fname)
	return(fname[1,1])
	}

#' @title addfolder
#' @description Create a new folder to save reports in
#' @param foldername is the name of the folder to be created
#' @param uid is the user id of the logged in user
#' @param M is the configuration/metadata structure
#' @details Creates a new folder for the user
#' @export
addfolder<- function(foldername, uid, M) {
	rootfold<- getrootfld(M$cfg, uid)
	addft(M$mycfg, next_graph_seq(M$mycfg), foldername, rootfold, gp=NULL)
	}
#
# folder_tree
#  ft_id | ft_parid | ft_name | ft_seq | ft_status | ft_expand
#
addft<- function(my_cfg, ftid, ftname, foldid, gp) {
	ftstat<- 'A'
	ftseq<- 0
	ftx<- 1
	ftid<- ftid
	ftname<- ftname
	ftpar<- foldid
	ft<- as.data.frame(cbind(ftid[1], ftpar[1], ftname[1], ftseq[1], ftstat[1], ftx[1]))
	colnames(ft)<- c('ft_id', 'ft_parid', 'ft_name', 'ft_seq', 'ft_status', 'ft_expand')
	dbWriteTable(my_cfg, "folder_tree", ft, row.names=F, append=T)
	return(ftid)
	}

next_graph_seq<- function(my_cfg) {
	q<-"Update folder_id_seq set fldid_seq=last_insert_id(fldid_seq+1)\n"
	dbGetQuery(my_cfg, q)
	q<-"select last_insert_id() from folder_id_seq"
	seq<-dbGetQuery(my_cfg, q)
	return(as.integer(seq[1,1]))
}

