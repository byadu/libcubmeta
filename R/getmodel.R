edb_dsn=NULL;est_storetype=NULL;est_storename=NULL;edb_name=NULL;edb_user=NULL;edb_pass=NULL;edb_hostname=NULL;edb_port=NULL
#' @export
#' @title getdbstores
#' @description Get all 'cuborg' warehouse database type of stores
#' @param cfg is the configuration/metadata database connection
#' @details Returns all configured database stores
getdbstores<- function(cfg) {
#	q<- "select est_storename, edb_name, edb_user, edb_pass, edb_hostname, edb_port from etl_store, etl_dbstore where edb_dsn='Mysql Dsn' and est_dbdirid=edb_dbid and est_storetype='D'"
#	dbGetQuery(configdb, q)
	stdb<- dojoin(cfg$etl_store, cfg$etl_dbstore, "est_dbdirid", "edb_dbid")
	stdb<- filter(stdb, edb_dsn=='Mysql Dsn' & est_storetype=='D')
	stdb<-select(stdb, est_storename, edb_name, edb_user, edb_pass, edb_hostname, edb_port)
	stdb<- as.data.frame(stdb)
	return(stdb)
	}

#' @export
#' @title getstore
#' @description Get 'cuborg' warehouse store
#' @param cfg is the configuration/metadata database connection
#' @param store is the data base store
#' @details Returns store particulars
getstore<- function(cfg, store) {
#	q<- paste0("select edb_name, edb_user, edb_pass, edb_hostname, edb_port from etl_store, etl_dbstore where edb_dsn='Mysql Dsn' and est_dbdirid=edb_dbid and est_storetype='D' and est_storename='", store, "'")
#	dbGetQuery(configdb, q)
	stdb<- dojoin(cfg$etl_store, cfg$etl_dbstore, "est_dbdirid", "edb_dbid")
	stdb<- dofilter(stdb, 'est_storename', store)
	stdb<-select(stdb, edb_name, edb_user, edb_pass, edb_hostname, edb_port)
	return(as.data.frame(stdb))
	}

#' @title tabstats
#' @description Get the table statistics for a data base
#' @param cfg is the configuration/metadata database connection
#' @param store is the data base store
#' @details Returns table row counts, and classification of the tables as small, medium, and large
#' @details (Potentially useful for auto idenitification of fact tables)
#' @export
tabstats<- function(cfg, store) {
	if(is.null(store)) return(NULL)
	db<- getstore(cfg, store)
#	datadb<<-dbConnect(MySQL(), user=db$edb_user,password=db$edb_pass,dbname=db$edb_name, host=db$edb_hostname, port=as.integer(db$edb_port))
	datadb<-opendb(user=db$edb_user,password=db$edb_pass,db=db$edb_name, host=db$edb_hostname, port=as.integer(db$edb_port))
	t<-	gettabs(datadb)

	ts<- data.frame()
	#for (i in 1:nrow(t)) {
	for (i in 1:length(t)) {
		stt<- rowcount(tbl(datadb, t[i]))
		thisrow<- as.data.frame(cbind(t[i],stt[1]))
		if(nrow(ts)==0)
			ts<- thisrow
		else
			ts<- rbind(ts, thisrow)
		}
#	ts[,2]<- as.numeric(levels(ts[,2]))[ts[,2]]
	counts<- ts[,2]
#	counts<- as.numeric(levels(counts))[counts]
	colnames(ts)<- c('Table', 'Rows')
	ts$Size<- cut(counts, 3, labels=c('small', 'medium', 'large'))
	ts
	}

gettables<- function(cfg, tabs) {
tl_tab=NULL
#	q<- 'select tl_tab, tl_type from table_list'
#	tl<- dbGetQuery(configdb, q)
	tl<- filter(cfg$table_list, tl_tab %in% !!tabs)
	tl<- selectcols(cfg$table_list, c('tl_tab', 'tl_type'))
	tl<- as.data.frame(tl)
	tl<- within(tl, tl_type<- ifelse(tl_type==1, 'D', 'F'))
	colnames(tl)<- c('name', 'type')
	tl
	}

getjoins<- function(cfg, tjoins) {
	#q<- "select tj_tab1, tj_tab2, tj_col1, tj_col2 from table_joins"
	#tj<- dbGetQuery(configdb, q)
#	tj<- selectcols(cfg$table_joins, c('tj_tab1', 'tj_tab2', 'tj_col1', 'tj_col2'))
	tj<- selectcols(tjoins, c('tj_tab1', 'tj_tab2', 'tj_col1', 'tj_col2'))
	tj<- as.data.frame(tj)
	colnames(tj)<- c('Fact_Table', 'Dimension_Table', 'Fact_Column', 'Dimension_Column')
#	for(i in 1:4)
#		tj[,i]<- as.factor(tj[,i])
	tj
	}


#' @title getmodel
#' @description Get the configured data model
#' @param cfg is the configuration/metadata database connection
#' @param tabs is the list of tables allowed access
#' @details Returns table names in the data model, table joins of facts and dimensions, and a matrix of the joins for drawing a chord diagram
#' @export
getmodel<- function(cfg, tabs) {
	tjoins<- cfg$table_joins
	#tjoins<- filter(cfg$table_joins, tj_tab1 %in% tabs)
	return(list(tl=gettables(cfg, tabs), tj=getjoins(cfg, tjoins), matrix=getmatrix(cfg, tjoins)))
	}
