#' @title opendb
#' @description Open a 'MySQL' connection
#' @param db is the database name
#' @param host is the database host/server
#' @param user is the database user id
#' @param password is the database password for the user
#' @param port is the database port
#' @details Returns a 'MySQL' connection
#' @export
opendb<- function(db=NULL, host='localhost', user='cubot', password='', port=3306) {
	src_mysql(dbname=db, host=host, port=port, username=user, password=password)
	}

#' @title gettabs
#' @description Get tables in a 'MySQL' database
#' @param dbconn is the database handle
#' @details Returns the set of tables in the database
#' @export
gettabs<- function(dbconn) {
	as.character(src_tbls(dbconn))
	}

#' @title getcols
#' @description Get columns of a table in a 'MySQL' database
#' @param dbconn is the database handle
#' @param tab is the table for which columns are required
#' @details Returns the columns of the table
#' @export
getcols<- function(dbconn, tab) {
	t<- tbl(dbconn, tab)
	as.character(tbl_vars(t))
	}

#' @title coltypes
#' @description Get column types of a table in a 'MySQL' database
#' @param dbconn is the database handle
#' @param tab is the table for which columns are required
#' @details Returns the columns and associated types of the table
#' @export
coltypes<- function(dbconn, tab) {
	q<- paste('select * from', tab, 'limit 1')
	d<- dbSendQuery(dbconn,q)
	info<-dbColumnInfo(d)
	dbFetch(d)
	dbClearResult(d)
	info<- info[,c('name','type')]
	}

#' @title datecols
#' @description Get the date columns in a table in a 'MySQL' database
#' @param dbconn is the database handle
#' @param tab is the table for which columns are required
#' @details Returns the date columns of the table
#' @export
datecols<- function(dbconn, tab) {
	ct<- coltypes(dbconn, tab)
	ct<- ct[ct$type=='DATE'|ct$type=='DATETIME',]
	as.character(ct$name)
	}

#' @title getprimary
#' @description Get the primary key of a table in a 'MySQL' database
#' @param dbconn is the database handle
#' @param tab is the table for which columns are required
#' @details Returns the primary key columns of the table
#' @export
getprimary<- function(dbconn, tab) {
	q<- paste('SHOW KEYS FROM',tab, 'WHERE Key_name = "PRIMARY"')
	cols<- dbGetQuery(dbconn, q)
#	cols<- as.data.frame(tbl(dbconn, q))
	as.character(cols$Column_name)
	}

#' @title dojoin
#' @description Join two tables
#' @param fact is the fact table of join
#' @param dim is the dimension table of join
#' @param tj1 is the column of fact table for join
#' @param tj2 is the column of dimension table for join
#' @details Returns the joined table
#' @importFrom stats setNames
#' @export
dojoin<- function(fact, dim, tj1 , tj2) {
	inner_join(fact, dim, setNames(tj2 , tj1))
	}

#' @title selectcols
#' @description Select columns of a 'dplyr' tbl table
#' @param tbl is the name of the tibble
#' @param cols are the columns to be selected
#' @details Returns the selected columns from the table
#' @export
selectcols<- function(tbl, cols) {
	select(tbl, one_of(cols))
	}

#' @title dofilter
#' @description Select columns of a 'dplyr' tbl table based a filter
#' @param tbl is the name of the tibble
#' @param colcond is the column being filtered
#' @param colval is the filter value
#' @details Returns the selected filtered columns of the table
#' @export
dofilter<- function(tbl, colcond, colval) {
	if(!is.null(colcond)) {
		cond<- paste0(colcond, "==", "'", colval, "'")
		tbls<- filter_(tbl, cond)
		}
	}

#' @title grpby
#' @description Aggregate table by a set of columns
#' @param tbls is the name of the tibble
#' @param cols are the columns to be aggregated
#' @details Returns the aggregate resulting table
#' @export
grpby<- function(tbls, cols) {
	dots <- lapply(cols, as.symbol)
	group_by_(tbls, .dots=dots, add=T)
	}

#' @title rowcount
#' @description Get row count of a table
#' @param tbl is the name of the tibble
#' @param col is the column to use for count
#' @details Returns the row count of the table
#' @export
rowcount<- function(tbl, col) {
	as.data.frame(summarise_(tbl, interp(~n())))
	}

#' @title dosum
#' @description Get aggregate value by sum
#' @param tbl is the name of the tibble
#' @param col are the columns to be aggregated
#' @details Returns the aggregated summarised table
#' @export
dosum<- function(tbl, col) {
	as.data.frame(summarise_(tbl, interp(~sum(var), var=as.name(col))))
	}

#' @title initconfig
#' @description Initialise the 'tibbles' of the meta data database
#' @param configdb is the name of the configuration data base 
#' @details Returns the initialised dplyr configuration handle
#' @export
initconfig<- function(configdb) {
	cfg<- list()
	cfg$configdb<- configdb

	cfg$etl_store<- tbl(configdb, 'etl_store')
	cfg$etl_dbstore<- tbl(configdb, 'etl_dbstore')
	cfg$table_list<- tbl(configdb, 'table_list')
	cfg$table_joins<- tbl(configdb, 'table_joins')
	cfg$menu_dtls<- tbl(configdb, 'menu_dtls')
	cfg$menu_trees<- tbl(configdb, 'menu_trees')
	cfg$user_content<- tbl(configdb, 'user_content')
	cfg$graph_def<- tbl(configdb, 'graph_def')
	cfg$graph_props<- tbl(configdb, 'graph_props')
	cfg$filter_header <- tbl(configdb, 'filter_header')
	cfg$filter_details <- tbl(configdb, 'filter_details')
	cfg$data_xy<- tbl(configdb, 'data_xy')
	cfg$data_series<- tbl(configdb, 'data_series')
	cfg$data_seriesvals<- tbl(configdb, 'data_seriesvals')
	cfg$graph_series<- tbl(configdb, 'graph_series')
	cfg$series_seq<- tbl(configdb, 'series_seq')
	cfg$login_users<- tbl(configdb, 'login_users')
	cfg$login_tree<- tbl(configdb, 'login_tree')
	cfg$folder_tree<- tbl(configdb, 'folder_tree')
	cfg$folder_id_seq<- tbl(configdb, 'folder_id_seq')
	return(cfg)
	}
