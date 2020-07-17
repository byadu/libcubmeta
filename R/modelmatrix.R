getmatrix<- function(cfg, tjoins) {
#	q<-"select tj_tab1,tj_tab2,count(*) j from table_joins group by 1,2 order by 1,2"
#	tj<-dbGetQuery(configdb, q)
	cols<- c('tj_tab1', 'tj_tab2')
#	tj<- selectcols(cfg$table_joins, cols) %>% arrange_(cols)
	tj<- selectcols(tjoins, cols) %>% arrange_(cols)
	tj<- as.data.frame(tj)
	
#	q<-"select tj_tab1,count(*) t from table_joins group by 1 order by 2 desc limit 8"
#	trns<-dbGetQuery(configdb, q)
#	trns<- selectcols(cfg$table_joins, 'tj_tab1') %>% grpby('tj_tab1') %>% rowcount() 
	trns<- selectcols(tjoins, 'tj_tab1') %>% grpby('tj_tab1') %>% rowcount() 
	trns<- as.data.frame(trns)
	colnames(trns)<- c('tj_tab1', 't')
t=NULL
	trns<- arrange(trns, desc(t))
	trns<- as.data.frame(trns)
#	q<-"select tj_tab2,count(*) d from table_joins group by 1 order by 2 desc limit 8"
#	dims<-dbGetQuery(configdb, q)
#	dims<- selectcols(cfg$table_joins, 'tj_tab2') %>% grpby('tj_tab2') %>% rowcount() 
	dims<- selectcols(tjoins, 'tj_tab2') %>% grpby('tj_tab2') %>% rowcount() 
	dims<- as.data.frame(dims)
#	dims<- as.data.frame(trns)
	colnames(dims)<- c('tj_tab2', 'd')
d=NULL
	dims<- arrange(dims, desc(d))
	dims<- as.data.frame(dims)
	
	trn<-c()
	t<-trns[,1]
	for(i in 1:length(t)) {
	#	q<-paste0("select md_name from menu_dtls where md_column='' and md_table='", t[i], "' limit 1") 
	#	mdname<-dbGetQuery(configdb, q)
		t[i]<- as.character(t[i])
		md<- filter(cfg$menu_dtls, md_table==!!t[i] & md_column=='')
		md<- as.data.frame(md)
		if(nrow(md)==0) 
			next
		md<- md%>% select(md_name, md_table)
		md<- as.data.frame(md)

		if(nrow(md) > 0)
			trn[i]<- md[1,2]
		else
			trn[i]<- t[i]
		}
	trn<- trn[!is.na(trn)]
	trns<- subset(trns, trns[,1] %in% trn)

	trns<- trns[1:min(20,nrow(trns)),]
	dims<- dims[1:min(20,nrow(dims)),]
	if(nrow(dims) > nrow(trns))
		dims<- dims[1:nrow(trns),]
	else if(nrow(dims) < nrow(trns))
		trns<- trns[1:nrow(dims),]
	tj1<-merge(tj,trns)
	tj2<-merge(tj1,dims)
	tj<-tj2[,1:3]

	wide <- reshape(tj, idvar = "tj_tab1", timevar = "tj_tab2", direction = "wide")
	wide[is.na(wide)]<-0
	
	md<- c()
	for(i in 1:nrow(trns)) {
		ti<- trns[i,1]
		mdn<- as.data.frame(filter(cfg$menu_dtls, md_table == ti & md_column=='') %>% select(md_name))
		md[i]<- mdn[1,1]
		}
	md<- as.data.frame(md)

	rownames(wide)<-md[,1]
	wide[,1]<-NULL
	
	t<-unique(tj[,1])
	dim<-c()
	for(i in 1:length(t)) {
	#	q<-paste0("select md_name from menu_dtls where md_column='' and md_table='", t[i], "' limit 1") 
	#	mdname<-dbGetQuery(configdb, q)
		mdname<- filter(cfg$menu_dtls, md_column=='' & md_table==!!t[i]) %>% select(md_name)
		mdname<- as.data.frame(mdname)
#		mdname<- mdname[1,1]

		if(nrow(mdname) > 0)
			dim[i]<-mdname[1,1]
		else
			dim[i]<-t[i]
		}
	colnames(wide)<-dim
	wide<-data.matrix(wide)
	}
