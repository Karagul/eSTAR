# hist info to prc_hist
#- Save S&P500 daily price raw data to eSTAR database
#- Mod. by Ted, Fri Feb  5 11:54:17 EST 2016
#-----------------------------------------------------
library(quantmod)
require(stringr)

#- Clean raw data 
cleanRawdata <- function(datax,dataxHdr,ticker) {
	dataxHdr=append(dataxHdr,"pbDate")
	dataN=length(datax[,1])
	ymd=as.integer(as.character(as.Date(row.names(datax),"%Y-%m-%d"),"%Y%m%d"))
	datax$pbDate=ymd
	colnames(datax) <- dataxHdr
	row.names(datax) <- (1:dataN)
	for(k in 1:dataN) datax$Name[k]=ticker
	return(datax)
}
#- Write data to Database
data2dbTbl <- function(dbConn, dstTbl, dbdata,iniFlg) {
#	query <- paste('DELETE FROM',dstTbl)
#	rst <- dbGetQuery(mydb, query)
	print(iniFlg)
	if(iniFlg>0)
		dbWriteTable(dbConn, dstTbl, dbdata, row.names=F, append=T,overwrite=F)
	else
		dbWriteTable(dbConn, dstTbl, dbdata, row.names=F, append=F,overwrite=T)
	return(iniFlg+1)
}

#===== Load Price History
LoadPrcHistory <- function(tks,mydb,src) {
	tksN=length(tks)
	if ( !missing(src) && src=="google") {
		prcHst <- lapply(tks, getSymbols, auto.assign=FALSE,src="google")
		dstTbl="prc_hist_google"
	} else {
		prcHst <- lapply(tks, getSymbols, auto.assign=FALSE)
		dstTbl="prc_hist"
	}
	jx=0
	for(j in 1:tksN) {
		datax=data.frame(prcHst[[j]])
		dataxHdr=str_replace(colnames(datax),paste(tks[j],".",sep=""),"")
		datax <- cleanRawdata(datax,dataxHdr,tks[j])
		jx=data2dbTbl(mydb,dstTbl,datax,jx) 
	}
}

#===== Load Dividend History
LoadDvndHistory <- function(tks,mydb) {
	tksN=length(tks)
	dvndHst <- lapply(tks, getDividends)
	dstTbl="dvnd_hist"
	jx=0
	for(j in 1:tksN) {
		if ( length( dvndHst[[j]] )<1 ) 
			next
		datax=data.frame(dvndHst[[j]])
		dataxHdr=c("Dividend")
		datax <- cleanRawdata(datax,dataxHdr,tks[j])
		jx=data2dbTbl(mydb,dstTbl,datax,jx) 
	}
}

#- To view & save finacial report [IS,BS,CF] for [Q,A]
LoadFinHistory <- function(rpt,fq,tks,finHst,mydb) {
	epsM=lapply(finHst, function(x) rbind(viewFin(x,rpt,fq)["Diluted Normalized EPS",],viewFin(x,rpt,fq)["Operating Income",] , viewFin(x,rpt,fq)["Total Revenue",] ))

	for(j in 1:length(tks)) {
		ticker=tks[j]
		eps=t(epsM[[j]])
		m=data.frame(ticker,eps)
		colnames(m)<-c("Ticker","dEPS","Income","Revenue")
		if(j==1)
			MQ=m
		else
			MQ=rbind(MQ,m)
		print(m)
	}
	print(MQ)
	ymd=as.integer(as.character(as.Date(row.names(MQ),"%Y-%m-%d"),"%Y%m%d"))
	MQ$pbDate=ymd
	dbWriteTable(mydb, paste("fin_hist",fq,sep="_"), MQ, row.names=F, append=F,overwrite=T)
}


#- To get list of fields in finacial report [IS|BS|CF] for both [Q|A] frequency
#- Saved in [fin_field] table in [eSTAR]
LoadFincialField <- function(tks,finHst,mydb) {
	fc=c("Field","Report","Freq")
	g=data.frame(rownames(finHst[[1]]$IS$Q),"IS","Q");colnames(g)=fc;
	f=data.frame(rownames(finHst[[1]]$BS$Q),"BS","Q");colnames(f)=fc;g=rbind(f,g);
	f=data.frame(rownames(finHst[[1]]$CF$Q),"CF","Q");colnames(f)=fc;g=rbind(f,g);
	f=data.frame(rownames(finHst[[1]]$IS$Q),"IS","A");colnames(f)=fc;g=rbind(f,g);
	f=data.frame(rownames(finHst[[1]]$BS$Q),"BS","A");colnames(f)=fc;g=rbind(f,g);
	f=data.frame(rownames(finHst[[1]]$CF$Q),"CF","A");colnames(f)=fc;g=rbind(f,g);
	dbWriteTable(mydb, "fin_field", g, row.names=F, append=F,overwrite=T)
}

#----------------------------------
#- Main Program

#----- SET TICKERS
tks <- c("AAPL","GOOG","AMZN","MSFT","IBM","INTC","GRMN","NFLX","UTX","VZ","GE","JPM","GS","NKE","LB","CMG","PNRA","KO","PG","DIS","JNJ","MRK","PFE","XOM","GWW","UHAL","TSM","SPY","XLK","NKE")
options("getSymbols.warning4.0"=FALSE)
#- QUICK TEST ONLY ----#
#prcHst <- lapply(tks, getSymbols, auto.assign=FALSE,src="google")
#head(prcHst[[1]])
#dvndHst <- lapply(tks, getDividends,src="google")
#tail(dvndHst[[1]])
#----------------------#

#library(RMySQL)
library("RPostgreSQL")
mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host='bbub2',dbname='eSTAR')
#mydb<-dbConnect(MySQL(),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')
LoadPrcHistory(tks,mydb)
LoadDvndHistory(tks,mydb)


#- Reference ONLY
#date.be <- c("2000-01-01", "2015-01-01")
#lapply(finHst, function(x) x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",])
#lapply(finHst, function(x) x$IS$Q["Diluted Normalized EPS", ])
#lapply(finHst, function(x) x$IS$Q["Dividends per Share - Common Stock Primary Issue", ])

#- To load finacial report [IS,BS,CF] for both [Q,A]
finHst <- lapply(tks, getFinancials, auto.assign=FALSE)

#- To view view & save report [IS,BS,CF] for both [Q,A]
LoadFinHistory("IS","A",tks,finHst,mydb)
LoadFinHistory("IS","Q",tks,finHst,mydb)

dbDisconnect(mydb)
#---- End of Main -----------------
#----------------------------------
