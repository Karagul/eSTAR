# hist info to prc_hist
#- Save S&P500 daily price raw data to eSTAR database
#-------------------------------------
library(RMySQL)
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
LoadPrcHistory <- function(tks,mydb) {
	tksN=length(tks)
	prcHst <- lapply(tks, getSymbols, auto.assign=FALSE)
	dstTbl="prc_hist"
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

#----------------------------------
#- Main Program

#----- SET TICKERS
tks <- c("AAPL","GOOG","AMZN","MSFT","IBM","INTC","GRMN","NFLX","UTX","VZ","GE","JPM","GS","NKE","LB","CMG","PNRA","KO","PG","DIS","JNJ","MRK","PFE","XOM","GWW","UHAL","TSM","SPY","XLK")

options("getSymbols.warning4.0"=FALSE)
mydb=dbConnect(MySQL(),user='sfdbo',password='sfdbo0',host='localhost',dbname="eSTAR")
LoadPrcHistory(tks,mydb)
LoadDvndHistory(tks,mydb)
dbDisconnect(mydb)

#---- End of Main -----------------
#----------------------------------

#- Reference ONLY
#date.be <- c("2000-01-01", "2015-01-01")
#finHst <- lapply(tks, getFinancials, auto.assign=FALSE)
#lapply(finHst, function(x) x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",])
#lapply(finHst, function(x) x$IS$Q["Diluted Normalized EPS", ])
#lapply(finHst, function(x) x$IS$Q["Dividends per Share - Common Stock Primary Issue", ])
