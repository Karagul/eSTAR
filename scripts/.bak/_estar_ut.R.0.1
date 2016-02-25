#===================================================================================#
#- Historical stock prices & financials from YAHOO finance (GOOGLE alternative)
#- Save S&P500 raw data into eSTAR database
#- Last mod. by Ted @ Thu Feb 25 13:46:46 EST 2016
#===================================================================================#
library(quantmod)
require(stringr)
library(qmao)

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

#- Write data [dbdata] into Database [dbConn:dstTbl], iniFlg[0|1]:[overwrite|append]
data2dbTbl <- function(dbConn, dstTbl, dbdata,iniFlg) {
#	query <- paste('DELETE FROM',dstTbl)
#	rst <- dbGetQuery(mydb, query)
	if(iniFlg>0) {
		dbWriteTable(dbConn, dstTbl, dbdata, row.names=F, append=T,overwrite=F)
	} else {
		dbWriteTable(dbConn, dstTbl, dbdata, row.names=F, append=F,overwrite=T)
		print(sprintf("%s overwrite[%d]",dstTbl,iniFlg))
	}
	return(iniFlg+1)
}

#- Load price history of tickers [tks] from [src] (default:YAHOO)
#- using insert flag [jx]:[0|1]:[overwrite|append]
LoadPrcHistory <- function(tks,mydb,jx,src,dstTbl,fromDt) {
	tksN=length(tks)
	if(missing(dstTbl)) dstTbl="prc_hist"
	if (!missing(src) && src=="google") {
		dstTbl= paste(dstTbl,src,sep="_")
	} else {
		src="yahoo"
	}
	print(tks)
	if(missing(fromDt)) {
		prcHst <- lapply( tks,function(x) tryCatch(getSymbols(x,auto.assign=FALSE), error=function(e){NULL}) )
	} else {
		print(sprintf("%s %s",src,fromDt))
		prcHst <- lapply( tks,function(x) tryCatch(getSymbols(x,auto.assign=FALSE,from=fromDt), error=function(e){NULL}) )
	}
#	jx=0
	for(j in 1:tksN) {
		if(is.null(prcHst[[j]])) {
			print(sprintf("PRICE N/A: tks[%d] = %s, jx=%d",j,tks[j],jx))
			next
		}
		print(sprintf("tks[%d] = %s, jx=%d",j,tks[j],jx))
		datax=data.frame(prcHst[[j]])
		dataxHdr=str_replace(colnames(datax),paste(tks[j],".",sep=""),"")
		datax <- cleanRawdata(datax,dataxHdr,tks[j])
		jx=data2dbTbl(mydb,dstTbl,datax,jx) 
	}
	return(jx);
}

#- Load dividend history of tickers [tks] 
#- using insert flag [jx]:[0|1]:[overwrite|append]
LoadDvndHistory <- function(tks,mydb,jx) {
	tksN=length(tks)
	dvndHst <- lapply(tks, getDividends)
	dstTbl="dvnd_hist"
#	jx=0
	for(j in 1:tksN) {
		if ( length( dvndHst[[j]] )<1 ) 
			next
		datax=data.frame(dvndHst[[j]])
		dataxHdr=c("Dividend")
		datax <- cleanRawdata(datax,dataxHdr,tks[j])
		sprintf("tks[%d] = %s, jx=%d",j,tks[j],jx)
		jx=data2dbTbl(mydb,dstTbl,datax,jx) 
	}
	return(jx);
}

#- Use view financial object [finHst] of tickers [tks] 
#- save finacial reports [rpt]:[IS|BS|CF] of frequency [fq]:[Q|A] into [mydb:fin_hist_Q/A]
#- using insert flag [jx]:[0|1]:[overwrite|append]
LoadFinHistory <- function(rpt,fq,tks,finHst,mydb,jx) {
	epsM=lapply(finHst, function(x) if(is.null(x)==F) rbind(viewFin(x,rpt,fq)["Diluted Normalized EPS",],viewFin(x,rpt,fq)["Operating Income",] , viewFin(x,rpt,fq)["Total Revenue",] ))
	MQ=NULL
	tksN=length(tks)
	js=-1
	for(j in 1:tksN) {
		if(is.null(epsM[[j]])==T)
			next
		else if(js==-1)
			js=j
		ticker=tks[j]
		eps=t(epsM[[j]])
		m=data.frame(ticker,eps)
		colnames(m)<-c("Ticker","dEPS","Income","Revenue")
		if(j==js)
			MQ=m
		else
			MQ=rbind(MQ,m)
		print(m)
	}
	if(is.null(MQ)==T)
		return(jx)
	print(MQ)
	ymd=as.integer(as.character(as.Date(row.names(MQ),"%Y-%m-%d"),"%Y%m%d"))
	MQ$pbDate=ymd
	if(jx>0)
		dbWriteTable(mydb, paste("fin_hist",fq,sep="_"), MQ, row.names=F, append=T,overwrite=F)
	else
		dbWriteTable(mydb, paste("fin_hist",fq,sep="_"), MQ, row.names=F, append=F,overwrite=T)
	return(jx+length(tks))
}


#- To get list of fields in finacial report [IS|BS|CF] for both [Q|A] frequency
#- Saved in [eSTAR:fin_field]
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

#- Batch job of LoadPrcHistory & LoadDvndHistory
#- ja, jb to control DB insert flag from the beginning
LoadHistoryBatch <- function(tickerNames,mydb) {
	ja=jb=1 #- [0|1] for DB [overwrite|append]
	tG=1    #- elements per group  
	tN=length(tickerNames)
	gN=as.integer(tN/tG)
	if(gN*tG<tN) gN = gN+1
	for(jg in 1:gN) {
		gB=(jg-1)*tG+1
		if(jg==gN)
                        gE = tN
                else
                        gE = gB+tG-1
		tks=tickerNames[gB:gE]
		print(tks)
		ja=LoadPrcHistory(tks,mydb,ja) # ja=LoadPrcHistory(tks,mydb,ja,"google") #= [google] source
		jb=LoadDvndHistory(tks,mydb,jb)
	}
}

#- Batch job of LoadFinHistory for both A(nnual) and Q(uarterly) reports
#- ja, jb to control DB insert flag from the beginning
LoadFinBatch <- function(tickerNames,mydb) {
	#- Reference ONLY
	#lapply(finHst, function(x) x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",])
	#lapply(finHst, function(x) x$IS$Q["Diluted Normalized EPS", ])
	#lapply(finHst, function(x) x$IS$Q["Dividends per Share - Common Stock Primary Issue", ])

	ja=jb=0 #- [0|1] for DB [overwrite|append]
	tG=10   #- elements per group  
	tN=length(tickerNames)
	gN=as.integer(tN/tG)
	if(gN*tG<tN) gN = gN+1
	for(jg in 1:gN) {
		gB=(jg-1)*tG+1
		if(jg==gN)
                        gE = tN
                else
                        gE = gB+tG-1
		tks=tickerNames[gB:gE]
		print(tks)
		#- To load finacial report [IS,BS,CF] for both [Q,A]
		finHst <- lapply(tks, function(x)tryCatch(getFinancials(x,auto.assign=FALSE),error=function(e){NULL}))
		#- To view view & save report [IS,BS,CF] for both [Q,A]
		ja=LoadFinHistory("IS","A",tks,finHst,mydb,ja)
		jb=LoadFinHistory("IS","Q",tks,finHst,mydb,jb)
	}
}

#- Batch job of price quote
#- Get list of today's quotes using src = "yahoo"
estarQuote.yahoo <- function(tickerNames,mydb) {
	dstTbl="prc_quote"
	tG=50     #- elements per group  
	tN=length(tickerNames)
	gN=as.integer(tN/tG)
	if(gN*tG<tN) gN = gN+1
	for(jg in 1:gN) {
		gB=(jg-1)*tG+1
		if(jg==gN)
                        gE = tN
                else
                        gE = gB+tG-1
		tks=tickerNames[gB:gE]
		dbdata=getQuote(paste(tks,collapse = ";"))
		colnames(dbdata)=c('TradeTime','Last','Change','PctChg','Open','High','Low','Volume')
		dbdata$Name=row.names(dbdata)
		row.names(dbdata)= (1:length(dbdata$Last))
		print(tks)
		if(jg>1) {
			dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=T,overwrite=F)
		#	dbWriteTable(mydb, "prc_quote_hist", dbdata, row.names=F, append=T,overwrite=F)
		} else {
			dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=F,overwrite=T)
		}
		print(jg)
	}
}

#- Get list of today's quotes using src = "google"
estarQuote.google <- function(tickerNames,mydb) {
	dstTbl="prc_gquote"
	dbdata=getQuote.google(tickerNames)
	colnames(dbdata)=c('TradeTime','Last','Change','PctChg','Exchange','GoogleID')
	dbdata$Name=row.names(dbdata)
	row.names(dbdata)= (1:length(dbdata$Name))
	dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=F,overwrite=T)
	dstTbl="prc_gquote_hist"
	#dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=T,overwrite=F)
}
