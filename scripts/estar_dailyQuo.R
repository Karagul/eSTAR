#!/usr/bin/env Rscript
#===================================================================================#
#- Historical stock prices & financials from YAHOO finance (GOOGLE alternative)
#- Save S&P500 raw data into eSTAR database
#- Last mod. by Ted, Wed Feb 24 16:34:27 EST 2016
#===================================================================================#
setwd("/apps/fafa/github/eSTAR/scripts")
library(quantmod)
library(qmao)
require(stringr)

#- Batch job of price quote
#- Get list of today's quotes using src = "yahoo"
estarQuote.yahoo <- function(tickerNames,mydb) {
	dstTbl="prc_quote"
	tG=100     #- elements per group  
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
		} else {
			dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=F,overwrite=T)
		}
		print(jg)
	}
	rst <- dbGetQuery(mydb, 'DELETE FROM "prc_quote" WHERE "Low" = \'N/A\'')
	rst <- dbGetQuery(mydb, 'INSERT INTO "prc_quote_hist" SELECT * from "prc_quote"')
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
	rst <- dbGetQuery(mydb, 'DELETE FROM "prc_gquote" WHERE "PctChg" IS NULL')
	dbWriteTable(mydb, dstTbl, dbdata, row.names=F, append=T,overwrite=F)
}

#===================================================================================#
#-----      MAIN PROGRAM BEGIN      -----#

#----- SET OPTIONS
options(download.file.method="wget")
options("getSymbols.warning4.0"=FALSE)

#----- SET TICKERS
tickerNames <- readLines("spyLst.dat")
tickerNames_g <- readLines("spyLst_g.dat")
#tickerNames = tickerNames[500:524] # TEMP INSERT TEST
#tickerNames <- readLines("/apps/fafa/github/eSTAR/scripts/spyLst.dat")

#---- DB: PostgresSQL
library("RPostgreSQL")
mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')
#---- DB: MySQL
#library(RMySQL)
#mydb<-dbConnect(MySQL(),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')

#---- RUN BATCH JOBS
estarQuote.yahoo(tickerNames,mydb)
estarQuote.google(tickerNames_g,mydb)

dbDisconnect(mydb)
#-----      MAIN PROGRAM END      -----#
#===================================================================================#
