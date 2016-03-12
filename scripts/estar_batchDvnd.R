#===================================================================================#
#- Historical stock dividends & financials from YAHOO finance 
#- Save S&P500 raw data into eSTAR database
#- Last mod. by Ted, Wed Mar  9 17:44:12 EST 2016
#===================================================================================#
setwd("/apps/fafa/github/eSTAR/scripts")
source("_estar_ut.R")
#library(qmao)

#- Batch job of price quote
LoadDvndBatch <- function(tickerNames,mydb,iniFlg) {
	if(missing(iniFlg)) iniFlg=0
	dstTbl="dvnd_hist"
	src="yahoo"
	fromDt=as.Date(Sys.Date()-1)
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
		iniFlg=LoadDvndHistory(tks,mydb,iniFlg,dstTbl)
	}
}

#===================================================================================#
#-----      MAIN PROGRAM BEGIN      -----#

#----- SET OPTIONS
options(download.file.method="wget")
options("getSymbols.warning4.0"=FALSE)

#----- SET TICKERS
tickerNames <- readLines("spyLst.dat")
#tickerNames_g <- readLines("spyLst_g.dat")
#tickerNames = tickerNames[500:524] # TEMP INSERT TEST
#tickerNames=c("AWK","CXO","EXR","FRT","CFG","UDR")

#---- DB: PostgresSQL
library("RPostgreSQL")
mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')
#---- DB: MySQL
#library(RMySQL)
#mydb<-dbConnect(MySQL(),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')

#---- RUN BATCH JOBS
iniFlg=0
LoadDvndBatch(tickerNames,mydb,iniFlg)

dbDisconnect(mydb)
#-----      MAIN PROGRAM END      -----#
#===================================================================================#
