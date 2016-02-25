#===================================================================================#
#- Historical stock prices & financials from YAHOO finance (GOOGLE alternative)
#- Save S&P500 raw data into eSTAR database
#- Last mod. by Ted, Wed Feb 24 16:34:27 EST 2016
#===================================================================================#
source("_estar_ut.R")
#library(qmao)

#- Batch job of price quote
LoadDailyBatch <- function(tickerNames,mydb) {
	dstTbl="prc_tmp"
	src="yahoo"
	fromDt=as.Date(Sys.Date()-1)
	iniFlg=0
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
		iniFlg=LoadPrcHistory(tks,mydb,iniFlg,src,dstTbl,fromDt)
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

#---- DB: PostgresSQL
library("RPostgreSQL")
mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')
#---- DB: MySQL
#library(RMySQL)
#mydb<-dbConnect(MySQL(),user='sfdbo',password='sfdbo0',host='localhost',dbname='eSTAR')

#---- RUN BATCH JOBS
LoadDailyBatch(tickerNames,mydb)

dbDisconnect(mydb)
#-----      MAIN PROGRAM END      -----#
#===================================================================================#
