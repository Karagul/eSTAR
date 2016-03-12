#===================================================================================#
#- Historical stock prices & financials from YAHOO finance (GOOGLE alternative)
#- Save S&P500 raw data into eSTAR database
#- Last mod. by Ted, Wed Feb 24 16:34:27 EST 2016
#===================================================================================#
setwd("/apps/fafa/github/eSTAR/scripts")
source("_estar_ut.R")
#library(qmao)

#- Batch job of LoadFinHistory for both A(nnual) and Q(uarterly) reports
#- ja, jb to control DB insert flag from the beginning
LoadRptBatch <- function(tickerNames,mydb,iniFlg) {
	#- Reference ONLY
	#lapply(finHst, function(x) x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",])
	#lapply(finHst, function(x) x$IS$Q["Diluted Normalized EPS", ])
	#lapply(finHst, function(x) x$IS$Q["Dividends per Share - Common Stock Primary Issue", ])

	jaIS=jbIS=iniFlg #- [0|1] for DB [overwrite|append]
	jaBS=jbBS=iniFlg #- [0|1] for DB [overwrite|append]
	jaCF=jbCF=iniFlg #- [0|1] for DB [overwrite|append]
	tG=50   #- elements per group
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
		jaIS=LoadRptHistory("IS","A",tks,finHst,mydb,jaIS)
		jbIS=LoadRptHistory("IS","Q",tks,finHst,mydb,jbIS)
		jaBS=LoadRptHistory("BS","A",tks,finHst,mydb,jaBS)
		jbBS=LoadRptHistory("BS","Q",tks,finHst,mydb,jbBS)
		jaCF=LoadRptHistory("CF","A",tks,finHst,mydb,jaCF)
		jbCF=LoadRptHistory("CF","Q",tks,finHst,mydb,jbCF)
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
LoadRptBatch(tickerNames,mydb,iniFlg)

dbDisconnect(mydb)
#-----      MAIN PROGRAM END      -----#
#===================================================================================#
