library(RPostgreSQL)
setwd('/apps/fafa/github/eSTAR/scripts')
source("_estar_ut.R")

mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host='bbub2',dbname='eSTAR_2')
xl=dbGetQuery(mydb,'select "ETFName" from description_spdr')
etfLst=xl$ETFName[2:12]
iniFlg=0
for(etfName in etfLst) {
	print(etfName)
	fpName=paste("http://www.sectorspdr.com/sectorspdr/IDCO.Client.Spdrs.Holdings/Export/ExportCsv?symbol=",etfName,sep='')
	xData=read.csv(fpName,skip=1)
	colnames(xData)=sub('X$','ETFName',gsub('[.]','',colnames(xData)))
	xData$ETFName=etfName
	iniFlg=data2dbTbl(mydb, 'spdr_holdings', xData,iniFlg)
}
