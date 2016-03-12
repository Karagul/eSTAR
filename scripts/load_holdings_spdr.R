library("RPostgreSQL")

#dbTst <- "holdings_spdr"
#dbTst <- "description_spdr"
host <- "localhost"
dbTst <- "details_spdr"
fname <- paste("/apps/fafa/github/eSTAR/scripts/",dbTst,".dat",sep="")
print(fname)
dbData <- read.csv(fname,header=T,sep="\t")
mydb<-dbConnect(dbDriver("PostgreSQL"),user='sfdbo',password='sfdbo0',host=host,dbname='eSTAR')
dbWriteTable(mydb, dbTst, dbData, row.names=F, append=F,overwrite=T)
