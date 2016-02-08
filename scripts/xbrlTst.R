inst <- "http://www.sec.gov/Archives/edgar/data/1223389/000122338914000023/conn-20141031.xml"
options(stringsAsFactors = FALSE)
xbrl.vars <- xbrlDoAll(inst)
xbrl.sec <- xbrlSECdev01(xbrl.vars)
xbrl.sec$showStatements()
