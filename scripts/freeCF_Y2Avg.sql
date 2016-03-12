SELECT "ETFName",max("Sector") as "Sector","Name",avg("FreeCF") as "FreeCF",avg("ShortTermCash") as "ShortTermCash", avg("TotalDebt") as "TotalDebt",avg("Price") as "Price",avg("TotalShare") as "TotalShare",avg("MarketCap") as "MarketCap" 
FROM "free_cf_yyyy"
GROUP by "ETFName","Name"
ORDER BY "ETFName","Name"
