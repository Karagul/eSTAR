SELECT h."Name" as "Name",h."ETFName" as "ETFName",d."Sector" as "Sector"
FROM "holdings_spdr" h, "description_spdr" d
WHERE h."ETFName"=d."ETFName"
AND ( h."ETFName" LIKE 'XL%' OR h."ETFName" = 'SPY' )
