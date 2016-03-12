SELECT h."ETFName" as "ETFName",d."Sector" as "Sector",f.*
FROM "holdings_spdr" h, "description_spdr" d , free_CF f
WHERE h."ETFName"=d."ETFName"
AND h."ETFName" LIKE 'XL%'
AND f."Name" = h."Name"
