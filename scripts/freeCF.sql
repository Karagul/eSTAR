SELECT c."Name",avg(c."CashfromOperatingActivities"+c."CapitalExpenditures") as "FreeCF",avg(b."CashandShortTermInvestments") as "ShortTermCash",avg(b."TotalDebt") as "TotalDebt",avg(p."Adjusted") as "Price", avg((b."TotalCommonSharesOutstanding")) as "TotalShare", avg((b."TotalCommonSharesOutstanding" * p."Adjusted")) as "MarketCap"
FROM "CF_A_hist" c, "BS_A_hist" b,
(
  SELECT "Name","yyyy",avg("Adjusted") as "Adjusted"
  FROM
  (
    SELECT round("pbDate"/10000) as "yyyy", "Name", "Adjusted"
    FROM "prc_hist"
    order by "pbDate"
  ) m
  GROUP BY "Name","yyyy"
  ORDER BY "yyyy" 
) p
WHERE
c."pbDate"=b."pbDate"
AND c."Name" = b."Name"
AND c."Name" = p."Name"
AND round(c."pbDate"/10000) = p."yyyy"
AND c."CapitalExpenditures" IS NOT NULL
GROUP BY c."Name"
ORDER BY "FreeCF" DESC
