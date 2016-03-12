SELECT h."ETFName" as "ETFName",d."Sector" as "Sector",c."Name", (c."CashfromOperatingActivities"+c."CapitalExpenditures") as "FreeCF", (b."CashandShortTermInvestments") as "ShortTermCash", (b."TotalDebt") as "TotalDebt", (p."Adjusted") as "Price", round((b."TotalCommonSharesOutstanding")) as "TotalShare", round((b."TotalCommonSharesOutstanding" * p."Adjusted")) as "MarketCap", c."pbDate"
FROM
    "holdings_spdr" h, "description_spdr" d,
    "CF_A_hist" c, "BS_A_hist" b,
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
    AND h."ETFName"=d."ETFName"
    AND h."ETFName" LIKE 'XL%'
    AND c."Name" = h."Name"
ORDER BY
    round(c."pbDate"/10000) DESC,"FreeCF" DESC
