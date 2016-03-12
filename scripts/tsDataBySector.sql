SELECT x."pbDate", x."Adjusted" as "X", y."Adjusted" as "Y"
FROM prc_hist x, prc_hist y
WHERE x."Name" = '_xName_' AND y."Name"='_yName_'
AND x."pbDate" = y."pbDate"
AND x."pbDate" > _pbDate_
