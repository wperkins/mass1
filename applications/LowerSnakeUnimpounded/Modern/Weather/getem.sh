#!/bin/sh

set -xue

codes="DENI SILW LBRW LEGW"

for c in $codes; do
    psql -d met -o $c-Weather.dat <<EOF
\pset fieldsep ' '
\pset border off
\pset footer off
\pset tuples_only on
SELECT DISTINCT on (methourly.date)
    to_char(methourly.date, 'MM-DD-YYYY HH24:MI:SS') as date,
    round(methourly.temperature::numeric, 2) as temperature,
    round(methourly.dewpoint::numeric, 2) as dewpoint,
    round(methourly.windspeed::numeric, 2) as windspeed,
    760.0  as pressure,
    round(methourly.radiation::numeric, 2) as radiation,
    '/',
    methourly.station, metstation.code
FROM methourly, metstation
WHERE metstation.code = '$c' AND 
      methourly.station = metstation.station AND
      methourly.date >= '12/31/1999' AND 
      methourly.date <= '01/01/2018' AND
      methourly.temperature IS NOT NULL AND
      methourly.dewpoint IS NOT NULL AND
      methourly.windspeed IS NOT NULL
ORDER BY methourly.date;
EOF
    
done
