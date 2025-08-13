#!/bin/sh


set -xue

stations="\
    DENI \
    SILW \
    LBRW \
    LEGW \
    HERO \
    GOLW \
    HOXO \
    BNDW \
"
stations="HMS"
start="01/01/1998"
end="01/01/2020"


for s in $stations; do
    psql -d met -f - -o "$s-Weather.dat" <<EOF
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
WHERE metstation.code = '$s' AND
      methourly.station = metstation.station AND
      methourly.date >= '$start' AND
      methourly.date <= '$end' AND
      methourly.temperature IS NOT NULL AND
      methourly.dewpoint IS NOT NULL AND
      methourly.windspeed IS NOT NULL
ORDER BY methourly.date;

EOF
done
