library(duckdb)

# Connect to DuckDB
con <- dbConnect(duckdb(), dbdir = "urgency_db.duckdb")

# Insert data from parquet files into DuckDB tables
# Drop the table if it exists, then create it fresh
dbExecute(con, "DROP TABLE IF EXISTS urgency_data")
dbExecute(
  con,
  "
  CREATE TABLE urgency_data AS
  SELECT * FROM read_parquet('raw-data/at_urg_respiratorio_semanal.parquet')
"
)

tbl(con, "urgency_data") |>
  glimpse()

tbl(con, "urgency_data") |>
  filter(Anio == 2025, !grepl("TOTAL", Causa)) |>
  summarize(n = sum(NumTotal), .by = Causa) |>
  arrange(desc(n))


# Disconnect from DuckDB
dbDisconnect(con, shutdown = TRUE)
