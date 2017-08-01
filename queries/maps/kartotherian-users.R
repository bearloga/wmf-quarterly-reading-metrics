#!/usr/bin/env Rscript

# Tiles per user
# Phabricator: T171531

args = commandArgs(trailingOnly = TRUE)
today <- args[1]

message("\nFetching average tiles per user for the 60 days leading up to ", today, "\n")

suppressPackageStartupMessages({
  library(glue)
})

tsv_path <- glue("data/maps/kartotherian-users_{today}.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

query <- "WITH per_user_counts AS (
  SELECT
    client_ip, user_agent,
    COUNT(1) AS tiles
  FROM wmf.webrequest
  WHERE
    webrequest_source = 'upload'
    AND year = ${year} AND month = ${month} AND day = ${day}
    AND uri_host = 'maps.wikimedia.org'
    AND http_status IN('200', '304')
    AND uri_path RLIKE '^/([^/]+)/([0-9]{1,2})/(-?[0-9]+)/(-?[0-9]+)(@([0-9]\\.?[0-9]?)x)?\\.([a-z]+)$'
    AND uri_query <> '?loadtesting'
    AND REGEXP_EXTRACT(uri_path, '^/([^/]+)/([0-9]{1,2})/(-?[0-9]+)/(-?[0-9]+)(@([0-9]\\.?[0-9]?)x)?\\.([a-z]+)$', 1) != '' -- style
    AND REGEXP_EXTRACT(uri_path, '^/([^/]+)/([0-9]{1,2})/(-?[0-9]+)/(-?[0-9]+)(@([0-9]\\.?[0-9]?)x)?\\.([a-z]+)$', 2) != '' -- zoom
  GROUP BY client_ip, user_agent
)
SELECT
  COUNT(1) AS unique_users,
  SUM(tiles) AS total_tiles,
  PERCENTILE(tiles, 0.5) AS median_tiles,
  PERCENTILE(tiles, 0.99) AS p99_tiles
FROM per_user_counts;"

results <- do.call(rbind, lapply(
  seq(as.Date(today) - 61, as.Date(today) - 1, by = "day"),
  function(date) {
    message("Fetching data from ", format(date, "%Y-%m-%d"))
    year <- lubridate::year(date)
    month <- lubridate::month(date)
    day <- lubridate::mday(date)
    query <- glue(query, .open = "${", .close = "}")
    result <- wmf::query_hive(query)
    return(cbind(date = date, result))
  }
))

readr::write_tsv(results, tsv_path)
message("\nData written to ", tsv_path, "\n")
