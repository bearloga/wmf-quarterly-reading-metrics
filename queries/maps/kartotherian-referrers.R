#!/usr/bin/env Rscript

# Tiles per referrer
# Phabricator: T171531

args = commandArgs(trailingOnly = TRUE)
message("\nFetching tile counts by referrer, etc.\n")
today <- args[1]
suppressPackageStartupMessages({
  library(glue)
})
tsv_path <- glue("data/maps/kartotherian-referrers_{today}.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

query <- "SELECT
    CASE WHEN parse_url(referer, 'HOST') = 'tools.wmflabs.org' THEN 'Wikimedia Labs'
         WHEN parse_url(referer, 'HOST') = 'query.wikidata.org' THEN 'Wikidata Query Service'
         WHEN parse_url(referer, 'HOST') RLIKE '(pkget)|(pkmtracker)|(pogomap)' THEN 'Pokemon Go fansite'
         WHEN referer IS NULL THEN 'Wikimedia Maps'
         WHEN referer_class = 'internal' & parse_url(referer, 'HOST') RLIKE 'wikipedia\\.org$' THEN 'Wikipedia'
         WHEN referer_class = 'internal' & parse_url(referer, 'HOST') RLIKE 'wikivoyage\\.org$' THEN 'Wikivoyage'
         WHEN referer_class = 'internal' THEN 'Other Wikimedia projects'
         ELSE 'Other' END AS referrer,
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
GROUP BY
    CASE WHEN parse_url(referer, 'HOST') = 'tools.wmflabs.org' THEN 'Wikimedia Labs'
         WHEN parse_url(referer, 'HOST') = 'query.wikidata.org' THEN 'Wikidata Query Service'
         WHEN parse_url(referer, 'HOST') RLIKE '(pkget)|(pkmtracker)|(pogomap)' THEN 'Pokemon Go fansite'
         WHEN referer IS NULL THEN 'Wikimedia Maps'
         WHEN referer_class = 'internal' & parse_url(referer, 'HOST') RLIKE 'wikipedia\\.org$' THEN 'Wikipedia'
         WHEN referer_class = 'internal' & parse_url(referer, 'HOST') RLIKE 'wikivoyage\\.org$' THEN 'Wikivoyage'
         WHEN referer_class = 'internal' THEN 'Other Wikimedia projects'
         ELSE 'Other' END
ORDER BY tiles DESC
LIMIT 100;"

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
