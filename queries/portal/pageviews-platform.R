#!/usr/bin/env Rscript

# Wikipedia.org PVs by mobile vs desktop
# Phabricator: T171529

args = commandArgs(trailingOnly = TRUE)
message("\nFetching portal pageviews by platform\n")
today <- args[1]
suppressPackageStartupMessages({
  library(glue)
})
tsv_path <- glue("data/portal/pageviews-platform_{today}.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

query <- "SELECT
  IF((
    user_agent_map['os_family'] IN('Android', 'iOS', 'Firefox OS', 'Chrome OS', 'BlackBerry OS', 'Kindle')
    OR INSTR(user_agent_map['os_family'], 'Phone') > 0
    OR user_agent_map['os_family'] RLIKE '^Symbian'
    OR INSTR(user_agent_map['browser_family'], 'Mobile') > 0
    OR user_agent_map['browser_family'] = 'Opera Mini'
    OR user_agent_map['device_family'] RLIKE '^((HTC)|(Samsung)|(Moto))'
    OR user_agent_map['device_family'] RLIKE '([pP]hone)|([tT]ablet)|(Lumia)'
  ), 'mobile', 'desktop') AS device,
  COUNT(1) AS pageviews
FROM wmf.webrequest
WHERE
  webrequest_source = 'text'
  AND year = ${year} AND month = ${month} AND day = ${day}
  AND uri_host RLIKE('^(www\\.)?wikipedia.org/*$')
  AND INSTR(uri_path, 'search-redirect.php') = 0
  AND content_type RLIKE('^text/html')
  AND NOT (referer RLIKE('^http://localhost'))
  AND agent_type = 'user'
  AND referer_class != 'unknown'
  AND http_status IN('200', '304')
GROUP BY
  IF((
    user_agent_map['os_family'] IN('Android', 'iOS', 'Firefox OS', 'Chrome OS', 'BlackBerry OS', 'Kindle')
    OR INSTR(user_agent_map['os_family'], 'Phone') > 0
    OR user_agent_map['os_family'] RLIKE '^Symbian'
    OR INSTR(user_agent_map['browser_family'], 'Mobile') > 0
    OR user_agent_map['browser_family'] = 'Opera Mini'
    OR user_agent_map['device_family'] RLIKE '^((HTC)|(Samsung)|(Moto))'
    OR user_agent_map['device_family'] RLIKE '([pP]hone)|([tT]ablet)|(Lumia)'
  ), 'mobile', 'desktop');"

results <- do.call(rbind, lapply(
  seq(as.Date(today) - 1, as.Date(today) - 1, by = "day"),
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
