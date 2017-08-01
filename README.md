# Quarterly Reading Metrics

Phabricator: [T171528](https://phabricator.wikimedia.org/T171528)

```bash
TODAY=`date +%F`
nice ionice ./main.sh ${TODAY} >> ${TODAY}.log 2>&1
```

Output:

- **data/**: has datasets generated by various queries
- **figures/**: has all the plots generated by [plot.R](plot.R)

## Dependencies

```R
install.packages(
  c("devtools", "tidyverse", "glue"),
  repos = c(CRAN = "https://cran.rstudio.com")
)
devtools::install_git("https://gerrit.wikimedia.org/r/wikimedia/discovery/wmf")
devtools::install_git("https://gerrit.wikimedia.org/r/wikimedia/discovery/polloi")
```

## Wikimedia Maps

- [ ] total article, articles with some sort of map (-link or -frame) and maplink prevalence % vs total articles by top languages
    - [x] data
    - [ ] figure
- [ ] pageviews overall vs pageviews of articles with a map (link or frame) by top languages (use highest prevalence languages)
    - [ ] data
    - [ ] figure
- [ ] tiles served
    - [x] data
    - [ ] figure
- [ ] average tiles per user
    - [x] data
    - [ ] figure
- [ ] pokemonGo users - how much of an impact are they still having on us as usage of the game declines
    - [x] data
    - [ ] figure

## Wikipedia.org Portal

- [ ] Pageviews (percentages of mobile v desktop)
    - [x] data
    - [ ] figure
- [ ] mobile app links (which gets most clicks also mobile vs desktop)
    - [ ] data
    - [ ] figure
- [ ] Bounce rate (mobile vs desktop)
    - [ ] data
    - [ ] figure
- [ ] look to see if there are interesting geographics in usage or bounce rate
    - [ ] data
    - [ ] figure
- [ ] How traffic gets there (direct vs referred)
    - [x] data
    - [ ] figure
- [ ] What sister projects they go to
    - [ ] data
    - [ ] figure

### Mobile vs Desktop

Since there is no "m.wikipedia.org" and, instead, "wikipedia.org" is a responsive page, we must estimate the per-platform breakdown of pageviews via rules on user agents. We can approximate the rules used in Varnish by looking at the top 100 devices, OSes, and browsers that access mobile versions of Wikipedia.

Since OS and browser breakdowns are available via [Pivot](https://pivot.wikimedia.org), we can just get those from there. For devices, a Hive query is required:

```Hive
SELECT
  user_agent_map['device_family'] AS device_family,
  COUNT(1) AS pageviews
FROM wmf.webrequest
WHERE
  webrequest_source = 'text'
  AND year = ${year} AND month = ${month} AND day = ${day} AND hour = ${hour}
  AND is_pageview
  AND agent_type = 'user'
  AND access_method = 'mobile web'
GROUP BY user_agent_map['device_family']
ORDER BY pageviews DESC
LIMIT 100;
```
