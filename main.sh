#!/bin/bash

Rscript queries/portal/pageviews-referrers.R $1
Rscript queries/portal/pageviews-platform.R $1
Rscript queries/maps/prevalence.R $1
Rscript queries/maps/kartotherian-users.R $1
Rscript queries/maps/kartotherian-referrers.R $1
# TODO: pageviews overall vs pageviews of articles with a map (link or frame) by top languages (use highest prevalence languages)

Rscript plot.R $1
