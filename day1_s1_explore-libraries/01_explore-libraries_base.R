#' Which libraries does R search for packages?
.libPaths()

#' Installed packages

## use installed.packages() to get all installed packages
pckgs <- as.data.frame(installed.packages())

## how many packages?
nrow(pckgs)

#' Exploring the packages
summary(pkg)

## count some things! inspiration
##   * tabulate by LibPath, Priority, or both
##   * what proportion need compilation?
##   * how break down re: version of R they were built on
table(pkg$Priority)
table(pkg$LibPath)

table(pkg$NeedsCompilation)

table(pkg$Built)

#' Reflections

## reflect on ^^ and make a few notes to yourself; inspiration
##   * does the number of base + recommended packages make sense to you?
##   * how does the result of .libPaths() relate to the result of .Library?

table(pkg$Priority)
pkg[which(pkg$Priority == 'base'), ]
pkg[which(pkg$Priority == 'recommended'), ]

.libPaths() # gives package source available to R version of current session
.Library # gives package source for all R versions installed on a machine 

#' Going further

## if you have time to do more ...

## is every package in .Library either base or recommended?
## study package naming style (all lower case, contains '.', etc
## use `fields` argument to installed.packages() to get more info and use it!
dir(.Library) %in% pkg$Package[which(pkg$Priority %in% c("base", "recommended"))]
