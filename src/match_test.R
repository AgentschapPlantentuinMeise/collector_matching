source("src/matching.R")

resu = list()

for (i in 1:20) {
  resu[[i]] = matchString(dates[i,],
                          wikiResults,
                          aliases)
}
