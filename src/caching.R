#is quite slow
library(jsonlite)
new = fromJSON("matching_results_dashfix.json")
save = write(toJSON(old,auto_unbox = T),"filename.json")

#faster
library(tidyverse)
library(fst)

write_fst(best_t,"best_naturalist.fst")

fst = read_fst("best_naturalist.fst")

#probably best to unlist everything each time for caching
#or save as rdata, but this scales poorly as well