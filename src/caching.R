#faster
library(tidyverse)
library(fst)

write_fst(best_t,"best_meise2.fst")

fst = read_fst("best_naturalist.fst")
fst2 = read_fst("best_meise2.fst")

#probably best to unlist everything each time for caching
#or save as rdata, but this scales poorly as well