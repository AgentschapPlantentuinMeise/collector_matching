#faster
library(tidyverse)
library(fst)

write_fst(best_t,"best_meise3.fst") #with updated dwc_agent parser

fst = read_fst("best_naturalist.fst")
fst2 = read_fst("best_meise3.fst")

#probably best to unlist everything each time for caching
#or save as rdata, but this scales poorly as well