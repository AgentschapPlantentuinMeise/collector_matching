library(tidyverse)
library(fst)

write_fst(best_t,"best_meise3.fst") #with updated dwc_agent parser
write_fst(best_t,"best_naturalist2.fst")
write_fst(best_t2,"best_germany.fst")

fst = read_fst("best_naturalist2.fst")
fst2 = read_fst("best_meise3.fst")