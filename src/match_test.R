library(parallel)
library(doParallel)
source("src/matching.R")

num_threads = 8

cl <- makePSOCKcluster(num_threads)
registerDoParallel(cl)
clusterExport(cl, c("wikiResults", "aliases"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(magrittr)
  source("src/matching.R")
})

start_time <- Sys.time()
resu2 = foreach(mat = tryout) %dopar% matchString(mat,
                                                  wikiResults,
                                                  aliases)
end_time <- Sys.time()
stopCluster(cl)

parallel_time <- end_time - start_time
print(parallel_time)

cl <- makePSOCKcluster(num_threads)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(tidyverse)
  library(magrittr)
  source("src/matching.R")
})

start_time <- Sys.time()
results = foreach(mat = resu) %dopar% match_validate(mat)
end_time <- Sys.time()
stopCluster(cl)

best = results %>%
  bind_rows() %>%
  mutate(parsed = dates$parsed[1:length(resu)],
         ori = dates$ori[1:length(resu)])

parallel_time <- end_time - start_time
print(parallel_time) #5213 done in 31s for match_validate with best
