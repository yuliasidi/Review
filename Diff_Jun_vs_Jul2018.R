library(dplyr)

old.search <- read.csv("pubmed_result.csv")
new.search <- read.csv("pubmed_result_jul2018.csv")

new.search <- new.search%>%mutate(EntrezUID = Db)
not.incl <- merge(x = new.search, y = old.search, by = "EntrezUID", all.x = T)

test <- not.incl%>%
  filter(is.na(Title.y))


  