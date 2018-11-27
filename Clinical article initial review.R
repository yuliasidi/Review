library(dplyr)

all.search <- read.csv("pubmed_result_final.csv")

all.search<-search <- all.search%>%
  mutate(PE.Type.Final1 = case_when(grepl("Pathology|Not in humans", PE.Type.Final) ~ 'Not clinical',
                                    grepl("Paired|Cluster", PE.Type.Final)~ "Clustered/Paired design",
                                    TRUE ~ as.character(PE.Type.Final)))%>%
  mutate(PE.Type.sum = case_when(
    grepl("Abstract|Poster|Bayesian|Cluster|Paired|Not clinical|Meta|Not NI|Not PE|design|stopped|randomized",
          PE.Type.Final1) ~ "Exclude",TRUE ~ as.character(PE.Type.Final1)))

#Overall summary by primary endpoint type
all.search%>%
  group_by(PE.Type.sum)%>%
  summarise(n = n())

#Summary of reasons for excluded studies
all.search%>%
  filter(PE.Type.sum=='Exclude')%>%
  group_by(PE.Type.Final1)%>%
  summarise(n = n())%>%
  arrange(desc(n))

#Summary of primary endpoints for included studies
all.search%>%
  filter(PE.Type.sum!='Exclude')%>%
  group_by(PE.Type.Final1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))%>%
  arrange(desc(n))


