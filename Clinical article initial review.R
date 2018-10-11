library(dplyr)

all.search <- read.csv("pubmed_result_refined.csv")

all.search<-search <- all.search%>%
  mutate(PE.Type.Final1 = case_when(grepl("andomized|Ranomized", PE.Type.Final) ~ 'Not randomized',
                                    grepl("Pathology|Not in humans", PE.Type.Final) ~ 'Not clinical',
                                    grepl("Paired|Cluster", PE.Type.Final)~ "Clustered/Paired design",
                                    TRUE ~ as.character(PE.Type.Final)))%>%
  mutate(PE.Type.sum = case_when(
    grepl("Abstract|Poster|Bayesian|Cluster|Paired|Not clinical|Meta|Not NI|Not PE|design|stopped|randomized",
          PE.Type.Final1) ~ "Exclude",TRUE ~ as.character(PE.Type.Final1)))

all.search%>%
  group_by(PE.Type.sum)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))

sum.type.exc <- all.search%>%
  filter(PE.Type.sum=='Exclude')%>%
  group_by(PE.Type.Final1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))

sum.pe.inc <- all.search%>%
  filter(PE.Type.sum!='Exclude')%>%
  group_by(PE.Type.Final1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))


