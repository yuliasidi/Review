# Random sample of 5 artcles out of 335 original 
sample(1:335,5)
#13 21 133 278 257

all.search <- read.csv("pubmed_result_refined.csv")

ofer5 <- all.search%>%filter(Article..==13|Article..==21|Article..==133|Article..==278|Article..==257)%>%
  rename(ID = Article..)%>%select(ID, PE.Type, PE.Type.Final)
ofer5 
texPreview::texPreview(xtable(ofer5))


#Random sample of 5 article out of binomial proportions
set.seed(45187)
sample(1:71, 5)
#7 44 26 47 59

pe.res <- read.csv("Proportions PE Methods in practice refined.csv")

pe.res.ofer5 <- pe.res%>%
  dplyr::mutate(i = 1:n())%>%
  dplyr::filter(i%in%c(7, 44, 26, 47, 59))
