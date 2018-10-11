# Random sample of 5 artcles out of 335 original 
sample(1:335,5)

13  21 133 278 257

all.search <- read.csv("pubmed_result.csv")

ofer5 <- all.search%>%filter(Article..==13|Article..==21|Article..==133|Article..==278|Article..==257)%>%
  rename(ID = Article..)%>%select(ID, PE.Type, PE.Type.Final)
ofer5 
texPreview::texPreview(xtable(ofer5))


#Random sample of 5 article out of binomial proportions
sample(1:81, 5)

73 39 31 64 67