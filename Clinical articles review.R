library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(xtable, warn.conflicts = FALSE, quietly = TRUE)
library(reshape2, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)

pe <- readr::read_csv("Proportions PE Methods in practice final.csv")

pe <- pe%>%
  mutate(SS.calc1 = case_when(SS.calc == "Newcomb-Wilson (5000 simulations)" ~ "Newcombe",
                              SS.calc == "Wilson" ~ "Newcombe",
                              SS.calc == "Z-Test" ~ "Wald",
                              is.na(SS.calc)==T ~ "Not reported",
                              SS.calc == "FM" ~ "Farrington-Manning",
                              TRUE ~ as.character(SS.calc)))%>%
  mutate(anal.CI1 = case_when(anal.CI == "Not specified" ~ "Not specified, but presented",
                              grepl("Chi|Pearson or",anal.CI) ~ "Not specified, but presented",
                              grepl("Exact",anal.CI) ~ "Exact",
                              grepl("Newcombe|Wilson",anal.CI) ~ "Newcombe",
                              grepl("Miettinen and Nurminen|FM",anal.CI) ~ "Farrington-Manning",
                              grepl("GLM|Linear mixed model",anal.CI) ~ "GLM",
                              grepl("Z-test|Wald|Blackwelder",anal.CI) ~ "Wald",
                              grepl("MH",anal.CI) ~ "CMH/MH",
                              TRUE ~ as.character(anal.CI)))%>%
  mutate(anal.CI2 = case_when(grepl("Agresti|Profile|average|TTEST",anal.CI1) ~ "Other",
                              TRUE ~ as.character(anal.CI1)))%>%
  mutate(anal.p1 = case_when(grepl("Fisher|Chi",anal.p) ~ "Fisher's Exact/Chi-Square",
                             grepl("GLM|Linear mixed model",anal.p) ~ "GLM",
                             grepl("Wald|Z-test|Blackwelder",anal.p) ~ "Wald",
                             #anal.p=="FM" ~ "Farrington-Manning",
                             grepl("CI|FM",anal.p) ~ "Other",
                             TRUE ~ as.character(anal.p)))%>%
  mutate(M2.1 = case_when(M2 <= 0.025              ~ "0   < M <= 2.5%"  ,
                          0.025 < M2 & M2 <= 0.05  ~ "2.5 < M <= 5%"    ,
                          0.05  < M2 & M2 <= 0.075 ~ "5   < M <= 7.5%"  ,
                          0.075 < M2 & M2 <= 0.1   ~ "7.5 < M <= 10%"   ,
                          0.01  < M2 & M2 <= 0.125 ~ "10  < M <= 12.5%" ,
                          0.125 < M2 & M2 <= 0.15  ~ "12.5< M <= 15%"   ,
                          0.15  < M2 & M2 <= 0.175 ~ "15  < M <= 17.5%" ,
                          0.175 < M2 & M2 <= 0.2   ~ "17.5< M <= 20%"   ,
                          0.2   < M2 & M2 <= 0.225 ~ "20  < M <= 22.5%" ,
                          0.225 < M2 & M2 <= 0.25  ~ "22.5< M <= 25%"   ,
                          0.25  < M2              ~ ">25%",
                          is.na(M2)==T~ "Not reported"))%>%
  mutate(M2.2 = case_when(M2 <= 0.05              ~ " M <= 5%"  ,
                          0.05  < M2 & M2 <= 0.075 ~ "5%     < M <= 7.5%"  ,
                          0.075 < M2 & M2 <= 0.1   ~ "7.5% < M <= 10%"   ,
                          0.01  < M2 & M2 <= 0.15  ~ "10%  < M <= 15%" ,
                          0.125 < M2 & M2 <= 0.15  ~ "12.5%< M <= 15%"   ,
                          0.15  < M2 & M2 <= 0.2   ~ "15%  < M <= 20%" ,
                          0.2   < M2 & M2 <= 0.25 ~  "20%  < M <= 25%", 
                          #0.25  < M2              ~ ">25%",
                          is.na(M2)==T~ "Not reported/Other",
                          TRUE ~ "Not reported/Other"))%>%
  # One margin was reported as 0.7 which was cut-off point for RR (article 114)
  mutate(p_C1 = case_when(S_F.rate=="F" ~ 1-p_C,
                          TRUE ~ p_C))%>%
  mutate(p_C2 = case_when(is.na(p_C1)==TRUE ~ "Not reported",
                          p_C1 <= 0.6               ~ "P <= 0.6"  ,
                          0.6 < p_C1 & p_C1 <= 0.7  ~ "0.6 < P <= 0.7"  ,
                          0.7 < p_C1 & p_C1 <= 0.8  ~ "0.7 < P <= 0.8"  ,
                          0.8 < p_C1 & p_C1 <= 0.9  ~ "0.8 < P <= 0.9"   ,
                          0.9 < p_C1 & p_C1 <= 0.95 ~ "0.9 < P <= 0.95" ,
                          0.95< p_C1 ~ "P > 0.95"))%>%
  mutate(DO1 = case_when(is.na(DO)==TRUE ~ "Not reported",
                          DO <= 0.1              ~ "DO <= 10%"  ,
                          0.1 < DO & DO <= 0.15  ~ "10% < DO <= 15%"  ,
                          0.15< DO & DO <= 0.2   ~ "15% < DO <= 20%"   ,
                          0.2 < DO ~ "DO > 20%"))%>%
  mutate(Power1 = case_when(is.na(Power)==TRUE ~ "Not reported",
                            Power==0.80~ "80%",
                            Power==0.90~ "90%",
                            TRUE~ "Other"))%>%
  mutate(p_T1 = case_when(is.na(p_T)==T ~ "Not reported",
                          p_T==p_C ~"Same as for standard treatment",
                          TRUE ~ "Other"))%>%
  mutate(Ni.decision1 = case_when(grepl("CI or|CI/",Ni.decision) ~ "CI/p-value",
                                  TRUE ~ as.character(Ni.decision)))%>%
  mutate(Alpha.one.s1 = case_when(Alpha.one.s==0.025 ~ "2.5%",
                                  Alpha.one.s==0.05 ~ "5%",
                                  is.na(Alpha.one.s)==T ~ "Not reported",
                                  TRUE ~ "Other"))%>%
  mutate(set = case_when(is.na(`Analysis Set`)==T ~ "Not reported",
                         `Analysis Set`=="Not specified" ~ "Not reported",
                         grepl('ITT/PP|mITT/PP|FAS/PP|PP/ITT', `Analysis Set`) ~ 'ITT/PP',
                         grepl('ITT|mITT|FAS', `Analysis Set`) ~ "ITT",
                         grepl('PP|ATP', `Analysis Set`) ~ "PP",
                         TRUE ~ "Other"))%>%
  mutate(Miss.report1 = case_when(Miss.report=="Y" ~ "Yes",
                                  TRUE ~ "No"))%>%
  mutate(miss.perc = case_when(`Missing % for rand.`<=0.05 ~ "DO <= 5%",
                               0.05 <`Missing % for rand.` & `Missing % for rand.`<=0.10 ~ "5%  < DO <= 10%",
                               0.10 <`Missing % for rand.` & `Missing % for rand.`<=0.15 ~ "10% < DO <= 15%",
                               0.15 <`Missing % for rand.` & `Missing % for rand.`<=0.20 ~ "15% < DO <= 20%",
                               0.20 <`Missing % for rand.`  ~ "DO > 20%"))%>%
  mutate(Miss.ass1 = case_when(is.na(Miss.ass)==T ~ "Not reported",
                               TRUE  ~ "Reported"))%>%
  mutate(Miss.anal1 = case_when(is.na(Miss.anal)==T ~ "Not specified",
                                Miss.anal=="NI" ~ "Not specified",
                                Miss.anal=="LOCF" ~ "Last observation carried forward",
                                grepl('failure', Miss.anal) ~ "Missing assigned as failure",
                                Miss.anal=="MI" ~ "Multiple-imputation",
                                Miss.anal=="CCA" ~ "Complete case analysis"))%>%
  mutate(miss.sensyn = case_when(is.na(`Sens for miss`)==T ~ "Not reported",
                               `Sens for miss`=="N" ~ "Not reported",
                               TRUE ~ "Reported"))%>%
  mutate(miss.sens = case_when(is.na(`Sens for miss`)==T ~ "Not reported",
                               `Sens for miss`=="N"~ "Not reported",
                              grepl("worst|best|failure|LOCF",`Sens for miss`) ~ "Single imputation",
                              grepl("As treated|Y- Diff|PP|Y- diff|FAS|ITT|set|CC|population",`Sens for miss`) ~ "Different analysis set",
                              grepl("MICE",`Sens for miss`) ~ "Mutliple imputation with chained equations",
                              grepl("tipping point", `Sens for miss`) ~ "Tipping point analysis"))%>%
  mutate(alloc = case_when(Allocation=='1:1'       ~ 'Equal allocation',
                         Allocation=='1:1:1'     ~ 'Equal allocation',
                         Allocation=='1:1:1:1'   ~ 'Equal allocation',
                         Allocation=='1:1:1:1:1' ~ 'Equal allocation',
                         Allocation=='1:1:1:1:1:1' ~ 'Equal allocation',
                         TRUE ~ 'Other'))

####################################################
### additional modification following SBR review ###
####################################################

#article 262.2 is excluded, the paper contains two clinical trials: NI and superiority
pe <- pe %>%
  dplyr::filter(id !=262.2)

pe <- pe %>%
  dplyr::mutate(m2_rel = (p_C1 - M2)/p_C1,
                m2_rel_new = case_when(0.6 < m2_rel & m2_rel <= 0.7 ~ '60-70%',
                                       0.7 < m2_rel & m2_rel <= 0.8 ~ '70-80%',
                                       0.8 < m2_rel & m2_rel <= 0.9 ~ '80-90%',
                                       0.9 < m2_rel  ~ '90% and above',
                                       M2 == 0.7 ~ 'Not reported/Other',
                                       is.na(m2_rel) ~ 'Not reported/Other'),
                M2_just_new = case_when(M2_just %in% c('clinical discussions',
                                              'expert consensus group meeting',
                                              'experts survey',
                                              'group of clinicians',
                                              "investigator's judgement") ~
                                      'Clinical experts',
                                    M2_just %in% c('FDA guidline',
                                                   'Regulatory guideline') ~
                                      'Regulatory guideline',
                                    is.na(M2_just) ~ 'Not reported/Other',
                                    M2 == 0.7 ~ 'Not reported/Other',
                                    M2_just == 'no' ~ 'No justification',
                                    M2_just %in% c('past studies', 'based on past litrature') ~ 'Past studies',
                                    M2_just == 'new treatment benefits' ~ 'New treatment benefits',
                                    M2_just == 'field consideration' ~ 'Field consideration'),
                TA_new = case_when(TA == 'cancer' ~ 'Cancer',
                                   TA %in% c('cardiovascular',
                                   'thrombosis') ~ 'Cardiology',
                                   TA %in% c('infection', 'hiv', 'malaria',
                                             'vaccine', 'tb') ~ 
                                     'Infectious disease and vaccines',
                                   TA %in% c('dermathoogy/pediatrics',
                                             'gynecology') ~ 
                                     'Obstetrics, gynecology, and urology',
                                   TA == 'gastro' ~ 'Gastroenterology',
                                   TA %in% c('stress urinary incontinence',
                                             'urinary tract infection',
                                             'kidneys', 'liver') ~ 'Other internal medicine',
                                   #TA == 'surgery preparation' ~ 'Surgery',
                                   TA %in% c('ra', 'autoimmune') ~ 'Autoimmune	disease',
                                  TRUE ~ 'Other'),
                sponsor_new = case_when(sponsor == 'pharma' ~ 'Pharmacutical company',
                                        is.na(sponsor) ~ 'Not reported',
                                        TRUE ~ 'Government/Research grants'),
                type_new = case_when(type == 'treatment' ~ 'Drug',
                                     type == 'device' ~ 'Device',
                                     type == 'vaccine' ~ 'Vaccine',
                                     type == 'treatment strategy' ~ 'Treatment strategy',
                                     type == 'surgical intervention' ~ 'Surgical intervention',
                                     TRUE ~ 'Other'))




#saveRDS(pe,"pe.rds")                         
# Summary of PE type (Difference, ratio, odds ratio)
pe%>%
  group_by(PE.type)%>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"))
# 90% used proportion difference, report the following results only for that.

#number of papers with protocols
pe %>%
  group_by(protocol) %>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"))

pe %>%
  group_by(sponsor_new) %>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"))


###########
# Table 1 #
###########

# Summary of methods for SS calculation
ss.method <- pe%>%
  group_by(SS.calc1)%>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"))

ss.method <- ss.method%>% dplyr::mutate(
  m = rep("Sample size method",length(n)),
  id = c(2,3,4,1)
)%>%
  dplyr::arrange(id)%>%
  dplyr::select(m,SS.calc1,n,p)%>%
  rename(var = SS.calc1)
#texPreview::texPreview(xtable(ss.method))

# Summary of margins used, when PE type is proportions difference
anal.M22 <- pe%>%
  group_by(M2.2)%>%
  summarise(n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"))%>%
  arrange(M2.2)

anal.M22 <- anal.M22%>%mutate(
  m = rep("Margin", length(n)),
  id = c(1,4,5,6,2,3,7)
)%>%
  arrange(id)%>%
  select(m,M2.2, n,p)%>%
  rename(var = M2.2)
#texPreview::texPreview(xtable(anal.M22))

#relative margin
rel.m2 <- pe %>%
  group_by(m2_rel_new) %>%
  summarise(n=n()) %>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"),
         m = 'Margin relative to the standard treatment')%>%
  rename(var = m2_rel_new)%>%
  select(m, var, n, p)

#determination of m2
det.m2 <- pe %>%
  group_by(M2_just_new) %>%
  summarise(n=n()) %>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"),
         id = c(3,6,5,1,7,2,4),
         m = 'Margin justification')%>%
  arrange(id)%>%
  rename(var = M2_just_new)%>%
  select(m, var, n, p)

#TA
ta <- pe %>%
  group_by(TA_new) %>%
  summarise(n=n()) %>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"),
         id = c(6,3,2,4,1,5,8,7),
         m = 'Therapeutic area')%>%
  arrange(id)%>%
  rename(var = TA_new)%>%
  select(m, var, n, p)

# what is being investigated
trt_type <- pe %>%
  group_by(type_new) %>%
  summarise(n=n()) %>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"),
         id = c(3,1,6,4,5,2),
         m = 'Treatment type')%>%
  arrange(id)%>%
  rename(var = type_new)%>%
  select(m, var, n, p)

anal.p_C2 <- pe%>%
  group_by(p_C2)%>%
  summarise(n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"))

anal.p_C2 <- anal.p_C2%>%mutate(
  m = rep("Event proportion- standard treatment",length(n)),
  id= c(2,3,4,5,7,1,6) 
)%>%
  arrange(id)%>%
  select(m, p_C2,n,p)%>%
  rename(var = p_C2)
#texPreview::texPreview(xtable(anal.p_C2))


anal.DO <- pe%>%
  group_by(DO1)%>%
  summarise(n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0), "%"))

anal.DO <- anal.DO%>%dplyr::mutate(
  m = rep("Drop-out rate",length(n)),
  id = c(2,3,1,4,5)
)%>%
  dplyr::arrange(id)%>%
  dplyr::select(m,DO1,n,p)%>%
  rename(var = DO1)
#texPreview::texPreview(xtable(anal.DO))


#p_T reporting
anal.p_T <- pe%>%
  group_by(p_T1)%>%
  summarise(n = n())%>%
  mutate(p = paste0(round(100 * n/sum(n), 0), "%"))

anal.p_T <- anal.p_T%>%mutate(
  m = rep("Event proportion- new treatment",length(n)),
  id = c(3,2,1)
)%>%
  arrange(id)%>%
  select(m,p_T1,n,p)%>%
  rename(var = p_T1)
#texPreview::texPreview(xtable(anal.p_T))


# One-sided alpha level summary
alpha.one <- pe%>%
  group_by(Alpha.one.s1)%>%
  summarise(n = n())%>%
  mutate(p = paste0(round(100 * n/sum(n), 0), "%"))

alpha.one <- alpha.one%>%mutate(
  m = rep("Type-I error", length(n)),
  id = c(1,2,3)
)%>%
  arrange(id)%>%
  select(m, Alpha.one.s1, n, p)%>%
  rename(var = Alpha.one.s1)
#texPreview::texPreview(xtable(alpha.one))

# Power level summary
pow1 <- pe%>%
  group_by(Power1)%>%
  summarise(n = n())%>%
  mutate(p = paste0(round(100 * n/sum(n), 0), "%"))

pow1 <- pow1%>%mutate(
  m = rep("Power",length(n)),
  id = c(1,2,4,3)
)%>%
  arrange(id)%>%
  select(m, Power1, n, p)%>%
  rename(var = Power1)
#texPreview::texPreview(xtable(pow1))

#Allocation ratio
allr <- pe%>%
  group_by(alloc)%>%
  summarise(n = n())%>%
  mutate(p = paste0(round(100 * n/sum(n), 0), "%"))

allr <- allr%>%mutate(
  m = rep("Group allocation",length(n)),
  id = c(1,2)
)%>%
  arrange(id)%>%
  select(m, alloc, n, p)%>%
  rename(var = alloc)
#texPreview::texPreview(xtable(allr))

# COMBINE THE ABOVE INTO ONE TABLE
table1 <- ss.method%>%bind_rows(anal.M22, rel.m2, det.m2, ta, trt_type,
                                anal.p_C2, anal.p_T, anal.DO,
                                alpha.one, pow1, allr)

myfun <- function(x) gsub('\\\\&','&',x)  

table1%>%
pixiedust::dust(longtable = TRUE)%>%
  pixiedust::sprinkle_border(rows = c(1,5,12,17,24,32,38,45,48,53,56,60),
                             border = c('top')
                             )%>%
  pixiedust::sprinkle_border(rows = c(47),
                             border = c('bottom')
  )%>%
  pixiedust::sprinkle_colnames(m='',var='',p='%')%>%
  pixiedust::sprinkle_caption('Study design attributes (n=70)')%>%
  pixiedust::sprinkle_label('tab1')%>%
  pixiedust::sprinkle(rows = 1:4,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 5:11,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 12:16,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 17:23,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 24:31,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 32:37,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 38:44,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 45:47,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 48:52,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 53:55,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 56:59,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 60:61,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle_print_method('latex')%>%
  print()%>%
  pixiedust::sanitize_latex()%>%
  myfun%>%
  cat(file = 'Review Paper/table1.tex')

###########
# Table 2 #
###########

#NI decision
NI.dec <- pe%>%
  group_by(Ni.decision1)%>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"))

NI.dec <- NI.dec%>%mutate(
  m = rep("NI decision", length(n)),
  id = c(1,3,2)
)%>%
  rename(var = Ni.decision1)%>%
  arrange(id)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(NI.dec))

# Summary CI methods used in analysis
anal.CI.sum <- pe%>%
  group_by(anal.CI2)%>%
  summarise (n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n), 0), "%"),p0 = n/sum(n))%>%
  arrange(desc(p0))%>%
  select(-p0)

anal.CI.sum <- anal.CI.sum%>%mutate(
  m = rep("CI method",length(n))
)%>%
  rename(var= anal.CI2)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(anal.CI.sum))

# Summary p-value methods in analysis
anal.p.sum <- pe%>%
  group_by(anal.p1)%>%
  summarise(n=n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"), p0=n/sum(n))%>%
  arrange(desc(p0))%>%
  select(-p0)

anal.p.sum <- anal.p.sum%>%mutate(
  m = rep("P-value method", length(n)),
  id = c(7,1,2,3,4,5,6)
)%>%
  arrange(id)%>%
  rename(var = anal.p1)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(anal.p.sum))

#Analysis set
anal.set <-pe%>%
  group_by(set)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
anal.set <- anal.set%>%mutate(
  m = rep("Analysis set",length(n)),
  id = c(1,3,4,2)
)%>%
  rename(var = set)%>%
  arrange(id)%>%
  select(m, var, n, p)

#texPreview::texPreview(xtable(anal.set))

#Reporting any missing data
miss.rep <- pe%>%
  group_by(Miss.report1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
miss.rep <- miss.rep%>%mutate(
  m = rep("Reporting any missing data for primary analysis", length(n)),
  id = c(2,1)
)%>%
  rename(var = Miss.report1)%>%
  arrange(id)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(miss.rep))  


table2 <- NI.dec%>%bind_rows(anal.CI.sum, anal.p.sum, anal.set, miss.rep)

table2%>%
  pixiedust::dust()%>%
  pixiedust::sprinkle_border(rows = c(1,4,14,21,25),
                             border = c('top')
  )%>%
  pixiedust::sprinkle_border(rows = c(26),
                             border = c('bottom')
  )%>%
  pixiedust::sprinkle_colnames(m='',var='',p='%')%>%
  pixiedust::sprinkle_caption('Study analysis attributes (n=70)')%>%
  pixiedust::sprinkle_label('tab2')%>%
  pixiedust::sprinkle(rows = 1:3,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 4:13,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 14:20,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 21:24,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 25:26,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle_print_method('latex')%>%
  print()%>%
  pixiedust::sanitize_latex()%>%
  myfun%>%
  cat(file = 'Review Paper/table2.tex')

###########
# Table 3 #
###########

# % missing
miss.perc <- pe%>%filter(Miss.report1=="Yes")%>%
  group_by(miss.perc)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
miss.perc <- miss.perc%>%mutate(
  m = rep("% of missing", length(n)),
  id = c(3,4,2,1,5)
)%>%
  rename(var = miss.perc)%>%
  arrange(id)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(miss.perc))  

# Missingness assumpion
anal.miss.ass <- pe%>%filter(Miss.report1=="Yes")%>%
  group_by(Miss.ass1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
anal.miss.ass <- anal.miss.ass%>%mutate(
  m = rep("Missingness assumption", length(n)),
  id = c(2,1)
)%>%
  rename(var = Miss.ass1)%>%
  arrange(id)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(anal.miss.ass))  

# Incomplete data analysis
anal.miss.anal <- pe%>%filter(Miss.report1=="Yes")%>%
  group_by(Miss.anal1)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
anal.miss.anal <- anal.miss.anal%>%mutate(
  m = rep("Incomplete data handling in primary analysis", length(n)),
  id = c(2,3,1,4,5)
)%>%
  rename(var = Miss.anal1)%>%
  arrange(id)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(anal.miss.anal))  

# Sensitivity for missing
anal.miss.sens <- pe%>%filter(Miss.report1=="Yes")%>%
  group_by(miss.sens)%>%
  summarise(n = n())%>%
  mutate(p=paste0(round(100 * n/sum(n),0),"%"))
anal.miss.sens <- anal.miss.sens%>%mutate(
  m = rep("Sensitivity analysis for incomplete data", length(n)),
  id = c(2,4,5,1,3)
)%>%
  arrange(id)%>%
  rename(var = miss.sens)%>%
  select(m, var, n, p)
#texPreview::texPreview(xtable(anal.miss.sens))  

table3 <- miss.perc%>%bind_rows(anal.miss.ass, anal.miss.anal,anal.miss.sens)

table3%>%
  pixiedust::dust()%>%
  pixiedust::sprinkle_border(rows = c(1,6,8,13),
                             border = c('top')
  )%>%
  pixiedust::sprinkle_border(rows = c(17),
                             border = c('bottom')
  )%>%
  pixiedust::sprinkle_colnames(m='',var='',p='%')%>%
  pixiedust::sprinkle_caption('Incomplete data amount and handling (n=63)')%>%
  pixiedust::sprinkle_label('tab3')%>%
  pixiedust::sprinkle(rows = 1:5,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 6:7,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 8:12,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle(rows = 13:17,
                      cols = 1,
                      merge = TRUE)%>%
  pixiedust::sprinkle_print_method('latex')%>%
  print()%>%
  pixiedust::sanitize_latex()%>%
  myfun%>%
  cat(file = 'Review Paper/table3.tex')


####################################
#### Additional data evaluation ####
####################################

m2_by_ta <- 
  pe %>%
  filter(M2.2 != 'Not reported/Other') %>%
  mutate(TA_new = fct_reorder(TA_new, M2, .fun='median')) %>%
  ggplot( aes(x=reorder(TA_new, M2), y=M2, fill=TA_new)) + 
  geom_boxplot() +
  labs(x = "Therapeutic area", y = 'Margin') +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_x_discrete(labels = c('Cardio', 'Cancer', 'Inf/Vac', 'OBGYN/U',
                              'Gastro', 'Other', 'Other IM', 'Autoim'
                              ))

pdf('Review Paper/m2_by_ta.pdf')
m2_by_ta
dev.off()


#target SS per sponsor
ss_by_sponsor <- pe %>%
  filter(sponsor_new != 'Not reported') %>%
  ggplot(aes(x = sponsor_new, y = Total.SS)) + 
  geom_boxplot() +
  labs(x = 'Sponsor', y = 'Target sample size') +
  theme_classic() 

pdf('Review Paper/ss_by_sponsor.pdf')
ss_by_sponsor
dev.off() 


m2_by_pc <- pe %>%
  filter(M2!=0.7)%>%
  ggplot(aes(x = p_C1, y = M2)) +
  geom_point() +
  labs(y = 'Margin',
       x = 'Event proportion, standard treatment') +
  theme_bw()

pdf('Review Paper/m2_by_pc.pdf')
m2_by_pc
dev.off() 


ss_target_acc <- pe %>% 
  mutate(ss = (Total.SS - ss_rand)/Total.SS) %>% 
  ggplot(aes(ss)) +
  geom_histogram(bins = 15, color = 'black', fill = 'white') +
  labs(y = 'Number of studies', 
       x = 'Target vs randomized sample size % difference' ) +
  theme_classic()

pdf('Review Paper/ss_target_acc.pdf')
ss_target_acc
dev.off() 

#regulatory guidance
table(pe$reg_guide_quote)

# tmp <- pe %>% 
#   select(id, M2, M2_comp, p_diff_ci, M2.2, Ni.decision1) %>% 
#   mutate(p_diff_ci = as.numeric(p_diff_ci), 
#          m2_ass_act = ifelse(grepl('above', M2_comp)>0, 
#                              (M2 - p_diff_ci), (p_diff_ci - M2)))
# 
# tmp %>% filter(M2_comp != 'not clear', !is.na(M2_comp), M2.2 != 'Not reported/Other',
#                grepl('CI', Ni.decision1)>0 ) %>%
#   ggplot(aes(x = m2_ass_act)) +
#   geom_histogram()
