
library(readxl)
library(readODS)
library(haven)
library(knitr)
library(broom)
library(ggplot2)
library(kableExtra)
library(MASS)
library(AER)
# reading data

agesex <- read_dta("population_age_sex.dta")
edu <- read_dta("population_education.dta")
buildings <- read_dta("buildings.dta")
elections <- read_dta("elections_ooe.dta")
emp <- read_dta("employment_by_sector.dta")
fin <- read_dta("finpower.dta")
mun <- read_dta("municipality_id.dta")
border <- read_dta("border_coordinates.dta")
unemployment <- read_dta("unemployment_statcube.dta")
foreigners <- read_dta("foreign_born_statcube.dta")
foreigners <- foreigners[order(foreigners$comm_id, decreasing = FALSE),]
spatial <- read_dta("spatial_data.dta")
refugees <- read_dta("refugees_weichenberger.dta") 

spatial$comm_id <- as.numeric(gsub(spatial$COMM_ID, pattern = "AT", replacement = ""))
refugees$comm_id <- as.numeric(refugees$comm_id)

refugees$asyl_seek <- refugees$refugee_abs_21_9_2015 >0 

########################################################







# using only municipalities with less than 6662 inhibitants, as all bigger municipalities have buildings for communities due to size

smallPop11 <- mun$pop_2011 < 6662
agesex <- agesex[smallPop11, ]
edu <- edu[smallPop11,]
buildings <- buildings[smallPop11,]
elections <- elections[smallPop11,]
emp <- emp[smallPop11,]
fin <- fin[smallPop11,]
mun <- mun[smallPop11,]
unemployment <- unemployment[smallPop11,]
foreigners <- foreigners[smallPop11,]
spatial <- spatial[smallPop11,]
refugees <- refugees[smallPop11,]


################################################################################

#### for covariates NOT used in the paper


########## education 2001

edu2 <- read.csv("OOE_Bev_Hoechste_abgeschl_Ausbildung.csv", sep = ";")
head(edu2)
edu_2001 <- edu2[edu2$YEAR== 2001,]


###########  gender and age

## sex: 0 = total, 1 = male, 2 = female


sexage2001 <- read.csv("OOE_Bev_laut_Volkszaehlung_Geschl_Alt5J.csv", sep =";")
sexage2001 <- sexage2001[sexage2001$YEAR == 2001 ,]

share_women_2001 <- data.frame((sexage2001$AGE_TOTAL[sexage2001$SEX == 2])/(sexage2001$AGE_TOTAL[sexage2001$SEX == 0]), sexage2001$LAU2_CODE)
share_under_30 <- data.frame(((sexage2001$AGE_0_TO_4+sexage2001$AGE_5_TO_9 + sexage2001$AGE_10_TO_14+ sexage2001$AGE_15_TO_19+ sexage2001$AGE_20_TO_24+ sexage2001$AGE_25_TO_29)/(sexage2001$AGE_TOTAL))[sexage2001$SEX==0], sexage2001$LAU2_CODE)


########## employment by sector

## description: https://e-gov.ooe.gv.at/at.gv.ooe.ogd2-citi/#/detail/5063b608-baa2-453a-9937-3aeef9aa0485

#### sources for groupings:

# Oesch, D. (2006). Redrawing the Class Map: Stratification and Institutions in Britain, Germany, Sweden and Switzerland.
# https://link.springer.com/article/10.1007/s11109-008-9075-2
# Kitschelt, H. & McGann, A. (1995). The Radical Right in Western Europe: A Comparative Analysis.
# https://www.sora.at/themen/wahlverhalten.html

####


emp_sector <- read.csv("OOE_Erwerbstaetige_Branche.csv", sep = ";")

emp_sector <- emp_sector[emp_sector$YEAR==2011,]

############ Agricultural-Conservative Sector (OEVP)

emp_agr_cult <- 100*(emp_sector$EMPL_OENACE2008_SECTION_A + emp_sector$EMPL_OENACE2008_SECTION_B)/emp_sector$EMPL_OENACE2008_TOTAL


############ Industrial and Skilled Manual Work (SPOE, FPOE)

emp_Ind <- 100*(emp_sector$EMPL_OENACE2008_SECTION_C + emp_sector$EMPL_OENACE2008_SECTION_D + 
  emp_sector$EMPL_OENACE2008_SECTION_E + emp_sector$EMPL_OENACE2008_SECTION_F)/emp_sector$EMPL_OENACE2008_TOTAL


######### Basic Services, Trade and Transport (SPOE, FPOE)

emp_Trade <- 100*(emp_sector$EMPL_OENACE2008_SECTION_G + emp_sector$EMPL_OENACE2008_SECTION_H + 
   emp_sector$EMPL_OENACE2008_SECTION_I + emp_sector$EMPL_OENACE2008_SECTION_S)/emp_sector$EMPL_OENACE2008_TOTAL


######### Highly Qualified Professional Services (NEOS, Greens, OEVP)

emp_HQ <- 100*(emp_sector$EMPL_OENACE2008_SECTION_J + emp_sector$EMPL_OENACE2008_SECTION_K + 
    emp_sector$EMPL_OENACE2008_SECTION_M + emp_sector$EMPL_OENACE2008_SECTION_N)/emp_sector$EMPL_OENACE2008_TOTAL

######### Public Sector, Education & Health (SPOE, Greens)

emp_PS_EH <- 100*(emp_sector$EMPL_OENACE2008_SECTION_O + emp_sector$EMPL_OENACE2008_SECTION_P + 
       emp_sector$EMPL_OENACE2008_SECTION_Q)/emp_sector$EMPL_OENACE2008_TOTAL

######### Creative and Cultural Sector (Greens, NEOS)

emp_Cr_CS <- 100*(emp_sector$EMPL_OENACE2008_SECTION_R)/emp_sector$EMPL_OENACE2008_TOTAL

######### Real Estate and Private Households 

emp_RE_priv <- 100*(emp_sector$EMPL_OENACE2008_SECTION_L + emp_sector$EMPL_OENACE2008_SECTION_T + 
       emp_sector$EMPL_OENACE2008_SECTION_U)/emp_sector$EMPL_OENACE2008_TOTAL


     
emp_sec11 <- data.frame(emp_agr_cult, emp_Ind, emp_Trade, emp_HQ, emp_PS_EH ,emp_Cr_CS,  emp_RE_priv, emp_sector$COMMUNE_CODE)



################## election data 2003 ("Landtgaswahl 2003")

LTW03_FPOE <- read.csv("FPOE_2003.csv", sep =";")
LTW03_FPOE <- LTW03_FPOE[7:nrow(LTW03_FPOE) ,]
LTW03_FPOE$comm.ID <- as.numeric(LTW03_FPOE$comm.ID)
LTW03_FPOE$FPÖ.vote.share <- gsub(LTW03_FPOE$FPÖ.vote.share, pattern = ",", replacement = ".")
LTW03_FPOE$FPÖ.vote.share <- as.numeric(LTW03_FPOE$FPÖ.vote.share)


################################################################################









################################################################################

##### creating relative covariates out of the absolute values, 
##### also dummy for distance to Linz < 25km (all used in the paper)

distLinz25 <- spatial$dist_linz < 25
borderGER <- agesex$comm_id %in% border$comm_id
mun$popsq15 <- mun$pop_2015^2
mun$popgr01_11 <- 100*(mun$pop_2011 - mun$pop_2001)/ mun$pop_2001
mun$popgr11_14 <- 100*(mun$pop_2014 - mun$pop_2011)/ mun$pop_2011




vocEdu <- 100*(edu$edu_apprenticeship+ edu$edu_intermed_techn_vocat_sch)/(edu$edu_total- edu$edu_not_applicable)
compEdu <- 100*(edu$edu_compulsory_sch)/(edu$edu_total- edu$edu_not_applicable)
HighEdu <- 100*(edu$edu_university_fachhochschule)/(edu$edu_total- edu$edu_not_applicable)
SecEdu <- 100*(edu$edu_higher_techn_vocat_sch+ edu$edu_academ_second_sch+ edu$edu_post_second_course_colleges)/(edu$edu_total- edu$edu_not_applicable)

forshare11 <- 100*((foreigners$other_2011+foreigners$eu_efta_2011)/ (foreigners$eu_efta_2011 + foreigners$other_2011+foreigners$austria_2011))

forshare14 <- 100*(foreigners$eu_efta_2014+foreigners$other_2014)/ (foreigners$eu_efta_2014 + foreigners$other_2014+foreigners$austria_2014)

foreignersPer <- 100*foreigners$other_2011/(foreigners$eu_efta_2011 + foreigners$other_2011 + foreigners$austria_2011)

finstrength11 <- fin$finpower_2011/mun$pop_2011
pop11sq <- mun$pop_2011^2


#########  Instrumental Variable (dummy) for whether a municipality hosted refugees (from paper)

IV <- buildings$bfc_total > 0

########################################################################################################################################################

################ replication of table of the paper 

Tab1 <- c(mean(mun$pop_2011),sd(mun$pop_2011),mean(mun$popgr01_11),sd(mun$popgr01_11),mean(mun$popgr11_14),sd(mun$popgr11_14),mean(agesex$share_women_2011),sd(agesex$share_women_2011),mean(agesex$share_below30_2011),sd(agesex$share_below30_2011),mean(compEdu),sd(compEdu),mean(vocEdu),sd(vocEdu),mean(SecEdu),sd(SecEdu),mean(HighEdu),sd(HighEdu),mean(unemployment$uerx_2011),sd(unemployment$uerx_2011),mean(forshare11),sd(forshare11),mean(finstrength11),sd(finstrength11), mean(borderGER), sd(borderGER), mean(distLinz25), sd(distLinz25))

Tab1 <- round(Tab1, digits = 2)

ind <- seq(1,(length(Tab1)-1),by = 2) 
tab1 <- numeric(0)           
for(i in 1:length(ind)){
  tab1[i] <- paste(Tab1[ind[i]], paste0("(",paste0(Tab1[ind[i]+1], ")")))
  
}


Tab2 <- c(mean(mun$pop_2011[IV]),sd(mun$pop_2011[IV]),mean(mun$popgr01_11[IV]),sd(mun$popgr01_11[IV]),mean(mun$popgr11_14[IV]),sd(mun$popgr11_14[IV]),mean(agesex$share_women_2011[IV]),sd(agesex$share_women_2011[IV]),mean(agesex$share_below30_2011[IV]),sd(agesex$share_below30_2011[IV]),mean(compEdu[IV]),sd(compEdu[IV]),mean(vocEdu[IV]),sd(vocEdu[IV]),mean(SecEdu[IV]),sd(SecEdu[IV]),mean(HighEdu[IV]),sd(HighEdu[IV]),mean(unemployment$uerx_2011[IV]),sd(unemployment$uerx_2011[IV]),mean(forshare11[IV]),sd(forshare11[IV]),mean(finstrength11[IV]),sd(finstrength11[IV]), mean(borderGER[IV]), sd(borderGER[IV]), mean(distLinz25[IV]), sd(distLinz25[IV]))

Tab2 <- round(Tab2, digits = 2)

ind <- seq(1,(length(Tab2)-1),by = 2) 
tab2 <- numeric(0)           
for(i in 1:length(ind)){
  tab2[i] <- paste(Tab2[ind[i]], paste0("(",paste0(Tab2[ind[i]+1], ")")))
}


Tab3 <- c(mean(mun$pop_2011[!IV]),sd(mun$pop_2011[!IV]),mean(mun$popgr01_11[!IV]),sd(mun$popgr01_11[!IV]),mean(mun$popgr11_14[!IV]),sd(mun$popgr11_14[!IV]),mean(agesex$share_women_2011[!IV]),sd(agesex$share_women_2011[!IV]),mean(agesex$share_below30_2011[!IV]),sd(agesex$share_below30_2011[!IV]),mean(compEdu[!IV]),sd(compEdu[!IV]),mean(vocEdu[!IV]),sd(vocEdu[!IV]),mean(SecEdu[!IV]),sd(SecEdu[!IV]),mean(HighEdu[!IV]),sd(HighEdu[!IV]),mean(unemployment$uerx_2011[!IV]),sd(unemployment$uerx_2011[!IV]),mean(forshare11[!IV]),sd(forshare11[!IV]),mean(finstrength11[!IV]),sd(finstrength11[!IV]), mean(borderGER[!IV]), sd(borderGER[!IV]), mean(distLinz25[!IV]), sd(distLinz25[!IV]))
Tab3 <- round(Tab3, digits = 2)


ind <- seq(1,(length(Tab2)-1),by = 2) 
tab3 <- numeric(0)           
for(i in 1:length(ind)){
  tab3[i] <- paste(Tab3[ind[i]], paste0("(",paste0(Tab3[ind[i]+1], ")")))
}



Desk1 <- data.frame(tab2,tab3,tab1)
rownames(Desk1) <- c("Population", "Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population", "Share of population below 30 years", "Share pop. compulsory education", "Share pop. vocational education", "Share pop. secondary education", "Share pop. higher education", "Unemployment rate", "Population share of foreigners","Financial strength of community","Direct border with Germany", "Linear distance to Linz <25km")
colnames(Desk1) <- c("1", "0", "Total")



kable(Desk1, ,booktabs = TRUE,linesep = '', format = "latex",caption = "Descriptive Statistics(mean and standard deviation) for communities with and without BFC")%>% 
  add_header_above(c("Buildings for communities in municipality" = 4))
################################################################################

#### For TSLS(exogene variates that were shown influential in describing voting 
#### results of (far) right parties, here used to describe the percentage of FPOE 2009): 
#### For financial power data from 2010 is used, as it's the closest to 2009 and the earliest available


mun$popsq09 <- mun$pop_2009^2
forshare09 <- 100*((foreigners$other_2009+foreigners$eu_efta_2009)/ (foreigners$eu_efta_2009 + foreigners$other_2009+foreigners$austria_2009))
finstrength10 <- fin$finpower_2010/mun$pop_2010
mun$popgr01_09 <- 100*(mun$pop_2009 - mun$pop_2001)/ mun$pop_2001

Yfpoe <- elections$lt_fpo_per_15 - elections$lt_fpo_per_09

X_paper <- cbind(IV, mun$pop_2011, pop11sq, mun$popgr01_11, mun$popgr11_14,agesex$share_women_2011, agesex$share_below30_2011, vocEdu, HighEdu, SecEdu, unemployment$uerx_2011, forshare11, finstrength11 ,borderGER, distLinz25)



lm_paper <- lm(Yfpoe ~ X_paper)


################################################################################

###### LMs reducing endogenity, describing with data not influenced by 2009 election(step by step)

################################################################################


### municipalities existing in 2001, 2011 and 2015, and having  less than 662 inhabitants in 2015


agesex$comm_id <- as.numeric(agesex$comm_id)



allcomms <- sort(unique(c(sexage2001$LAU2_CODE[sexage2001$SEX== 0], as.numeric(agesex$comm_id), emp_sec11$emp_sector.COMMUNE_CODE, edu_2001$COMMUNE_CODE,LTW03_FPOE$comm.ID )))

N1 <- allcomms[!allcomms%in%  agesex$comm_id]
N2 <- allcomms[!allcomms %in% edu_2001$COMMUNE_CODE]
N3 <- allcomms[!allcomms %in% emp_sec11$emp_sector.COMMUNE_CODE]
N4 <- allcomms[!allcomms %in% sexage2001$LAU2_CODE[sexage2001$SEX== 0]]
N5 <- allcomms[!allcomms %in%  LTW03_FPOE$comm.ID]
  
N <- sort(unique(c(N1,N2,N3,N4,N5)))

##### municipalities left for regression

mLeft <- allcomms[!allcomms %in% N]


###### covariates used in paper 


edu <- edu[agesex$comm_id %in% mLeft,]
buildings <- buildings[agesex$comm_id %in% mLeft,]
elections <- elections[agesex$comm_id %in% mLeft,]
emp <- emp[agesex$comm_id %in% mLeft,]
fin <- fin[agesex$comm_id %in% mLeft,]
mun <- mun[agesex$comm_id %in% mLeft,]
unemployment <- unemployment[agesex$comm_id %in% mLeft,]
foreigners <- foreigners[agesex$comm_id %in% mLeft,]
spatial <- spatial[agesex$comm_id %in% mLeft,]
agesex <- agesex[agesex$comm_id %in% mLeft,]
refugees <- refugees[refugees$comm_id %in% mLeft,]


####### added covariates (exogenes)

emp_sec11 <- emp_sec11[emp_sec11$emp_sector.COMMUNE_CODE %in% mLeft,]
edu_2001 <- edu_2001[edu_2001$COMMUNE_CODE %in% mLeft,]
sexage2001 <- sexage2001[sexage2001$LAU2_CODE %in% mLeft,]
LTW03_FPOE <- LTW03_FPOE[LTW03_FPOE$comm.ID %in% mLeft,]


share_women_2001 <- data.frame((sexage2001$AGE_TOTAL[sexage2001$SEX == 2])/(sexage2001$AGE_TOTAL[sexage2001$SEX == 0]), sexage2001$LAU2_CODE[sexage2001$SEX == 0])
share_under_30 <- data.frame(((sexage2001$AGE_0_TO_4+sexage2001$AGE_5_TO_9 + sexage2001$AGE_10_TO_14+ sexage2001$AGE_15_TO_19+ sexage2001$AGE_20_TO_24+ sexage2001$AGE_25_TO_29)/(sexage2001$AGE_TOTAL))[sexage2001$SEX==0], sexage2001$LAU2_CODE[sexage2001$SEX==0])
mun$popsq09 <- mun$pop_2009^2
forshare09 <- 100*((foreigners$other_2009+foreigners$eu_efta_2009)/ (foreigners$eu_efta_2009 + foreigners$other_2009+foreigners$austria_2009))
finstrength10 <- fin$finpower_2010/mun$pop_2010
finstrength10 <- as.vector(finstrength10)
mun$popgr01_09 <- 100*(mun$pop_2009 - mun$pop_2001)/ mun$pop_2001

vocEdu2001 <- 100*(edu_2001$EDU_APPRENTICESHIP+ edu_2001$EDU_INTERMED_TECHN_VOCAT_SCH)/(edu_2001$EDU_TOTAL- edu_2001$EDU_NOT_APPLICABLE)
compEdu2001 <- 100*edu_2001$EDU_COMPULSORY_SCH/(edu_2001$EDU_TOTAL- edu_2001$EDU_NOT_APPLICABLE)
HighEdu2001 <- 100*edu_2001$EDU_UNIVERSITY_FACHHOCHSCHULE/(edu_2001$EDU_TOTAL- edu_2001$EDU_NOT_APPLICABLE)
SecEdu2001 <- 100*(edu_2001$EDU_INTERMED_TECHN_VOCAT_SCH+ edu_2001$EDU_ACADEM_SECOND_SCH+ edu_2001$EDU_POST_SECOND_COURSE_COLLEGES)/(edu_2001$EDU_TOTAL - edu_2001$EDU_NOT_APPLICABLE)


################### reative covariates of paper, for new limited municipalities

distLinz25 <- spatial$dist_linz < 25
borderGER <- agesex$comm_id %in% border$comm_id
mun$popsq15 <- mun$pop_2015^2
mun$popgr01_11 <- 100*(mun$pop_2011 - mun$pop_2001)/ mun$pop_2001
mun$popgr11_14 <- 100*(mun$pop_2014 - mun$pop_2011)/ mun$pop_2011




vocEdu <- 100*(edu$edu_apprenticeship+ edu$edu_intermed_techn_vocat_sch)/(edu$edu_total- edu$edu_not_applicable)
compEdu <- 100*(edu$edu_compulsory_sch)/(edu$edu_total- edu$edu_not_applicable)
HighEdu <- 100*(edu$edu_university_fachhochschule)/(edu$edu_total- edu$edu_not_applicable)
SecEdu <- 100*(edu$edu_higher_techn_vocat_sch+ edu$edu_academ_second_sch+ edu$edu_post_second_course_colleges)/(edu$edu_total- edu$edu_not_applicable)

forshare11 <- 100*((foreigners$other_2011+foreigners$eu_efta_2011)/ (foreigners$eu_efta_2011 + foreigners$other_2011+foreigners$austria_2011))

forshare14 <- 100*(foreigners$eu_efta_2014+foreigners$other_2014)/ (foreigners$eu_efta_2014 + foreigners$other_2014+foreigners$austria_2014)

foreignersPer <- 100*foreigners$other_2011/(foreigners$eu_efta_2011 + foreigners$other_2011 + foreigners$austria_2011)

finstrength11 <- fin$finpower_2011/mun$pop_2011
pop11sq <- mun$pop_2011^2

IV <- buildings$bfc_total > 0



################################################################################

### amount of municipalities left: n = 408

length(mLeft)


#### matrix of endogenes

Int <- rep(1, times = length(mLeft))

u_30_2001 <- share_under_30$X..sexage2001.AGE_0_TO_4...sexage2001.AGE_5_TO_9...sexage2001.AGE_10_TO_14...

# CompEdu "base level", otherwise no full rank of W


Yfpoe <- elections$lt_fpo_per_15 - elections$lt_fpo_per_09


df <- data.frame(
  Yfpoe = Yfpoe,
  IV = IV,
  pop_2011 = mun$pop_2011,
  pop11sq = pop11sq,
  popgr01_11 = mun$popgr01_11,
  popgr11_14 = mun$popgr11_14,
  share_women_2011 = agesex$share_women_2011,
  share_below30_2011 = agesex$share_below30_2011,
  vocEdu = vocEdu,
  HighEdu = HighEdu,
  SecEdu = SecEdu,
  uerx_2011 = unemployment$uerx_2011,
  forshare11 = forshare11,
  finstrength11 = finstrength11,
  borderGER = borderGER,
  distLinz25 = distLinz25,
  lt_fpo_per_09 = elections$lt_fpo_per_09,
  asylum_seekers = refugees$asyl_seek,
  
  u_30_2001 = u_30_2001,
  share_women_2001 = as.vector(share_women_2001$X.sexage2001.AGE_TOTAL.sexage2001.SEX....2....sexage2001.AGE_TOTAL.sexage2001.SEX....),
  pop_2009 = mun$pop_2009,
  forshare09 = forshare09,
  fpo_share_2003 = LTW03_FPOE$FPÖ.vote.share,
  vocEdu2001 = vocEdu2001,
  HighEdu2001 = HighEdu2001,
  SecEdu2001 = SecEdu2001,
  emp_agr_cult = emp_sec11$emp_agr_cult,
  emp_Ind = emp_sec11$emp_Ind,
  emp_Trade = emp_sec11$emp_Trade,
  emp_HQ = emp_sec11$emp_HQ,
  emp_PS_EH = emp_sec11$emp_PS_EH,
  emp_Cr_CS = emp_sec11$emp_Cr_CS,
  emp_RE_priv = emp_sec11$emp_RE_priv,
  finstrength10 = finstrength10
)

X <-  data.frame(
  asylum_seekers = refugees$asyl_seek,
  pop_2011 = mun$pop_2011,
  pop11sq = pop11sq,
  popgr01_11 = mun$popgr01_11,
  popgr11_14 = mun$popgr11_14,
  share_women_2011 = agesex$share_women_2011,
  share_below30_2011 = agesex$share_below30_2011,
  vocEdu = vocEdu,
  HighEdu = HighEdu,
  SecEdu = SecEdu,
  uerx_2011 = unemployment$uerx_2011,
  forshare11 = forshare11,
  finstrength11 = finstrength11,
  borderGER = borderGER,
  distLinz25 = distLinz25,
  lt_fpo_per_09 = elections$lt_fpo_per_09
)
X <- as.matrix(X)
X_paper <- X[,-ncol(X)]

W <- data.frame(
  Int = rep(1, times = length(u_30_2001)),
  u_30_2001 = u_30_2001,
  share_women_2001 = as.vector(share_women_2001$X.sexage2001.AGE_TOTAL.sexage2001.SEX....2....sexage2001.AGE_TOTAL.sexage2001.SEX....),
  pop_2009 = mun$pop_2009,
  forshare09 = forshare09,
  fpo_share_2003 = LTW03_FPOE$FPÖ.vote.share,
  vocEdu2001 = vocEdu2001,
  HighEdu2001 = HighEdu2001,
  SecEdu2001 = SecEdu2001,
  emp_agr_cult = emp_sec11$emp_agr_cult,
  emp_Ind = emp_sec11$emp_Ind,
  emp_Trade = emp_sec11$emp_Trade,
  emp_HQ = emp_sec11$emp_HQ,
  emp_PS_EH = emp_sec11$emp_PS_EH,
  emp_Cr_CS = emp_sec11$emp_Cr_CS,
  emp_RE_priv = emp_sec11$emp_RE_priv,
  finstrength10 = finstrength10,
  IV = IV,
  borderGER = borderGER,
  distLinz25 = distLinz25
)
W <- as.matrix(W)

Int <- rep(1, times = nrow(X))



################################################################################
#
# lm with only "Fpoe vote share 2009" instrumentized 
#
# first regression of the exogene variate "Fpoe vote share 2009" on the space of 
# the endogenes; 
# -> because of LS-estimation the fitted values are the porojection on W
# endogenes don't have to be projected, as they are element of the column space of
# the endogenes and P_X X = X 
# 
# Using only instruments predetermined regarding the election 2009
# 
#
################################################################################




# CompEdu "base level", otherwise no full rank of W



X_names <-  c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
        "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25", "lt_fpo_per_09")  

X_names_paper <-  c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
              "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25")  

X_names_paper_orig <-  c("IV","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
                         "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25")  

endogenes <- c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
               "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11",  "lt_fpo_per_09")  


exogenes <- c(
  "u_30_2001", "share_women_2001", "pop_2009","forshare09", "fpo_share_2003",
  "vocEdu2001", "HighEdu2001", "SecEdu2001", "emp_agr_cult", "emp_Ind",
  "emp_Trade", "emp_HQ", "emp_PS_EH", "emp_Cr_CS", "emp_RE_priv", "IV","borderGER",
  "distLinz25","finstrength10"
)

###############################################################################
#### 
#### regression from paper, but with n = 208, tradeoff for using instruments




formula_str <- paste("Yfpoe", "~", paste(X_names_paper_orig, collapse = " + "))

fml <- as.formula(formula_str)

lm_paper_orig_2 <- lm(fml, data = df)
summary(lm_paper_orig_2)



################################################################################
#### instrumented fpoeshare 2009
#### only with theoretical influential exogenes

formula_str <- paste("lt_fpo_per_09", "~", paste(exogenes, collapse = " + "))

fml <- as.formula(formula_str)

lm_IV_fpoe_2009 <- lm(fml, data = df)
summary(lm_IV_fpoe_2009)

IV_fpoe_2009 <- lm_IV_fpoe_2009$fitted.values

df$IV_fpoe_2009 <- IV_fpoe_2009


### instrumented fpoeshare 2009 
### with all exogenes under (bad) assumption

formula_str <- paste("lt_fpo_per_09", "~", paste(c(exogenes,endogenes[-c(1,length(endogenes))]), collapse = " + "))
fml <- as.formula(formula_str)

lm_IV_fpoe_2009_bad <- lm(fml, data = df)
summary(lm_IV_fpoe_2009_bad)

IV_fpoe_2009_bad <- lm_IV_fpoe_2009_bad$fitted.values

df$IV_fpoe_2009_bad <- IV_fpoe_2009_bad

# _bad is used in ivreg

###############################################################################



###############################################################################
#####
##### adding instrumented fpoe vote share 2009 to the papers model
##### last 3 columns from W excluded, duplicates otherwise
#####
##### 


X_names_paper_ext <-  c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
                    "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25", "lt_fpo_per_09")  
W_names <-  c("IV","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
              "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25", 
              "u_30_2001", "share_women_2001", "pop_2009", "forshare09", "fpo_share_2003",
              "vocEdu2001", "HighEdu2001", "SecEdu2001", "emp_agr_cult", "emp_Ind",
              "emp_Trade", "emp_HQ", "emp_PS_EH", "emp_Cr_CS", "emp_RE_priv", "IV","borderGER",
              "distLinz25","finstrength10"
)
              

formula_str <- paste("Yfpoe", "~", paste(paste(X_names_paper_ext, collapse = " + "), paste(W_names, collapse = " + "), sep  = " | "))

########## All assumed exogenes to instrument fpoe votes 2009

lm_TSLS <- ivreg(Yfpoe ~ asylum_seekers + pop_2011 + pop11sq + popgr01_11 + popgr11_14 + share_women_2011 + share_below30_2011 + vocEdu + 
        HighEdu + SecEdu + uerx_2011 + forshare11 + finstrength11 + borderGER + distLinz25 + lt_fpo_per_09 | IV +
        pop_2011 + pop11sq + popgr01_11 + popgr11_14 + share_women_2011 + share_below30_2011 + vocEdu + HighEdu + 
        SecEdu + uerx_2011 + forshare11 + finstrength11 + borderGER + distLinz25 + u_30_2001 + share_women_2001 + 
        pop_2009+  forshare09 + fpo_share_2003 + vocEdu2001 + HighEdu2001 + SecEdu2001 + emp_agr_cult + emp_Ind + 
        emp_Trade + emp_HQ + emp_PS_EH + emp_Cr_CS + emp_RE_priv + IV + borderGER + distLinz25 + finstrength10, data = df)


X_names_paper_IVfpoe <-  c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
                        "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25", "IV_fpoe_2009")

formula_str <- paste("Yfpoe", "~", paste(X_names_paper_IVfpoe, collapse = " + "))
fml <- as.formula(formula_str)

########## All valid exogenes to instrument fpoe votes 2009

lm_TSLS_2 <- lm(fml, data = df)
X_TSLS2 <- as.matrix(cbind(rep(1,times = nrow(df)), df[,X_names_paper_IVfpoe ]))
fitted_TSLS2 <- X_TSLS2%*%lm_TSLS_2$coefficients
resid <- Yfpoe - fitted_TSLS2
sig2 <- sum(resid^2) /(nrow(X_TSLS2)- ncol(X_TSLS2))
sig2_wrong <- (sum(lm_TSLS_2$residuals^2))/(nrow(X_TSLS2)- ncol(X_TSLS2))
correction_1 <- sig2/sig2_wrong
# covariance matrix is right, sig2 hat is wrong, wrong residuals as resid = Y- P_w Xbeta are used, not Y - Xbeta

###########################################################



################################################################################


####### Model with all probable endogenes instrumentized

### with fpoe votes 2009

# Projection of X on W


models <- list()

for (i in endogenes) {
  formula_str <- paste(i, "~", paste(exogenes, collapse = " + "))
  
  fml <- as.formula(formula_str)
  
  models[[i]] <- lm(fml, data = df)
  
  cat("\n--- Regression für", i, "---\n")
  print(summary(models[[i]]))
  
}

# projection of X on W (to show first stage of LS in TSLS)

P_WX <- data.frame()
P_WX <- data.frame(models[[1]]$fitted.values)

for(i in 2:length(endogenes)){
  P_WX <- cbind(P_WX, models[[i]]$fitted.values)
}



# append columns of X that are \in span(W)

P_WX <- cbind(P_WX, borderGER, distLinz25, IV,Yfpoe) 


colnames(P_WX) <- c(paste0(endogenes, "_IV"),"borderGER", "distLinz25", "IV","Yfpoe")
X_names_IV <- colnames(P_WX)[-ncol(P_WX)]
X_names_paper_IV <- X_names_IV[X_names_IV != "lt_fpo_per_09_IV"]

formula_str <- paste("Yfpoe", "~", paste(X_names_IV, collapse = " + "))
fml <- as.formula(formula_str)
LM_TSLS_test <- lm(fml, data = P_WX)

# summary(LM_TSLS_test) to compare, results are the same, is not used


P_W <- W%*%solve(t(W)%*%W)%*%t(W)
Xhat <- P_W %*% X

LM_TSLS <- lm(Yfpoe ~ Xhat)

# summary(LM_TSLS) for comparison with latter method, yield same results
X_des <- cbind(Int, X)

fitted <- X_des%*%LM_TSLS$coefficients
resid <- Yfpoe-fitted
sig2 <- sum(resid^2)/(nrow(X)-ncol(X))
sig2_wrong <- (sum(LM_TSLS$residuals^2))/(nrow(X)-ncol(X))
correction_2 <- sig2/sig2_wrong

################################################################################
#### without fpoe votes 2009

Xhat_paper <- P_W %*%X_paper
colnames(X_paper)

formula_str <- paste("Yfpoe", "~", paste(X_names_paper_IV, collapse = " + "))
fml <- as.formula(formula_str)
LM_TSLS2_test <- lm(fml, data = P_WX)

# summary(LM_TSLS2_test) again to compare, identical

LM_TSLS2 <- lm(Yfpoe ~ Xhat_paper)

# summary(LM_TSLS2) same results


X_des <- cbind(Int, X_paper)


fitted <- X_des%*%LM_TSLS2$coefficients
resid <- Yfpoe-fitted
sig2 <- sum(resid^2)/(nrow(X)-ncol(X))
sig2_wrong <- (sum(LM_TSLS2$residuals^2))/(nrow(X)-ncol(X))
correction_3 <- sig2/sig2_wrong

################################################################################

# only asylum_seekers instrumented by TSLS

# assumed exogenes in paper (new ones not added)


W_3 <- as.matrix(cbind(IV, X_paper[,-1]))
IV_asyl_proj <- lm(X_paper[,1]~ W_3)
IV_asyl <- IV_asyl_proj$fitted.values
Xhat_asyl <-cbind(IV_asyl, X_paper[,-1])


## Xhat asyl no dummy anymore, harder to interprete


lm_tsls_on_asyl <- lm(Yfpoe ~ Xhat_asyl)
summary(lm_tsls_on_asyl)

X <- as.matrix(cbind(rep(1, times = nrow(X_paper)),X_paper))

fitted <- X%*%lm_tsls_on_asyl$coefficients
resid <- Yfpoe-fitted
sig2 <- sum(resid^2)/(nrow(X)-ncol(X))
sig2_wrong <- (sum(lm_tsls_on_asyl$residuals^2))/(nrow(X)-ncol(X))
correction_4 <- sig2/sig2_wrong


################################################################################

######## exogenes assumed in paper + added ones 

W_4 <- as.matrix(cbind(W, X_paper[,-1]))
IV_asyl_proj_2 <- lm(X_paper[,1]~ W_4)
IV_asyl_2 <- IV_asyl_proj_2$fitted.values
Xhat_asyl_2 <- cbind(IV_asyl_2, X_paper[,-1])


## Xhat asyl no dummy anymore, harder to interprete

lm_tsls_on_asyl_2 <- lm(Yfpoe ~ Xhat_asyl_2)
summary(lm_tsls_on_asyl_2)

fitted <- X%*%lm_tsls_on_asyl_2$coefficients
resid <- Yfpoe-fitted
sig2 <- sum(resid^2)/(nrow(X)-ncol(X))
sig2_wrong <- (sum(lm_tsls_on_asyl_2$residuals^2))/(nrow(X)-ncol(X))
correction_5 <- sig2/sig2_wrong

################################################################################

############ decriptive statistics and tables




## lm of the paper
#lm1tab <- tidy(lm1)

#confU <- confint(lm1)[,1]
#confO <- confint(lm1)[,2]

#Table1 <- data.frame(lm1$coefficients, confU, confO, lm1tab$p.value)
#Table1 <- round(Table1, digits = 3)

#rownames(Table1) <- c("Intercept","BFC","Population", "Population squared" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population", "Share of population below 30 years", "Share pop. vocational education", "Share pop. higher education","Share pop. secondary education",  "Unemployment rate", "Population share of foreigners","Financial strength of community","Direct border with Germany", "Linear distance to Linz <25km")

#colnames(Table1) <- c("beta", "95%-CI_low","95%-CI_up","p-value")

#kable(Table1, caption = "OLS: Effect of refugees (instrumentized by BFC) on the change in state council vote shares of the FPÖ")  

################################################################################

# descriptive statistics of exogenes (data before 2009) or predetermined

m <- round(apply(W[,-1], FUN = mean, MARGIN = 2), digits = 2)
SD <- round(apply(W[,-1], FUN = sd, MARGIN = 2), digits = 2)
TabExo <- paste0(m,paste0("(",paste0(SD, ")")))

tabExo <- TabExo           

DeskExo <- data.frame(tabExo)
rownames(DeskExo) <- c("Share of population below 30 years(2001)", "Share of women in the population (2001)", "Population (2009)", 
                      "Population share of foreigners (2009)" ,"FPOE vote share Landtagswahl 2003",
                       "Share pop. vocational education (2001)", "Share pop. higher education (2001)", "Share pop. secondary education (2001)",
                       "Share pop. empl. in agrarculture sector(2011)", "Share pop. empl. in industrial sector(2011)", 
                       "Share pop. empl. in trade sector(2011)", "Share pop. empl. in highly qualified sector(2011)",
                       "Share pop. empl. in public sector(2011)", "Share pop. empl. in creative sector(2011)", 
                       "Share pop. empl. in private sector(2011)", "Financial strength of community (2010)",
                       "building(s) for communities existent", "Direct border with Germany", "Linear distance to Linz ¡25km"
                        )
colnames(DeskExo) <- c("mean, sd")

kable(DeskExo,format = "latex",booktabs = TRUE,linesep = '')

################################################################################

## lm of the paper
lm1tab <- tidy(lm_paper)

confU <- confint(lm_paper)[,1]
confO <- confint(lm_paper)[,2]

Table1 <- data.frame(lm_paper$coefficients, confU, confO, lm1tab$p.value)
Table1 <- round(Table1, digits = 3)

rownames(Table1) <- c("Intercept","BFC","Population", "Population squared" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population", "Share of population below 30 years", "Share pop. vocational education", "Share pop. higher education","Share pop. secondary education",  "Unemployment rate", "Population share of foreigners","Financial strength of community","Direct border with Germany", "Linear distance to Linz <25km")

colnames(Table1) <- c("beta", "95%-CI_low","95%-CI_up","p-value")

kable(Table1, format = "latex",booktabs = TRUE,linesep = '', caption = "OLS: Effect of refugees (instrumentized by BFC) on the change in state council vote shares of the FPÖ")  


################################################################################
##### first stage of TSLS, regressing fpoe votes 2009 on W

lm2tab <- tidy(lm_IV_fpoe_2009)
confU <- confint(lm_IV_fpoe_2009)[,1]
confO <- confint(lm_IV_fpoe_2009)[,2]

Table2 <- data.frame(lm_IV_fpoe_2009$coefficients, confU, confO, lm2tab$p.value)
Table2 <- round(Table2, digits = 3)

# rownames(Table2) <- c("Intercept","BFC","Population", "Population squared" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population", "Share of population below 30 years", "Share pop. vocational education", "Share pop. higher education","Share pop. secondary education",  "Unemployment rate", "Population share of foreigners","Financial strength of community","Direct border with Germany", "Linear distance to Linz <25km")
# ändern

colnames(Table2) <- c("beta", "95%-CI_low","95%-CI_up","p-value")

kable(Table2, caption = "OLS: Effect of refugees (instrumentized by BFC) on the change in state council vote shares of the FPÖ",format = "latex",booktabs = TRUE,linesep = '') 

################################################################################

###### projection of asylum seekers on assumed exogenes from paper

IV_asyl_proj_tab <- tidy(IV_asyl_proj)
confU <- confint(IV_asyl_proj)[,1]
confO <- confint(IV_asyl_proj)[,2]
Table_asyl <- data.frame(IV_asyl_proj$coefficients, confU, confO, IV_asyl_proj_tab$p.value)
Table_asyl <- round(Table_asyl, digits = 3)

colnames(Table_asyl) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(Table_asyl) <- c("Intercept","Buildings for communities existent","Population (2011)", "Population squared (2011)" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")


kable(Table_asyl, caption = "First Stage: Regresion of \"asylum seekers in community\" on exogenes from paper" ,format = "latex",booktabs = TRUE,linesep = '') 

################################################################################
# corresponding regression

lm_tsls_on_asyl_tab <- tidy(lm_tsls_on_asyl)
df <-  lm_tsls_on_asyl$df.residual
beta <- lm_tsls_on_asyl_tab$estimate
se <- sqrt(correction_4) * lm_tsls_on_asyl_tab$std.error
t <- beta / se 

lm_tsls_on_asyl_tab$std.error <- se
lm_tsls_on_asyl_tab$statistic <- t
lm_tsls_on_asyl_tab$p.value <-  2 * (1 - pnorm(abs(t)))

t_crit <- qnorm(0.975)

confU <- beta - t_crit * se
confO <- beta + t_crit * se

Table_asyl_lm <- data.frame(lm_tsls_on_asyl$coefficients, confU, confO, lm_tsls_on_asyl_tab$p.value)
Table_asyl_lm <- round(Table_asyl_lm, digits = 3)



colnames(Table_asyl_lm) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(Table_asyl_lm) <- c("Intercept","Asylum seekers in community (2015)","Population (2011)", "Population squared (2011)" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")

kable(Table_asyl_lm, caption = "Second Stage: corresponding regression" ,format = "latex",booktabs = TRUE,linesep = '') 


################################################################################
## everything instrumentized

lm_tsls_on_asyl_2_tab <- tidy(lm_tsls_on_asyl_2)
df <-  lm_tsls_on_asyl_2$df.residual
beta <- lm_tsls_on_asyl_2_tab$estimate
se <- sqrt(correction_5) * lm_tsls_on_asyl_2_tab$std.error
t <- beta / se 

lm_tsls_on_asyl_2_tab$std.error <- se
lm_tsls_on_asyl_2_tab$statistic <- t
lm_tsls_on_asyl_2_tab$p.value <-  2 * (1 - pnorm(abs(t)))

t_crit <- qnorm(0.975)

confU <- beta - t_crit * se
confO <- beta + t_crit * se

Table_asyl_lm_2 <- data.frame(lm_tsls_on_asyl_2$coefficients, confU, confO, lm_tsls_on_asyl_2_tab$p.value)
Table_asyl_lm_2 <- round(Table_asyl_lm_2, digits = 3)



colnames(Table_asyl_lm_2) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(Table_asyl_lm_2) <- c("Intercept","Asylum seekers in community (2015)","Population (2011)", "Population squared (2011)" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")


kable(Table_asyl_lm_2, caption = "Second Stage: corresponding regression" ,format = "latex",booktabs = TRUE,linesep = '') 



################################################################################
## everything instrumentized


lm_TSLS2_tab <- tidy(LM_TSLS2)
df <-  LM_TSLS2$df.residual
beta <- lm_TSLS2_tab$estimate
se <- sqrt(correction_3) * lm_TSLS2_tab$std.error
t <- beta / se 

lm_TSLS2_tab$std.error <- se
lm_TSLS2_tab$statistic <- t
lm_TSLS2_tab$p.value <-  2 * (1 - pnorm(abs(t)))



t_crit <- qnorm(0.975)

confU <- beta - t_crit * se
confO <- beta + t_crit * se

Table_TSLS2_lm <- data.frame(LM_TSLS2$coefficients, confU, confO, lm_TSLS2_tab$p.value)
Table_TSLS2_lm <- round(Table_TSLS2_lm, digits = 3)



colnames(Table_TSLS2_lm) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(Table_TSLS2_lm) <- c("Intercept","Asylum seekers in community (2015)","Population (2011)", "Population squared (2011)" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")

kable(Table_TSLS2_lm, caption = "Second Stage: corresponding regression" ,format = "latex",booktabs = TRUE,linesep = '') 



################################################################################










################################################################################

######### lm adding projection of fpoe votes 2009 on the space of the exogenes

lm3tab <- tidy(lm_TSLS)
confU <- confint(lm_TSLS)[,1]
confO <- confint(lm_TSLS)[,2]

Table3 <- data.frame(lm_TSLS, confU, confO, lm3tab$p.value)
Table3 <- round(Table3, digits = 3)

# rownames(Table3) <- c("Intercept","BFC","Population", "Population squared" ,"Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population", "Share of population below 30 years", "Share pop. vocational education", "Share pop. higher education","Share pop. secondary education",  "Unemployment rate", "Population share of foreigners","Financial strength of community","Direct border with Germany", "Linear distance to Linz <25km")
# ändern

colnames(Table3) <- c("beta", "95%-CI_low","95%-CI_up","p-value")

kable(Table3, caption = "OLS: Effect of refugees (instrumentized by BFC) on the change in state council vote shares of the FPÖ",format = "latex",booktabs = TRUE,linesep = '') 





################################################################################

