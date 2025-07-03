library(readxl)
library(readODS)
library(haven)
library(knitr)
library(broom)
library(ggplot2)
library(kableExtra)
library(MASS)
library(AER)
library(car)
library(sandwich)
library(lmtest)
library(MASS)
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
pop11sq <- mun$pop_2011^2 - mean(mun$pop_2011^2)


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

##  For TSLS(exogene variates that were shown influential in describing voting )
##  results of (far) right parties, here used to describe the percentage of FPOE 2009): 
##  For financial power data from 2010 is used, as it's the closest to 2009 and the earliest available


mun$popsq09 <- mun$pop_2009^2
forshare09 <- 100*((foreigners$other_2009+foreigners$eu_efta_2009)/ (foreigners$eu_efta_2009 + foreigners$other_2009+foreigners$austria_2009))
finstrength10 <- fin$finpower_2010/mun$pop_2010
mun$popgr01_09 <- 100*(mun$pop_2009 - mun$pop_2001)/ mun$pop_2001

Yfpoe <- elections$lt_fpo_per_15 - elections$lt_fpo_per_09



############ First regression  (same as in paper, but without popsq09, due to numerical issues )


W <- cbind(IV, mun$pop_2011,  mun$popgr01_11, mun$popgr11_14,agesex$share_women_2011, agesex$share_below30_2011, vocEdu, HighEdu, SecEdu, unemployment$uerx_2011, forshare11, finstrength11 ,borderGER, distLinz25)
exos_paper<- cbind(mun$pop_2011,  mun$popgr01_11, mun$popgr11_14,agesex$share_women_2011, agesex$share_below30_2011, vocEdu, HighEdu, SecEdu, unemployment$uerx_2011, forshare11, finstrength11 ,borderGER, distLinz25)
X <- cbind(rep(1, times = nrow(W)),refugees$asyl_seek, exos_paper)

X_hat <- lm(refugees$asyl_seek ~ ., data = data.frame(W))


C_D_Ftest <- linearHypothesis( X_hat,"IV = 0")

robust_vcov <- vcovHC(X_hat, type ="HC0")
t_K_P_F <- (coef(X_hat)["IV"] / sqrt(robust_vcov["IV", "IV"]))^2

X_HAT <- cbind(X_hat$fitted.values, exos_paper)
lm_paper <- lm(Yfpoe ~ X_HAT)

W <- cbind(rep(1, times = nrow(X)),W)  
u <- as.vector(Yfpoe - X %*% lm_paper$coefficients)

# Function for the White estimator (robust SEs)

SEs_rob <- function(W,X,u){
  u2 <- u^2
  D_hat <- diag(u2, nrow= length(u), ncol=length(u))
  P_W <- W%*%solve(t(W)%*%W)%*%t(W)
  M <- solve(t(X) %*% P_W %*% X)
  
  vCov_rob <- M%*%t(X)%*%P_W%*%D_hat%*%P_W%*%X%*%M
  return(sqrt(diag(vCov_rob)))
}


vcov_rob_f <- function(W,X,u){
  u2 <- u^2
  D_hat <- diag(u2, nrow= length(u), ncol=length(u))
  P_W <- W%*%solve(t(W)%*%W)%*%t(W)
  M <- solve(t(X) %*% P_W %*% X)
  
  vCov_rob <- M%*%t(X)%*%P_W%*%D_hat%*%P_W%*%X%*%M
  return(vCov_rob)
}


SEs_rob_lm_paper <- SEs_rob(W,X,u)
vCov_rob_lm_paper <-  vcov_rob_f(W,X,u)

## for hausmann test illustration

sig2 <- as.numeric(t(u)%*%u *(length(u)- length(lm_paper$coefficients)))
vcov_homoskedastic_lm_paper <-  solve(t(X)%*%W%*%solve(t(W)%*%W)%*%t(W)%*%X)
vcov_homoskedastic_lm_paper <- sig2 * vcov_homoskedastic_lm_paper


############ Table first stage

###### projection of asylum seekers on assumed exogenes from paper

IV_asyl_proj_tab <- tidy(X_hat)
confU <- confint(X_hat)[,1]
confO <- confint(X_hat)[,2]
Table_asyl <- data.frame(X_hat$coefficients, confU, confO, IV_asyl_proj_tab$p.value)
Table_asyl <- round(Table_asyl, digits = 3)

colnames(Table_asyl) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(Table_asyl) <- c("Intercept","Buildings for communities existent","Population (2011)", "Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")


kable(Table_asyl, caption = "First Stage: Regression of \"asylum seekers in community\" on exogenes from paper" ,format = "latex",booktabs = TRUE,linesep = '') 

# for the table
C_D_Ftest$F[2]
t_K_P_F 
sum(summary(X_hat)$df[c(1,2)])
summary(X_hat)$r.squared

########### Table second stage

Table <- function(lm, SE){
  beta <- coef(lm)
  df <- lm$df.residual
  t <- beta/SE
  t_crit <- qnorm(0.975)
  
  p <- 2 * (1 - pnorm(abs(t)))
  confU <- beta - t_crit * SE
  confO <- beta + t_crit * SE
  
  tab <- data.frame(beta, confU,confO, p)
  return(tab)
}

TAB <- Table(lm_paper,SEs_rob_lm_paper)
colnames(TAB) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(TAB) <- c("Intercept","Asylum seekers in community (2015)","Population (2011)", "Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")

Tab_lm_paper <- round(TAB, digits = 3)

kable(Tab_lm_paper, caption = "Second Stage: corresponding regression" ,format = "latex",booktabs = TRUE,linesep = '') 

summary(lm_paper)$r.squared








################# Second model:


################################################################################

### Preparing the data
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
  distLinz25 = distLinz25
)
X <- as.matrix(X)

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




X_names_paper <-  c("asylum_seekers","pop_2011", "pop11sq", "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
                    "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11", "borderGER", "distLinz25")  




endogenes <- c("asylum_seekers","pop_2011",  "popgr01_11","popgr11_14", "share_women_2011","share_below30_2011",
               "vocEdu","HighEdu", "SecEdu","uerx_2011","forshare11", "finstrength11")  


exogenes <- c(
  "u_30_2001", "share_women_2001", "pop_2009","forshare09", "fpo_share_2003",
  "vocEdu2001", "HighEdu2001", "SecEdu2001", "emp_agr_cult", "emp_Ind",
  "emp_Trade", "emp_HQ", "emp_PS_EH", "emp_Cr_CS", "emp_RE_priv", "IV","finstrength10", "borderGER",
  "distLinz25"
)




######################################################

############ LM with all possible endogenes explained by true exogenes

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




colnames(P_WX) <- c(paste0(endogenes, "_IV"),"borderGER", "distLinz25", "IV")
X_names_IV <- colnames(P_WX)[-c(ncol(P_WX),(ncol(P_WX)-1))]
formula_str <- paste("Yfpoe", "~", paste(X_names_IV, collapse = " + "))
fml <- as.formula(formula_str)
LM_TSLS <- lm(fml, data = P_WX)



X <- cbind(Int,X)

u <- as.vector(Yfpoe- X%*% LM_TSLS$coefficients)

se_rob <- SEs_rob(W=W,X=X,u=u)
Vcov_rob_TSLS <- vcov_rob_f(W,X,u)

##### for hausmann test: 
sig2 <- as.numeric(t(u)%*%u *(length(u)- length(LM_TSLS$coefficients)))
vcov_homoskedastic_lm_TSLS <-  solve(t(X)%*%W%*%solve(t(W)%*%W)%*%t(W)%*%X)
vcov_homoskedastic_lm_TSLS <- sig2 * vcov_homoskedastic_lm_TSLS



tab <- Table(LM_TSLS, se_rob)
tab <- round(tab, digits=3)
colnames(tab) <- c("beta", "95%-CI_low","95%-CI_up","p-value")
rownames(tab) <- c("Intercept","Asylum seekers in community (2015)","Population (2011)", "Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)","Direct border with Germany", "Linear distance to Linz <25km")

kable(tab, caption = "Second Stage: corresponding regression" ,format = "latex",booktabs = TRUE,linesep = '') 

summary(LM_TSLS)$r.squared 
sum(summary(LM_TSLS)$df[c(1,2)])



##############


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




#######################
#### Table for: F tests for projections, and test on endogeneity


Instruments <- exogenes[1:17]
Instruments[16] <- paste0(Instruments[16],"TRUE")
hyp <- paste0(Instruments, " = 0")
t_K_P_F <- numeric(0)
C_D_Ftest <- numeric(0)
R2 <- numeric(0)
# for the wald test
R <- matrix(0, nrow = length(Instruments), ncol=(length(exogenes)+1) )
for(j in 2: (nrow(R)+1)){
  R[j-1,j] <- 1
}



for(i in 1:length(models)){
  robust_vcov <- vcovHC(models[[i]], type ="HC0")
  beta <- coef(models[[i]])
  
  C_D_Ftest <- c(C_D_Ftest, linearHypothesis(models[[i]], hyp)$F[2])
  t_K_P_F <- c(t_K_P_F, linearHypothesis(models[[i]], hyp,vcov = robust_vcov, test="F")$F[2])
  R2 <- c(R2,summary(models[[i]])$r.squared)
}


# Hansen J-Test  on endogeneity of the instruments

HansenJTest <- function(u, W,X) {
  n <- nrow(W)
  gbar <- colMeans(W * u) 
  S_hat <- t(W * u) %*% (W * u) / n  
  
  J <- n * t(gbar) %*% solve(S_hat) %*% gbar
  df <- ncol(W) - ncol(X)  
  p_value <- 1 - pchisq(J, df)
  
  return(list(J_stat = as.numeric(J),df = df,p_value = p_value))
}

HansenJTest(u,W,X)


tableF <- data.frame(t_K_P_F, C_D_Ftest,R2)
tableF <- round(tableF, digits = 2)
colnames(tableF) <- c("Kleibergen-Papp rk Wald F","Cragg-Donald Wald F", "Multiple R2")
rownames(tableF) <- c("Asylum seekers in community (2015)","Population (2011)", "Population growth in % (2001-11)", "Population growth in % (2011-14)", "Share of women in the population (2011)", "Share of population below 30 years (2011)", "Share pop. vocational education (2011)", "Share pop. higher education (2011)","Share pop. secondary education (2011)",  "Unemployment rate (2011)", "Population share of foreigners (2011)","Financial strength of community (2011)")


kable(tableF, caption = "Tests on Instrument strength" ,format = "latex",booktabs = TRUE,linesep = '')


