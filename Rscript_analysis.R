#0.1 loading packages####
#R version 4.0.5 (2021-03-31) -- "Shake and Throw" (updated 12052021)
pacman::p_load(
  foreign,         
  psych, 
  
  haven, #to install stata/other formed data and to describe data
  readxl,
  
  dplyr,
  tidyr,
  
  gtsummary,
  
  ggplot2, #mask psych::%+%, alpha
  gridExtra,
  interactions,
  sjPlot,
  ggpubr,
  gridExtra,
  grid,
  forcats,
  RColorBrewer,
  grDevices,
  
  mice,
  
  lme4, #LMM
  lmerTest, #to show p-value and 95%CI for MLL
  broomExtra
)

options(max.print=1000000)
memory.limit(size=56000) 



#1.1 Preparing PISA data: loading ####
PISA2018_data <- as.data.frame(read_sas("cy07_msu_stu_qqq.sas7bdat", encoding = "latin1"),use.value.labels = FALSE)
describe(PISA2018_data)
View(PISA2018_data)



#1.2 Preparing PISA data: variable cleaning ####
#outcomes
#life satisfaction (subjective well-being): https://www.oecd-ilibrary.org/sites/c414e291-en/index.html?itemId=/content/component/c414e291-en
table(PISA2018_data$ST016Q01NA, useNA = "always") #missing=118802/Overall, how satisfied are you with your life as a whole these days?
#purpose of life (psychological well-being): https://www.oecd-ilibrary.org/sites/c414e291-en/index.html?itemId=/content/component/c414e291-en
table(PISA2018_data$EUDMO, useNA = "always") #composite score whose average is 0 and SD is 1 across OECD countries./missing=105309


#competitiveness
#competitiveness: https://www.oecd-ilibrary.org/sites/0d62bf6c-en/index.html?itemId=/content/component/0d62bf6c-en
table(PISA2018_data$COMPETE, useNA = "always") #composite score of attitudes towards competition whose average is 0 and SD is 1 across OECD countries. Positive values mean that students are more competitive.
PISA2018_data$COMPETE_mean <- ave(PISA2018_data$COMPETE, PISA2018_data$CNTRYID, FUN = function(x) mean(x, na.rm=T)) # Group averages over countries
PISA2018_data$COMPETE_cent <- I(PISA2018_data$COMPETE - PISA2018_data$COMPETE_mean)


#covariates
#age
summary(PISA2018_data$AGE)
table(is.na(PISA2018_data$AGE))
#gender
table(PISA2018_data$ST004D01T, useNA = "always") #missing n = 2
#household SES: https://www.oecd-ilibrary.org/sites/f7986824-en/index.html?itemId=/content/component/f7986824-en#sbox-II.2.1
#missing n = 14379
summary(PISA2018_data$ESCS)#a composite measure that combines into a single score the financial, social, cultural and human-capital resources available to students
#immigration status
#missing n = 32568
table(PISA2018_data$IMMIG, useNA = "always")#1=native,2=second generation,3=first generation
#sampling weights
summary(PISA2018_data$W_FSTUWT)


#country code
table(PISA2018_data$CNT)
#KSV (kosovo),QAZ(Baku (Azerbaijan)),QCI(B-S-J-Z (China)),QMR(Moscow Region (RUS)),QRT(Tatarstan (RUS)),TAP(Chinese Taipei)are not matched
#Kosovo->none (SRB, serbia can be assigned?),Azerbaijan:AZE, china:CHN,Russian:RUS,Chinese taipei=taiwan:TWN
PISA2018_data$countrycode <- PISA2018_data$CNT
PISA2018_data$countrycode[PISA2018_data$CNT=="QAZ"] <- "AZE"
PISA2018_data$countrycode[PISA2018_data$CNT=="QCI"] <- "CHN"
PISA2018_data$countrycode[PISA2018_data$CNT=="QMR"] <- "RUS"
PISA2018_data$countrycode[PISA2018_data$CNT=="QRT"] <- "RUS"
PISA2018_data$countrycode[PISA2018_data$CNT=="TAP"] <- "TWN"


#region code
PISA2018_data$regioncodeModi[PISA2018_data$countrycode %in% c("USA","CAN","CHL","MEX","BRA","COL","CRI","COM","PER","TTO","URY","DOM","PAN","ARG")] <- "NOrth/South America"
PISA2018_data$regioncodeModi[PISA2018_data$countrycode %in% c("AUT","BEL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA","LVA","LUX","NLD","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","GBR",
                                                              "ALB","BGR","HRV","MKD","GEO","KSV","LTU","MLT","MDA","MNE","ROU","RUS","BIH","BLR","BRN","QAZ","QMR","QRT","SRB","UKR","KAZ",
                                                              "ISR","TUR","DZA","CYP","JOR","LBN","QAT","TUN","ARE","SAU","MAR","AZE")] <- "Europe+Africa+Middle east"
PISA2018_data$regioncodeModi[PISA2018_data$countrycode %in% c("AUS","NZL","QCH","HKG","IDN","MAC","MYS","SGP","TAP","THA","VNM","PHL","QCI","JPN","KOR","CHN","TWN")] <- "Asia+Oceania"
table(PISA2018_data$regioncodeModi, PISA2018_data$countrycode, deparse.level = 2,useNA = "always")
table(PISA2018_data$regioncodeModi,useNA = "always")



#1.3 Preparing Gini data: loading from World Bank ####
Gini_data <- read_excel("Gini_coefficient.xlsx")
describe(Gini_data)
table(subset(PISA2018_data, CNT %in% Gini_data$`Country Code`)$CNT)
names(Gini_data)[5:14] <- c("YR2009","YR2010","YR2011","YR2012","YR2013","YR2014","YR2015","YR2016","YR2017","YR2018")
Gini_data[5:14][Gini_data[5:14]==".."] <- NA
Gini_data$Ginicoef <- ifelse(!is.na(Gini_data$YR2018),Gini_data$YR2018,
                             ifelse(is.na(Gini_data$YR2018)&!is.na(Gini_data$YR2017),Gini_data$YR2017,
                                    ifelse(is.na(Gini_data$YR2017)&!is.na(Gini_data$YR2016),Gini_data$YR2016,
                                           ifelse(is.na(Gini_data$YR2016)&!is.na(Gini_data$YR2015),Gini_data$YR2015,
                                                  ifelse(is.na(Gini_data$YR2015)&!is.na(Gini_data$YR2014),Gini_data$YR2014,
                                                         ifelse(is.na(Gini_data$YR2014)&!is.na(Gini_data$YR2013),Gini_data$YR2013,
                                                                ifelse(is.na(Gini_data$YR2013)&!is.na(Gini_data$YR2012),Gini_data$YR2012,
                                                                       ifelse(is.na(Gini_data$YR2012)&!is.na(Gini_data$YR2011),Gini_data$YR2011,
                                                                              ifelse(is.na(Gini_data$YR2011)&!is.na(Gini_data$YR2010),Gini_data$YR2010,
                                                                                     ifelse(is.na(Gini_data$YR2010)&!is.na(Gini_data$YR2009),Gini_data$YR2009,NA))))))))))
Gini_data$Ginicoef <- as.numeric(Gini_data$Ginicoef) #take most recently measured available data
table(Gini_data$Ginicoef, useNA = "always")
names(Gini_data)[4] <- "Countrycode"
Gini_data_small <- subset(Gini_data, select = c("Countrycode","Ginicoef"))



#1.4 merging PISA and GINI data ####
PISA_count <- merge(PISA2018_data, Gini_data_small, by.x = "countrycode", by.y = "Countrycode", all.x = T)
describe(PISA_count)



#1.5 making sub-sample with will-be-used data ####
ANAL_PISA2018_data <- subset(PISA_count, select = c(ST016Q01NA,EUDMO,COMPETE,PERCOMP,COMPETE_mean,COMPETE_cent,
                                                    AGE,ST004D01T,ESCS,PA042Q01TA,MISCED,FISCED,BMMJ1,BFMJ2, #age,sex,SES,income,m_edu,f_edu,m_occu,f_occu
                                                    IMMIG,W_FSTUWT,countrycode,regioncodeModi,Ginicoef))



#2.1 flow chart ####
#with data on competitiveness, purpose of life, life satisfaction, Gini-coefficient
ANAL_data_PISA <- ANAL_PISA2018_data %>% subset(!is.na(COMPETE)&!is.na(PERCOMP)&!is.na(EUDMO)&!is.na(ST016Q01NA)) #n=408235
ANAL_data_PISA <- ANAL_data_PISA %>% subset(!is.na(Ginicoef)) #n=347934, 60 countries



#2.2 imputation ####
describe(ANAL_data_PISA)
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(ANAL_data_PISA) 
str(ANAL_data_PISA, list.len=ncol(ANAL_data_PISA))
#exclude covariates missing more than 90 %
imp_data <- ANAL_data_PISA %>% subset(select = c(ST016Q01NA,EUDMO,COMPETE,PERCOMP,COMPETE_mean,COMPETE_cent,
                                                 AGE,ST004D01T,ESCS,MISCED,FISCED,BMMJ1,BFMJ2,
                                                 IMMIG,W_FSTUWT,countrycode,regioncodeModi,Ginicoef))
propmiss(imp_data)

inlist <- c("COMPETE","PERCOMP","ST016Q01NA","EUDMO","ST004D01T","AGE") # we only include the outcome and exposure and key factors for prediction
outlist <- c("COMPETE_mean","COMPETE_cent","W_FSTUWT","regioncodeModi") 
pred <- quickpred(imp_data, include = inlist, exclude = outlist)
View(pred)
rowSums(pred) # a number of predictors :10 to 11

#imp <- mice(imp_data,m = 3, print = T, maxit = 5, pred = pred, seed = 30101995)
imp <- mice(imp_data,m = 50, print = T, maxit = 25, pred = pred, seed = 30101995)
imp$loggedEvents # NULL
View(imp$predictorMatrix)
summary(imp)
plot(imp) # Convergence is diagnosed when the variance between different sequences is no larger than the variance within each individual sequence.
#the variance between the imputation chains is almost equal to the variance within the chains, which indicates healthy convergence.
imp



#3.1 description ####
#3.1.1 sample demographics (table.1)
table_data <- as.data.frame(imp[[1]])
table_data$MISCEDcat <- ifelse(table_data$MISCED<3,"Low",
                               ifelse(table_data$MISCED==3|table_data$MISCED==4,"Middle",
                                      ifelse(table_data$MISCED==5|table_data$MISCED==6,"High",NA)))
table(table_data$MISCED,table_data$MISCEDcat,deparse.level = 2,useNA = "always")
table_data$FISCEDcat <- ifelse(table_data$FISCED<3,"Low",
                               ifelse(table_data$FISCED==3|table_data$FISCED==4,"Middle",
                                      ifelse(table_data$FISCED==5|table_data$FISCED==6,"High",NA)))
table(table_data$FISCED,table_data$FISCEDcat,deparse.level = 2,useNA = "always")

fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result}

t <- 
  tbl_summary(
    table_data[,c("AGE","ST004D01T","MISCEDcat","FISCEDcat","BMMJ1","BFMJ2","IMMIG","regioncodeModi")], # split table by group
    missing = "ifany", # don't list missing data separately
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ c(1, 2),
                  all_categorical() ~ c(0, 1))
  ) %>%
  add_n() %>% # add column with total number of non-missing observationst for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 
t



#3.1.2 relationship bw country competitiveness and Gini index (by country, colored by Gini index?) (Figure.1)
plot_data <- imp[[1]]
hist(plot_data$Ginicoef,100)
hist(plot_data$COMPETE_mean,100)
corr.test(subset(plot_data,select=c(Ginicoef,COMPETE_mean))) #r=0.35
plot_data <- plot_data[!duplicated(plot_data$countrycode),]
png("Plot_competeGini.png", width = 14, height = 21, units = "cm", res = 600)
plot_data %>%
  mutate(countrycode = fct_reorder(countrycode,desc(COMPETE_mean))) %>%
  ggplot(aes(x=countrycode, y=COMPETE_mean, fill=Ginicoef)) +
  geom_col(position = "identity", colour = "black", size = 0.25) +
  scale_fill_gradientn(colors = brewer.pal(9,"YlGnBu")) +
  coord_flip() +
  xlab("") +
  ylab("Competitiveness") +
  labs(fill = "Gini index") +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white")) 
dev.off()



#3.1.3 region/sex mean of competitiveness and Gini index
aggregate(plot_data$COMPETE_mean, list(plot_data$regioncodeModi), FUN=mean)
aggregate(plot_data$COMPETE_mean, list(plot_data$regioncodeModi), FUN=SD)
aggregate(plot_data$Ginicoef, list(plot_data$regioncodeModi), FUN=mean)
aggregate(plot_data$Ginicoef, list(plot_data$regioncodeModi), FUN=SD)

aggregate(plot_data$COMPETE_mean, list(plot_data$ST004D01T), FUN=mean)
aggregate(plot_data$COMPETE_mean, list(plot_data$ST004D01T), FUN=SD)
aggregate(plot_data$Ginicoef, list(plot_data$ST004D01T), FUN=mean)
aggregate(plot_data$Ginicoef, list(plot_data$ST004D01T), FUN=SD)



#3.2 analysis overall effects ####
#3.2.1 competitiveness and well-being (individual, country) (Table.2)
imp %>% with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()



#3.3 analysis stratification
#3.3.1 by regions
imp %>% with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)*as.factor(regioncodeModi)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% with(lmer(scale(EUDMO)~scale(COMPETE_cent)*as.factor(regioncodeModi)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="NOrth/South America") %>% as.mids() %>% #only N/S America
  with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="NOrth/South America") %>% as.mids() %>% #only N/S America
  with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="Europe+Africa+Middle east") %>% as.mids() %>% #only Europe+Africa+Middle east
  with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="Europe+Africa+Middle east") %>% as.mids() %>% #only Europe+Africa+Middle east
  with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="Asia+Oceania") %>% as.mids() %>% #only Asia+Oceania
  with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% complete(action="long",include=T) %>% filter(regioncodeModi=="Asia+Oceania") %>% as.mids() %>% #only Asia+Oceania
  with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()


#3.3.2 competitiveness and well-being effect modification by country inequality (Gini index) (S.Table.1)
imp_long <- complete(imp, action = "long", include = T)
imp_long$Ginicat2 <- ifelse(imp_long$Ginicoef < quantile(imp_long$Ginicoef,probs=0.2),1,
                            ifelse(imp_long$Ginicoef>=quantile(imp_long$Ginicoef,probs=0.2)&imp_long$Ginicoef<quantile(imp_long$Ginicoef,probs=0.8),2,
                                   ifelse(imp_long$Ginicoef>=quantile(imp_long$Ginicoef,probs=0.8),3,NA)))
table(imp_long$Ginicat2,imp_long$Ginicoef,deparse.level = 2,useNA = "always")

imp_new <- as.mids(imp_long)  

imp %>% with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)*scale(Ginicoef)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% with(lmer(scale(EUDMO)~scale(COMPETE_cent)*scale(Ginicoef)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp_new %>% with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)*as.factor(Ginicat2)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp_new %>% with(lmer(scale(EUDMO)~scale(COMPETE_cent)*as.factor(Ginicat2)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()


result_Gini_sati <- NULL;
for(i in 1:3){
  result <- imp_new %>% complete(action="long",include=T) %>% filter(Ginicat2==i) %>% as.mids() %>%
    with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
  mean  <- result[2,]$estimate
  lower <- result[2,]$conf.low
  upper <- result[2,]$conf.high
  pvalue<- result[2,]$p.value
  data  <- data.frame(mean,lower,upper,pvalue)
  result_Gini_sati <- rbind(result_Gini_sati,data)
}

result_Gini_life <- NULL;
for(i in 1:3){
  result <- imp_new %>% complete(action="long",include=T) %>% filter(Ginicat2==i) %>% as.mids() %>%
    with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ST004D01T+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
  mean  <- result[2,]$estimate
  lower <- result[2,]$conf.low
  upper <- result[2,]$conf.high
  pvalue<- result[2,]$p.value
  data  <- data.frame(mean,lower,upper,pvalue)
  result_Gini_life <- rbind(result_Gini_life,data)
}

# create a plot for linear regression
plot_data <- complete(imp_new,1) %>% mutate(ST016Q01NA   = scale(ST016Q01NA),
                                            EUDMO        = scale(EUDMO),
                                            COMPETE_cent = scale(COMPETE_cent),
                                            Ginicat2     = as.factor(Ginicat2))
p_sati <- ggplot(plot_data, aes(x = COMPETE_cent, y = ST016Q01NA, colour = Ginicat2)) +
  geom_smooth(method = lm, se = T, fullrange = TRUE) +
  scale_colour_manual(values = brewer.pal(3,"YlGnBu"),labels = c("Equal","Middle", "Inequal")) +
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "none") + 
  labs(color="",x ="Individual competitiveness", y = "Life satisfaction")+
  guides(color=guide_legend(nrow=5,byrow=F,reverse=TRUE))
p_life <- ggplot(plot_data, aes(x = COMPETE_cent, y = EUDMO, colour = Ginicat2)) +
  geom_smooth(method = lm, se = T, fullrange = TRUE) +
  scale_colour_manual(values = brewer.pal(3,"YlGnBu"),labels = c("Equal","Middle", "Inequal")) +
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "none") + 
  labs(fill="",x ="Individual competitiveness", y = "Purpose of life")+
  guides(color=guide_legend(nrow=5,byrow=F,reverse=TRUE))

png("Plot_linear.png", width = 20, height = 8, units = "cm", res = 600)
ggarrange(p_sati,p_life,common.legend = T, legend = "right", labels = c("A", "B"),ncol = 2)
dev.off()


#3.3.3 by sex
imp %>% with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)*as.factor(ST004D01T)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% with(lmer(scale(EUDMO)~scale(COMPETE_cent)*as.factor(ST004D01T)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp %>% complete(action="long",include=T) %>% filter(ST004D01T==1) %>% as.mids() %>% #only female
  with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% complete(action="long",include=T) %>% filter(ST004D01T==1) %>% as.mids() %>% #only female
  with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()

imp %>% complete(action="long",include=T) %>% filter(ST004D01T==2) %>% as.mids() %>% #only male
  with(lmer(scale(ST016Q01NA)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()
imp %>% complete(action="long",include=T) %>% filter(ST004D01T==2) %>% as.mids() %>% #only male
  with(lmer(scale(EUDMO)~scale(COMPETE_cent)+scale(COMPETE_mean)+AGE+ESCS+IMMIG+(1|countrycode),REML = T,weights = W_FSTUWT)) %>% pool() %>% tidy_parameters()



#3.3 sensitivity analysis ####
#3.3.1 excluded vs included
# we exlcluded those without data on competitiveness, well-being, Gini-coefficient
# To use intact data (no filtering, no selecting)
all_data <- subset(PISA_count, select = c(CNTSTUID,AGE,ST004D01T,ESCS,PA042Q01TA,MISCED,FISCED,BMMJ1,BFMJ2, #age,sex,SES,income,m_edu,f_edu,m_occu,f_occu
                                          IMMIG,countrycode,regioncodeModi,COMPETE,PERCOMP,EUDMO,ST016Q01NA,Ginicoef))
include_data <- all_data %>% subset(!is.na(COMPETE)&!is.na(PERCOMP)&!is.na(EUDMO)&!is.na(ST016Q01NA)&!is.na(Ginicoef)) #n=408235
all_data$included <- ifelse(c(all_data$CNTSTUID)%in%c(include_data$CNTSTUID),"included","excluded")
table(all_data$included,useNA = "always")
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}
table_data <- 
  tbl_summary(
    all_data[,c("AGE","ST004D01T","ESCS","PA042Q01TA","MISCED","FISCED","BMMJ1","BFMJ2", 
                "IMMIG","countrycode","regioncodeModi","included")],
    by = included, # split table by group
    missing = "ifany", # don't list missing data separately
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ c(1, 2),
                  all_categorical() ~ c(0, 1))
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p((test = list(all_categorical() ~ "fisher.test.simulate.p.values")),
        #default: "kruskal.test" for continuous, "chisq.test" for categorical with all expected cell counts >=5, and "fisher.test" for categorical with any <5.
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 
table_data
