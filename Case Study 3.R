
                           #  Case study 3 - Logistic Regression

#  Getting the data:
mydata<- read.csv("E:\\Analytixlabs\\Module 6 (Data science using R)\\Case Studies\\Case study 3 - Logistic Regression\\Proactive Attrition Management-Logistic Regression Case Study.csv")
mydata$RETCALLS<- NULL
mydata$RETCALL<- NULL
#  We've deleted these two variables from our dataset beacuse they indicate that the customer
#  has already churned, so there is no point using these variables in predicting whether
# the customer will churn or not
mydata$INCOME[mydata$INCMISS==1]<- NA
mydata$INCMISS<- NULL
mydata$SETPRC[mydata$SETPRCM==1]<- NA 
mydata$SETPRCM<- NULL
mydata$CALIBRAT<- NULL
   
    #  Step 1: Explanatory Data analysis and Data cleaning/preparation
#     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

#  Getting all the continuous variables
names(mydata)
var_con<- c("REVENUE","MOU","RECCHRGE","DIRECTAS","OVERAGE","ROAM","CHANGEM","CHANGER",
            "DROPVCE","BLCKVCE","UNANSVCE","CUSTCARE","THREEWAY","MOUREC","OUTCALLS",
            "INCALLS","PEAKVCE","OPEAKVCE","DROPBLK","CALLFWDV","CALLWAIT","MONTHS",
            "UNIQSUBS","ACTVSUBS","PHONES","MODELS","EQPDAYS","AGE1","AGE2","RETACCPT",
            "REFER","INCOME","CREDITAD","SETPRC")

# User defined function for calculating descriptives

udf<- function(x){
  n<-length(x)
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<- mean(a)
  max<- max(a)
  min<- min(a)
  p1<-quantile(a,0.01)
  p5<- quantile(a,0.05)
  p95<- quantile(a,0.95)
  p99<-quantile(a,0.99)
  
  return(c(count=n,nmiss=nmiss,mean=m,max=max,min=min,P1=p1,P5=p5,P95=p95,P99=p99))  
}
options(scipen = 999)
stats_con<- data.frame(t(apply(mydata[var_con],2,udf)))
write.csv(stats_con,"Stats Con.csv")
#  The variable 'SETPRC' has lot of missing values,more than 50% of data is missing
# so we'll drop this variable
mydata$SETPRC<- NULL
var_con<- var_con[var_con!="SETPRC"]

 #  Outlier Treatment:
udf2<- function(x){
  p5<- quantile(x,0.05,na.rm = T)
  p95<- quantile(x,0.95,na.rm = T)
  x[x<p5]<- p5   # Any value less than p5 are treated as Outlier
  x[x>p95]<- p95 # Any value greater than p95 are treated as Outlier
  return(x)
}
mydata[var_con]<- data.frame(apply(mydata[var_con],2,udf2))

#  Missing value treatment:
mydata$REVENUE[is.na(mydata$REVENUE)]<- 58.8539614
mydata$MOU[is.na(mydata$MOU)]<- 525.7283924
mydata$RECCHRGE[is.na(mydata$RECCHRGE)]<- 46.87649165
mydata$DIRECTAS[is.na(mydata$DIRECTAS)]<- 0.894801146
mydata$OVERAGE[is.na(mydata$OVERAGE)]<- 40.0953598
mydata$ROAM[is.na(mydata$ROAM)]<- 1.221526168
mydata$CHANGEM[is.na(mydata$CHANGEM)]<- -10.84646141
mydata$CHANGER[is.na(mydata$CHANGER)]<- -1.205925579
mydata$PHONES[is.na(mydata$PHONES)]<- 1.808616952
mydata$MODELS[is.na(mydata$MODELS)]<- 1.561790952
mydata$EQPDAYS[is.na(mydata$EQPDAYS)]<- 380.2656307
mydata$AGE1[is.na(mydata$AGE1)]<- 31.37511282
mydata$AGE2[is.na(mydata$AGE2)]<- 21.15771528
mydata$INCOME[is.na(mydata$INCOME)]<- 5.777698557

#  Getting all the categorical variables:

var_cat<- (!names(mydata) %in% var_con)
View(var_cat)

udf3<- function(x){
  n<- length(x)
  nmiss<- sum(is.na(x))
  return(c(n=n,nmiss=nmiss))
}  

stats_cat<- data.frame(t(apply(mydata[var_cat],2,udf3)))  
write.csv(stats_cat,"Stats cat.csv")
#  No missing values in the categorical variables except 'CHURNDEP'. But the missing values
# in 'CHURNDEP' are not missing actually, they are left blank delibrately so that we can 
# split the dataset into calibration and validation datasets using the missing values in
# this variable

##########################################################################################

           #  Step 2 - Variable Reduction
#           _ _ _ _ __ _ _ _ _ _ _ _ _ _ _ _

#  (1) Selection of significant categorical variables using Chi square test

chisq.test(mydata$CHURN,mydata$CHILDREN)
chisq.test(mydata$CHURN,mydata$CREDITA) # Significant
chisq.test(mydata$CHURN,mydata$CREDITAA) # Significant
chisq.test(mydata$CHURN,mydata$CREDITB) # Significant
chisq.test(mydata$CHURN,mydata$CREDITC) # Significant
chisq.test(mydata$CHURN,mydata$CREDITDE) # Significant
chisq.test(mydata$CHURN,mydata$CREDITGY)
chisq.test(mydata$CHURN,mydata$CREDITZ)
chisq.test(mydata$CHURN,mydata$PRIZMRUR) # Significant
chisq.test(mydata$CHURN,mydata$PRIZMUB) # Significant
chisq.test(mydata$CHURN,mydata$PRIZMTWN) # Significant
chisq.test(mydata$CHURN,mydata$REFURB) # Significant
chisq.test(mydata$CHURN,mydata$WEBCAP) # Significant
chisq.test(mydata$CHURN,mydata$TRUCK)
chisq.test(mydata$CHURN,mydata$RV)
chisq.test(mydata$CHURN,mydata$OCCPROF)
chisq.test(mydata$CHURN,mydata$OCCCLER)
chisq.test(mydata$CHURN,mydata$OCCCRFT)
chisq.test(mydata$CHURN,mydata$OCCSTUD)
chisq.test(mydata$CHURN,mydata$OCCHMKR)
chisq.test(mydata$CHURN,mydata$OCCRET) # Significant
chisq.test(mydata$CHURN,mydata$OCCSELF)
chisq.test(mydata$CHURN,mydata$OWNRENT) # Significant
chisq.test(mydata$CHURN,mydata$MARRYUN) # Significant
chisq.test(mydata$CHURN,mydata$MARRYYES)
chisq.test(mydata$CHURN,mydata$MARRYNO) # Significant
chisq.test(mydata$CHURN,mydata$MAILORD) # Significant
chisq.test(mydata$CHURN,mydata$MAILRES) # Significant
chisq.test(mydata$CHURN,mydata$MAILFLAG)
chisq.test(mydata$CHURN,mydata$TRAVEL)
chisq.test(mydata$CHURN,mydata$PCOWN)
chisq.test(mydata$CHURN,mydata$CREDITCD) # Significant
chisq.test(mydata$CHURN,mydata$NEWCELLY) # Significant
chisq.test(mydata$CHURN,mydata$NEWCELLN)
chisq.test(mydata$CHURN,mydata$MCYCLE)

#  The significant categorical variables we got after performing Chi Square test:
# CREDITA,CREDITAA,CREDITB,CREDITC,CREDITDE,PRIZMRUR,PRIZMUB,PRIZMTWN,REFURB,WEBCAP,
# OCCRET,OWNRENT,MARRYUN,MARRYNO,MAILORD,MAILRES,CREDITCD,NEWCELLY

#  The Potential Continuous variables are:

# REVENUE,MOU,RECCHRGE,DIRECTAS,OVERAGE,ROAM,CHANGEM,CHANGER,DROPVCE,BLCKVCE,UNANSVCE,
# CUSTCARE,THREEWAY,MOUREC,OUTCALLS,INCALLS,PEAKVCE,OPEAKVCE,DROPBLK,CALLWAIT,
# MONTHS,UNIQSUBS,ACTVSUBS,PHONES,MODELS,EQPDAYS,AGE1,AGE2,INCOME

mydata$CHURN<- as.factor(mydata$CHURN)

hist(sqrt(mydata$REVENUE))
mydata$REVENUE1<- sqrt(mydata$REVENUE)

hist(sqrt(mydata$MOU))
mydata$MOU1<- sqrt(mydata$MOU)

hist(sqrt(mydata$EQPDAYS))
mydata$EQPDAYS1<- sqrt(mydata$EQPDAYS)

fit1<- glm(CHURN ~ REVENUE1+MOU1+RECCHRGE+DIRECTAS+OVERAGE+ROAM+CHANGEM+CHANGER+DROPVCE+
             BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+
             DROPBLK+CALLWAIT+MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+EQPDAYS1+AGE1+
             AGE2+INCOME+CREDITA+CREDITAA+CREDITB+CREDITC+CREDITDE+
             PRIZMRUR+PRIZMUB+PRIZMTWN+REFURB+OCCRET+OWNRENT+MARRYUN+MARRYNO+MAILORD+
             MAILRES+CREDITCD+NEWCELLY,data = mydata,family = "binomial")

summary(fit1)

#  Stepwise Linear regression:
library(MASS)
step<- stepAIC(fit1,direction = "both")
ls(step)
step$anova

#  The final model we get after applying stepAIC method:
#  CHURN ~ REVENUE1 + MOU1 + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
#          CHANGER + DROPVCE + BLCKVCE + CUSTCARE + THREEWAY + INCALLS + PEAKVCE + 
#          OPEAKVCE + CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + 
#          PHONES + EQPDAYS1 + AGE1 + INCOME + CREDITAA + CREDITB + CREDITC + 
#          CREDITDE + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + MARRYNO + 
#          MAILRES + CREDITCD + NEWCELLY
##########################################################################################

       #  Building the final Model
#       _ _ _ _ _ _ _ _ _ _ _ _ _ _  _

mydata$CREDITAA<- as.factor(mydata$CREDITAA)
mydata$CREDITB<- as.factor(mydata$CREDITB)
mydata$CREDITC<- as.factor(mydata$CREDITC)
mydata$CREDITDE<- as.factor(mydata$CREDITDE)
mydata$PRIZMRUR<- as.factor(mydata$PRIZMRUR)
mydata$PRIZMUB<- as.factor(mydata$PRIZMUB)
mydata$PRIZMTWN<- as.factor(mydata$PRIZMTWN)
mydata$REFURB<- as.factor(mydata$REFURB)
mydata$MARRYNO<- as.factor(mydata$MARRYNO)
mydata$MAILRES<- as.factor(mydata$MAILRES)
mydata$CREDITCD<- as.factor(mydata$CREDITCD)
mydata$NEWCELLY<- as.factor(mydata$NEWCELLY)

#  Data Partition

library(dplyr)
calibration<- filter(mydata,!is.na(CHURNDEP))
prop.table(table(calibration$CHURN))
calibration$CHURNDEP<- NULL

validation<- filter(mydata,is.na(CHURNDEP))
prop.table(table(validation$CHURN))
validation$CHURNDEP<- NULL

fit2<- glm(CHURN ~ REVENUE1 + MOU1 + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
                   CHANGER + DROPVCE + BLCKVCE + CUSTCARE + THREEWAY + INCALLS + PEAKVCE + 
                   OPEAKVCE + CALLWAIT + MONTHS + UNIQSUBS + ACTVSUBS + 
                   PHONES + EQPDAYS1 + AGE1 + INCOME + CREDITAA + CREDITB + CREDITC + 
                   CREDITDE + PRIZMRUR + PRIZMUB + PRIZMTWN + REFURB + MARRYNO + 
                   MAILRES + CREDITCD + NEWCELLY,data = calibration,family = binomial(logit))
summary(fit2)
library(car)
vif(fit2)

#  Building model with significant variables:


final_model<- glm(CHURN ~ REVENUE1 + MOU1 + RECCHRGE + OVERAGE + ROAM + CHANGEM + CHANGER +
                    DROPVCE + BLCKVCE + THREEWAY + INCALLS + PEAKVCE +  
                    OPEAKVCE + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + EQPDAYS1 + AGE1 +
                    INCOME + CREDITC +  CREDITDE + REFURB + MARRYNO + MAILRES +
                    NEWCELLY,data = calibration,family = binomial(logit))
summary(final_model)
vif(final_model)
# Concordance:
source("E:\\Analytixlabs\\Module 6 (Data science using R)\\Class files\\Class 5\\Logistics\\Concordance.r")
Concordance(final_model)

#  Prediction on calibration and validation datasets:

prob<- predict(final_model,newdata = calibration,type = "response")
pred1<- ifelse(prob>0.5,1,0)
table(pred1,calibration$CHURN)
calibration<- cbind(calibration,prob)

prob<- predict(final_model,newdata = validation,type = "response")
pred2<- ifelse(prob>0.5,1,0)
table(pred2,validation$CHURN)
validation<- cbind(validation,prob)

     #  Decile Analysis
#     _ _ _ _ _ _ _ __ _ _

 # (i) For Calibration dataset

dec_loc<- quantile(calibration$prob,probs = seq(0.1,0.9,by=0.1))
calibration$decile<- findInterval(calibration$prob,c(-Inf,dec_loc,Inf))

calibration$CHURN<- as.integer(as.character(calibration$CHURN))

library(dplyr)
x<- group_by(calibration,decile)

cal_decile<- summarise(x,total_cnt= n(),
                       MaxP=max(prob),
                       MinP=min(prob),
                       churn_cnt=sum(CHURN),
                       non_churn_cnt=total_cnt-churn_cnt)

cal_decile<- arrange(cal_decile,desc(decile))
write.csv(cal_decile,"Calibration decile.csv")

#  (ii) For Validation dataset

dec_loc<- quantile(validation$prob,probs = seq(0.1,0.9,by=0.1))
validation$decile<- findInterval(validation$prob,c(-Inf,dec_loc,Inf))

validation$CHURN<- as.integer(as.character(validation$CHURN))

library(dplyr)
x<- group_by(validation,decile)

val_decile<- summarise(x,total_cnt= n(),
                       MaxP=max(prob),
                       MinP=min(prob),
                       churn_cnt=sum(CHURN),
                       non_churn_cnt=total_cnt-churn_cnt)

val_decile<- arrange(val_decile,desc(decile))
write.csv(val_decile,"Validation decile.csv")

# From the KS - score we get 0.51 as the cutoff probability for correctly classifying the 
# Churned and Non-Churned customers
library(caret)

calibration$pred_churn<- ifelse(calibration$prob>0.51,1,0)
calibration$CHURN<- as.factor(calibration$CHURN)
calibration$pred_churn<- as.factor(calibration$pred_churn)
confusionMatrix(calibration$pred_churn,calibration$CHURN,positive = "1")

validation$pred_churn<- ifelse(validation$prob>0.51,1,0)
validation$CHURN<- as.factor(validation$CHURN)
validation$pred_churn<- as.factor(validation$pred_churn)
confusionMatrix(validation$pred_churn,validation$CHURN,positive = "1")

############++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++############