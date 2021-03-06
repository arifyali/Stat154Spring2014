#################################### Problem 1A #################################### 
sp500Components = read.csv("~/Dropbox/School/Statistics/Stat 154 Spring 2014/HW1/bf4024f5c75fc062.csv")

DailyReturns = c()
tick = unique(sp500Components$TICKER)
for(i in 1:length(unique(sp500Components$TICKER))){
  Ticker = sp500Components[sp500Components$TICKER == tick[i], ]
  uniquediff = (Ticker$PRC[2:length(Ticker$PRC)]/Ticker$PRC[1:(length(Ticker$PRC)-1)])-1
  DailyReturns = c(DailyReturns, 0, uniquediff)
}
sp500Components$DailyReturns = abs(DailyReturns)
sp500Components$PRC <- abs(sp500Components$PRC)
sp500Components$date <- as.factor(sp500Components$date)


tabulate(sp500Components$COMNAM)[which(tabulate(sp500Components$COMNAM) > 1343)] 

levels(sp500Components$COMNAM)[which(tabulate(sp500Components$COMNAM) > 1343)]

sp500Components$date[which(sp500Components$COMNAM=="C B S CORP NEW")]
sp500Components$date[which(sp500Components$COMNAM=="CHIPOTLE MEXICAN GRILL INC")]
sp500Components$date[which(sp500Components$COMNAM=="LOEWS CORP")] #visual checks

hurricane.sandy.indices = which(sp500Components$date=="20121029")
sp500Components.mod = sp500Components[-hurricane.sandy.indices,]#delete hurricane sandy dates
dates = levels(sp500Components.mod$date)[-1301] #pick the dates we want to use; delete the date of Sandy
sp500Components.mod$date = as.character(sp500Components.mod$date) #we've extracted the unique dates so we turn this back to a character vector for technical reasons
comnames = levels(sp500Components.mod$COMNAM)[which(tabulate(sp500Components.mod$COMNAM) == 1342)]

imp.corp = data.frame(sp500Components.mod[sp500Components.mod$COMNAM == comnames[1],"PRC"])
for(i in 2:length(comnames)){
  imp.corp[,i] = sp500Components.mod[sp500Components.mod$COMNAM == comnames[i],"PRC"]
}
names(imp.corp) = comnames 
imp.corp$dates = dates
SP500 = read.csv("~/Dropbox/School/Statistics/Stat 154 Spring 2014/HW1/sp500Data.csv")
library("RSQLite")
m = dbDriver("SQLite")
tfile = tempfile()
con=dbConnect(m, dbname = tfile)
dbWriteTable(con, "impcorp", imp.corp)
dbWriteTable(con, "SP500", SP500)

SP500.combined = dbGetQuery(con, "SELECT * FROM impcorp AS i,SP500 AS sp WHERE i.dates = sp.Calendar_Date;")
SP500.combined = SP500.combined[, -c(430, 429, 428)]

library("glmnet")
sp.500.glm = glmnet(as.matrix(SP500.combined[,-428]),SP500.combined$SP_500_Level)

lambda = numeric()
for (i in 1:length(sp.500.glm$lambda)){
  lambda [i] = sum(coef(sp.500.glm, s = sp.500.glm$lambda[i])[,1]!=0)
}

which(lambda == 20)

Portfolio1a = coef(sp.500.glm, s = sp.500.glm$lambda[22])[,1][coef(sp.500.glm, s = sp.500.glm$lambda[22])[,1]!=0]
# (Intercept) AMERICAN_ELECTRIC_POWER_CO_INC       AMERIPRISE_FINANCIAL_INC                   AUTODESK_INC 
# 4.292976e+02                   1.716858e+00                   1.348732e-02                   4.639504e-05 
# BOEING_CO     CAPITAL_ONE_FINANCIAL_CORP      CINCINNATI_FINANCIAL_CORP DENTSPLY_INTERNATIONAL_INC_NEW 
# 8.278728e-01                   9.560155e-02                   4.662254e-01                   1.114951e+00 
# HARLEY_DAVIDSON_INC    HONEYWELL_INTERNATIONAL_INC        ILLINOIS_TOOL_WORKS_INC         INTERNATIONAL_PAPER_CO 
# 5.275814e-01                   1.046299e+00                   8.765892e-01                   2.004584e+00 
# MACERICH_CO                      MOLEX_INC          NEWELL_RUBBERMAID_INC                      NEWS_CORP 
# 1.359944e+00                   4.032228e+00                   4.046757e-02                   2.414746e+00 
# OMNICOM_GROUP_INC        PATTERSON_COMPANIES_INC      TOTAL_SYSTEM_SERVICES_INC                   WILLIAMS_COS 
# 4.905628e+00                   1.767719e-01                   1.155144e-01                   1.358354e+00
#################################### Problem 1B ####################################
lowerbound = seq(0, 1340, 60)[-23] + 1
upperbound = seq(0, 1340, 60)[-1]
Portfolio.60.days = list()
Portfolio.60.days.names = list()
for(i in 1:length(lowerbound)){
dates.60days = lowerbound[i]:upperbound[i]
sp.500.glm = glmnet(as.matrix(SP500.combined[dates.60days,-428]),SP500.combined$SP_500_Level[dates.60days])

lambda = numeric()
for (i in 1:length(sp.500.glm$lambda)){
  lambda [i] = sum(coef(sp.500.glm, s = sp.500.glm$lambda[i])[,1]!=0)
}

lambda.prime = lambda[order(abs(lambda-20))][1]
Portfolio1q = coef(sp.500.glm, s = sp.500.glm$lambda[lambda.prime])[,1][coef(sp.500.glm, s = sp.500.glm$lambda[lambda.prime])[,1]!=0]
Portfolio.60.days[[i]] = Portfolio1q
Portfolio.60.days.names[[i]] = names(Portfolio1q)
}
#################################### Problem 1C #################################### 
#################################### Problem 1D.A ####################################
imp.corp.DR = data.frame(sp500Components.mod[sp500Components.mod$COMNAM == comnames[1],"DailyReturns"])
for(i in 2:length(comnames)){
  imp.corp.DR[,i] = sp500Components.mod[sp500Components.mod$COMNAM == comnames[i],"DailyReturns"]
}
names(imp.corp.DR) = comnames 
imp.corp.DR$dates = dates
m = dbDriver("SQLite")
tfile = tempfile()
con=dbConnect(m, dbname = tfile)
dbWriteTable(con, "impcorp", imp.corp.DR)
dbWriteTable(con, "SP500", SP500)

SP500.combined.DR = dbGetQuery(con, "SELECT * FROM impcorp AS i,SP500 AS sp WHERE i.dates = sp.Calendar_Date;")
SP500.combined.DR = na.omit(SP500.combined.DR[, -c(430, 429, 428)])

X.sp = na.omit(as.matrix(SP500.combined.DR[,-428]))
sp.500.glm = glmnet(X.sp,SP500.combined.DR$SP_500_Level)

lambda = numeric()
for (i in 1:length(sp.500.glm$lambda)){
  lambda [i] = sum(coef(sp.500.glm, s = sp.500.glm$lambda[i])[,1]!=0)
}
which(lambda == 21)

Portfolio1d.a = coef(sp.500.glm, s = sp.500.glm$lambda[11])[,1][coef(sp.500.glm, s = sp.500.glm$lambda[11])[,1]!=0]

# (Intercept)                     A_E_S_CORP         AMERISOURCEBERGEN_CORP               APOLLO_GROUP_INC 
# 1292.29674                       57.35684                     -212.58984                     -287.42818 
# BAKER_HUGHES_INC     CAPITAL_ONE_FINANCIAL_CORP      CH_ROBINSON_WORLDWIDE_INC                      COACH_INC 
# 127.00013                      -98.27150                     -496.20672                       14.06000 
# DU_PONT_E_I_DE_NEMOURS___CO               FLIR_SYSTEMS_INC      INTERPUBLIC_GROUP_COS_INC              K_L_A_TENCOR_CORP 
# -187.26383                      116.30060                     -135.08641                     -693.38550 
# MARRIOTT_INTERNATIONAL_INC_NEW                 MASTERCARD_INC           MCKESSON_H_B_O_C_INC          NABORS_INDUSTRIES_LTD 
# -93.83880                     -165.52266                     -551.94695                     -215.63191 
# PRICELINE_COM_INC                    SAFEWAY_INC                    SNAP_ON_INC             SPRINT_NEXTEL_CORP 
# 28.24306                      269.44416                       18.58140                     -271.10870 
# WESTERN_UNION_CO 
# -597.51358 
#################################### Problem 1D.B ####################################
#################################### Problem 1E #################################### 
#################################### Problem 3 #################################### 
sa.HeartDisease = read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data",
                             sep=",", head=T, row.names=1)
sa.HeartDisease$famhist = as.numeric(sa.HeartDisease$famhist)
pairs(sa.HeartDisease[-10],pch=21,bg=c("red","green")[factor(sa.HeartDisease$chd)])

n = sample.int(dim(sa.HeartDisease)[1], size=300)
sa.train= sa.HeartDisease[n,]
sa.test = sa.HeartDisease[-n,]

library("MASS")
###LDA
myX = sa.train[, c(2,3,5,9)]
sa.lda = lda(myX, sa.train[,10])

sa.predict.train <- predict(sa.lda,myX)$class
sa.predict.test <- predict(sa.lda,sa.test[,c(2,3,5,9)])$class
mean(sa.predict.train == sa.train$chd)
mean(sa.predict.test == sa.test$chd)

logReg.sa = glm(chd~tobacco+ldl+famhist+age,binomial, data = sa.train)
logReg.sa.pred.train = predict(logReg.sa,sa.train)
logReg.sa.pred.train[logReg.sa.pred.train>0]=1
logReg.sa.pred.train[logReg.sa.pred.train<=0]=0
mean(logReg.sa.pred.train == sa.train$chd)


logReg.sa.pred.test = predict(logReg.sa,sa.test)
logReg.sa.pred.test[logReg.sa.pred.test>0]=1
logReg.sa.pred.test[logReg.sa.pred.test<=0]=0
mean(logReg.sa.pred.test == sa.test$chd)
