##
rm(list = ls())
##
library(dfms)
library(MSwM)
library(feasts)
library(forecast)
library(readxl)
library(seasonal)
library(fpp2)
library(hpfilter)
library(bHP)
library(fpp2)
library(showtext)
library(midasr)
library(pretest)


fac_stata = read.table('Data\\factor_Stata.txt', header = TRUE)
fac_stata = fac_stata[2:dim(fac_stata)[1],1]

y = read_xlsx('Data\\newnewnewdata.xlsx')

yy.ts = ts(y, frequency = 12, start = c(2000, 1))
rg = seq(61,dim(y)[1])
# 2,3,4,9不错;2,6,9,11,12,13
# 2,3,4,7,9,10
# 8 很重要
yy.ts = yy.ts[rg,c(2,3,4,8,9,10)]
yy.T = y[rg, 1]

yy = scale(yy.ts)

# y = diff(y)

# P0 = matrix(data = c(0.95,0.08,0.05,0.92), nrow = 2, ncol = 2)
res <- DFM(yy, r=1,
           p=2,
           max.iter = 3000,
           em.method = "BM")
QML_EM.factor = res$F_qml
QML_EM.factor = ts(QML_EM.factor, frequency = 12, start = c(2005, 1))
## X13 adjustment
f.res <- final(seas(QML_EM.factor))
##############
## stl #####
f.stl <- stl(QML_EM.factor[,1], s.window="periodic")
f.res <- f.stl$time.series[,'remainder']
###################
## hp ####
f.hp <- hp2(QML_EM.factor)
f.res <- QML_EM.factor - f.hp[,1]
###############
##### bHP ##########
f.bhp <- BoostedHP(QML_EM.factor)
f.res <- f.bhp$cycle
######################
tt = seq(from=1, to=dim(yy.T)[1], by=1)
ft.reg <- lm(QML_EM.factor[,1] ~ tt)
f.res <- ft.reg$residuals

## -- hw method
f.res <- f.res - decompose(QML_EM.factor)$seasonal

plot(f.res, col = "red", type='l')
lines(QML_EM.factor[,1], col="blue")

# Raw series
###########################
f.res <- fac_stata
f.res <- QML_EM.factor
############################
### msmFit Begin #####
f.f_lag = embed(f.res, 2) 
colnames(f.f_lag) = c("Ft", "Flag1")
f.f_lag = as.data.frame(f.f_lag)
model = lm(Ft~Flag1, f.f_lag)
res.ar = msmFit(model, k = 3, sw = c(TRUE,FALSE,FALSE),
                p=0,
                control=list(maxiter=20000,
                             maxiterOuter=200,
                             maxiterInner=300))

plotProb(res.ar, 1)
summary(res.ar)
AIC(res.ar)
##### msmFit end #######
########################

library(scoringRules)
res.bay <- ar_ms(f.res, beta_switch=TRUE, variance_switch = FALSE,
      identification_constraint = "mean")
bay.prob = colMeans(res.bay$probs)
plot(mm[-1],bay.prob, type = 'l')

########################


## Months ##
mm = seq(2000+1/12, 2024+3/12, 1/12)
mm = mm[rg]
TT = length(mm)
# plot(mm[2:length(mm)], res$ProbS[2:266,1], type='l')

# 画图
library(ggplot2)
##########
icpt <- res.ar@Coef[["(Intercept)"]]
plot.data<-res.ar@Fit@smoProb[,order(icpt)]
num_regime = 3
Regime = apply(plot.data, 1, function(x) num_regime-which.max(x))
plot.data = cbind(mm, plot.data, Regime, f.res)
colnames(plot.data) <- c('Months','State2', 'State1', 'State0',
                         'Regime','Factor')
plot.data = as.data.frame(plot.data)


s2.index = which(plot.data$Regime==2)
s2.index.end = s2.index+1
m2.min = mm[s2.index]
m2.max = mm[s2.index.end]

refcic <- read.csv('Data/refcic.csv')
refcic = refcic[rg, 2]
refcic = refcic[(1-dim(plot.data)[1]+length(refcic)):length(refcic)]

plot.data[,'recession'] = refcic

dev.off()
p <- ggplot(data = plot.data)+
  geom_line(mapping = aes(x=Months, y=State2, colour="State2"))+
  geom_line(mapping = aes(x=Months,y=Regime,colour="Regime"))+
  geom_line(mapping = aes(x=Months,y=Factor,colour="Factor"))+
  geom_line(mapping = aes(x=Months,y=recession,colour="recession"))+
  annotate("rect", xmin=m2.min, xmax=m2.max, ymin=0, ymax=2, alpha=0.7, fill="pink")+
  scale_color_manual(name="图例",
    breaks=c('State2','Regime','Factor','recession'),
    values=c('red','blue','green','black'))+
  scale_x_continuous(limits=c(2005,2024.5),breaks=seq(2005,2024,0.5))+
  theme(axis.text.x=element_text(angle=80,size=8))+
  ggtitle('State 2')
  
p

### msmFit END #########
########################


#########################
## fMarkovSwitching #####
#########################
library(fMarkovSwitching)

S <- c(1,0)	       # where to switch (in this case in the only indep)
k <- 3		         # number of states
distIn <- "Normal" #distribution assumption


dep <- f.res[-1]
lag.f <- f.res[-length(f.res)]
ones <- rep(1, length(dep))
indep <- cbind(ones,lag.f)
fMS.res <- MS_Regress_Fit(dep,indep,S,k)
mm = seq(2000+1/12, 2024+3/12, 1/12)
mm = mm[rg]
mm = mm[-1]

icpt <- fMS.res@Coeff[["indep_S"]]
plot.data <- fMS.res@smoothProb
num_regime = 3
Regime = apply(plot.data, 1, function(x) num_regime-which.max(x))
plot.data = cbind(mm, plot.data, Regime, f.res[-1])
# colnames(plot.data) <- c('Months', 'State2','State1', 'State0',
#                          'Regime','Factor')
if (num_regime==2){
  colnames(plot.data) <- c('Months', 'State1', 'State0',
                         'Regime','Factor')
}
if (num_regime==3){
  colnames(plot.data) <- c('Months','State2','State1','State0',
                           'Regime','Factor')
}
plot.data = as.data.frame(plot.data)


s2.index = which(plot.data$Regime==2)
s2.index.end = s2.index+1
m2.min = mm[s2.index]
m2.max = mm[s2.index.end]

refcic <- read.csv('Data/refcic.csv')
refcic = refcic[rg, 2]
if (num_regime==2){
  refcic[refcic>1]=1
}
refcic = refcic[(1-dim(plot.data)[1]+length(refcic)):length(refcic)]
plot.data[,'recession'] = refcic
rownames(plot.data) <- 1:nrow(plot.data)
## Draw 
dev.off()
font_add(family = "STSONG", regular = "C:/Windows/Fonts/STSONG.TTF")
if (num_regime==3){
p <- ggplot(data = plot.data)+
  geom_line(mapping = aes(x=Months, y=State2, colour="Pr(S_t=2)"))+
  geom_line(mapping = aes(x=Months,y=Regime,colour="区制"),family="STSONG")+
  geom_line(mapping = aes(x=Months,y=Factor,colour="因子"),family="STSONG")+
  scale_linetype_manual(values = c('twodash', 'longdash', 'dashed'))+
  geom_line(mapping = aes(x=Months,y=recession,colour="recession"))+
  annotate("rect", xmin=m2.min, xmax=m2.max, ymin=0, ymax=2, alpha=0.7, fill="pink")+
  scale_color_manual(name="",
                     breaks=c('Pr(S_t=2)','区制','因子','recession'),
                     values=c('red','blue','green','black'))+
  scale_x_continuous(limits=c(2005,2024.5),breaks=seq(2005,2024,0.5))+
  theme(axis.text.x=element_text(angle=80,size=8))
}
if (num_regime==2){
  p <- ggplot(data = plot.data)+
    geom_line(mapping = aes(x=Months, y=State1, colour="Pr(S_t=1)"))+
    geom_line(mapping = aes(x=Months,y=Regime,colour="区制"),family="STSONG")+
    geom_line(mapping = aes(x=Months,y=Factor,colour="因子"),family="STSONG")+
    scale_linetype_manual(values = c('twodash', 'longdash', 'dashed'))+
    geom_line(mapping = aes(x=Months,y=recession,colour="recession"))+
    annotate("rect", xmin=m2.min, xmax=m2.max, ymin=0, ymax=2, alpha=0.7, fill="pink")+
    scale_color_manual(name="",
                       breaks=c('Pr(S_t=1)','区制','因子','recession'),
                       values=c('red','blue','green','black'))+
    scale_x_continuous(limits=c(2005,2024.5),breaks=seq(2005,2024,0.5))+
    theme(axis.text.x=element_text(angle=80,size=8))
}
p

## Draw END fMarkovSwitching ####
#################################

## rMS #####
library(rMSWITCH)
library(Rcpp)
library(RcppArmadillo)


write.csv(plot.data,
          file = "Outputs/Final_Matrix_2.csv")
write.csv(f.res, row.names = FALSE, col.names = FALSE,
            file = "Outputs/factors.txt")

#################################################
plot.data <- readxl::read_excel("Outputs/画图.xlsx")
colnames(plot.data)[1]<-'月份'
dev.off()
############################################

#### Rolling Windows ###########
get.yq <- function(str){
  year = as.integer(substring(str,1,4))
  q = as.integer(substring(str,6))
  yq = year+q*3/12
  return(yq)
}

roll.t = "2022Q1"
end.t = "2024Q1"
begin.t = "2005Q2"
t.num = seq(2005+6/12, 2024+3/12, 3/12)

roll.t.num = get.yq(roll.t)

roll.t.index = which(t.num==roll.t.num)
nQ = (dim(plot.data)[1]-2)/3


#--------------------------------------
TT = dim(plot.data)[1]
GDP = readxl::read_excel('Data/CME_Qqgdp.xlsx')
GDP = GDP[GDP[,1]>'2005-03'&GDP[,1]<='2024-03',2]
GDP = as.numeric(unlist(GDP[,1]))
prob.m = read.csv('Data/TwoRegimeProb.csv', sep = ",", header = FALSE)
prob.m = as.matrix(prob.m)
prob.m = as.numeric(prob.m)[3:TT]
# We can read the data from results in matlab
plot.data = read.csv('Data/M_final_matrix.csv', sep = ",", header = FALSE)
plot.data = apply(plot.data, 2, as.numeric)

#--------------------------------------

temp = matrix(prob.m, ncol = 3, byrow=TRUE)
prob.q = apply(temp,1,mean)
temp = matrix(plot.data[3:TT,2], ncol = 3, byrow=TRUE)
state2.q = apply(temp,1,mean)
temp = matrix(plot.data[3:TT,3], ncol = 3, byrow=TRUE)
state1.q = apply(temp,1,mean)
temp = matrix(plot.data[3:TT,4], ncol = 3, byrow=TRUE)
state0.q = apply(temp,1,mean)
temp = matrix(plot.data[3:TT,6], ncol = 3, byrow=TRUE)
factor.q = apply(temp,1,mean)
trend = seq(from = 1, to = length(GDP), by = 1)
#----
prob.rec = state2.q+state1.q
state2.m = plot.data[3:TT,2]
state1.m = plot.data[3:TT,3]
factor.m = plot.data[3:TT,6]
state2.m = ts(data = state2.m, start = c(2005, 4), frequency = 12)
state1.m = ts(data = state1.m, start = c(2005, 4), frequency = 12)
factor.m = ts(data=factor.m, start=c(2005,4), frequency=12)

prob.m = ts(data = prob.m, start=c(2005,4), frequency=12)
prob1.m = ts(data = state1.m, start=c(2005,4), frequency=12)
prob2.m = ts(data = state2.m, start=c(2005,4), frequency=12)
factor.m = ts(data=factor.m, start=c(2005,4), frequency=12)
factor.q = ts(data=factor.q, start=c(2005,2), frequency = 4)
GDP.q = ts(data=GDP, start=c(2005,2), frequency=4)
prob.q = ts(data=prob.q, start=c(2005,2), frequency=4)
prob1.q = ts(data=state1.q, start=c(2005,2), frequency=4)
prob2.q = ts(data=state2.q, start=c(2005,2), frequency=4)
trend = ts(data=trend, start=c(2005,2), frequency=4)


#----------------------------------------------------
h = 0
lag = 1
pred.GDP = rep(NA, nQ-roll.t.index-h)
ii=0


for (t.idx in roll.t.index:(nQ-h)){
  
  temp.f = factor.q[(2+h):t.idx]
  temp.lag.f = factor.q[(1+h):(t.idx-1)]
  temp.prob = prob.rec[(2+h):t.idx]
  temp.lag.prob = prob.rec[(1+h):(t.idx-1)]
  temp.lag.GDP = GDP[(1+h):(t.idx-1)]
  temp.GDP = GDP[(2*h+2):(t.idx+h)]
  temp.X = cbind(temp.f,
                 temp.lag.f,
                 temp.prob,
                 temp.lag.prob,
                 temp.lag.GDP)
  temp.X.df = data.frame(temp.X)
  if (t.idx > roll.t.index){
    ii = ii + 1
    pred.GDP[ii]=t(newX)%*%fit$coefficients
  }
  fit <- lm(temp.GDP~temp.f+temp.lag.f+temp.prob+temp.lag.prob+temp.lag.GDP)
  newX = cbind(1,
               factor.q[t.idx+1],
               factor.q[t.idx],
               prob.rec[t.idx+1],
               prob.rec[t.idx],
               GDP[t.idx])
  newX = data.frame(newX)
}
print(pred.GDP)

#### MIDAS 
library(midasr)
library(pretest)

#----
##################################################################
############## Divide the dataset. ###############################
##################################################################
for (hor in 0:2){
y.h0.start = c(2005,2)
y.h1.start = c(2005,3)
y.h2.start = c(2005,4)
xm.h0.end = c(2024,3)
xm.h1.end = c(2023,12)
xm.h2.end = c(2023,9)
xq.h0.end = c(2024,1)
xq.h1.end = c(2023,4)
xq.h2.end = c(2023,3)
if (hor==0){
  y.start = y.h0.start
  xm.end = xm.h0.end
  xq.end = xq.h0.end
}
if (hor==1){
  y.start = y.h1.start
  xm.end = xm.h1.end
  xq.end = xq.h1.end 
}
if (hor==2){
  y.start = y.h2.start
  xm.end = xm.h2.end
  xq.end = xq.h2.end 
}
fulldata <- list(GDP.q=window(GDP.q, start=y.start, end=c(2024,1)),
                 prob.m=window(prob.m, start=c(2005,4), end=xm.end),
                 factor.m=window(factor.m, start=c(2005,4), end=xm.end),
                 prob.q=window(prob.q, start=c(2005,2), end=xq.end),
                 prob1.q=window(prob1.q, start=c(2005,2), end=xq.end),
                 prob2.q=window(prob2.q, start=c(2005,2), end=xq.end),
                 factor.q=window(factor.q, start=c(2005,2), end=xq.end),
                 prob1.m=window(prob1.m, start=c(2005,4), end=xm.end),
                 prob2.m=window(prob2.m, start=c(2005,4), end=xm.end),
                 trend = window(trend, start=c(2005,2), end=xq.end))


insample <- 1:length(window(GDP.q, start=y.start, end=c(2019,1)))
outsample <- (1:length(fulldata$GDP.q))[-insample]

source("midas_list.R")

avgf <- average_forecast(midas_list(hor), 
                         data = fulldata,
                         insample = insample,
                         outsample = outsample,
                         type = "recursive")
error.pred <- abs(avgf[["forecast"]] - avgf[["xout"]])

# print(cbind(avgf[["forecast"]],avgf[["xout"]]))

pred.oos = avgf[["forecast"]]

oos = avgf[["xout"]]

rmsfe = diag(sqrt(t(pred.oos - oos)%*%(pred.oos - oos)/length(oos)))
relative = rmsfe[1:(length(rmsfe)-1)]/rmsfe[length(rmsfe)]
relative23 = rmsfe[1:4]/rmsfe[5:8]
dm_sig = rep(0,10)
cw_sig = rep(0,10)
dm_sig23 = rep(0,4)
cw_sig23 = rep(0,4)
for (i in 1:10){
  nested_idx = i
  smaller_idx= 11
  dm_cw_res = dm_cw(error.pred[,smaller_idx],error.pred[,nested_idx])
  dm_sig[i] = dm_cw_res$pv_dm_nw
  cw_sig[i] = dm_cw_res$pv_cw_nw
}
two_vs_three = rep(0,4)
for (i in 1:4){
  nested_idx = i
  smaller_idx = i+4
  dm_cw_res = dm_cw(error.pred[,smaller_idx],error.pred[,nested_idx])
  dm_sig23[i] = dm_cw_res$pv_dm_nw
  cw_sig23[i] = dm_cw_res$pv_cw_nw
}

if (hor==0){
  relative0 = cbind(relative,dm_sig,cw_sig)
  relative23.0 = cbind(relative23,dm_sig23,cw_sig23)
}
if (hor==1){
  relative1 = cbind(relative,dm_sig,cw_sig)
  relative23.1 = cbind(relative23,dm_sig23,cw_sig23)
}
if (hor==2){
  relative2 = cbind(relative,dm_sig,cw_sig)
  relative23.2 = cbind(relative23,dm_sig23,cw_sig23)
}
}

# relative23 = rmsfe_3state/rmsfe_2state
# relative_hor = rmsfe(midas)/rmsfe(baseline)
# rejecting if this difference is sufficiently positive !!!


cat('Better than Baseline:',sum(error.pred[,nested_idx]-error.pred[,smaller_idx]<0),'/',length(oos),
    '=',sum(error.pred[,nested_idx]-error.pred[,smaller_idx]<0)/length(oos))

# Combination Forecast
comb_MSE = avgf[["accuracy"]][["average"]][["MSE"]]
smallest_MSE_fore = avgf[["avgforecast"]][,which(comb_MSE==min(comb_MSE))]
comb.error = smallest_MSE_fore - oos
comb.test = rbind(dm_cw(error.pred[,11],abs(comb.error))[["pv_dm_nw"]],
                  dm_cw(error.pred[,11],abs(comb.error))[["pv_cw_nw"]])


######## AR model
# ar.forec <- average_forecast(list(ar1.fit), 
#                          data = fulldata,
#                          insample = insample,
#                          outsample = outsample,
#                          type = "recursive")

## Write
library(openxlsx)
for.res <- list('Forecast'=avgf[["forecast"]],'OOS'=avgf[["xout"]],
                'Error'=error.pred, 'RMSFE'=rmsfe,
                'Comb_Error'=comb.error, 'Comb_Test'=comb.test,
                'h0_RMSFE_vs_AR(1)'=relative0, 'h1_RMSFE_vs_AR(1)'=relative1,'h2_RMSFE_vs_AR(1)'=relative2,
                'h0_23'=relative23.0, 'h1_23'=relative23.1,'h2_23'=relative23.2)

write.xlsx(for.res,
          file = "Outputs/for_res_3.csv")


### STAR ###############################
library(tsDyn)
f.res = as.matrix(f.res)
mod.star <- star(f.res, mTh=c(0,1), control=list(maxit=3000))
mod.star

addRegime(mod.star)

### 画图

yy = ts(y, frequency = 12, start = c(2000, 1))
yy = scale(yy)

plot(yy[,2],type='l',col=2, ylim=c(-5,5))
for (i in (3:5)){
  lines(yy[,i],col=i)
}
legend("bottomleft", legend=var.names[2:5], col = 2:5, cex = 0.5, lty = 1)

plot(scale(GDP.q),type='l')
f.ts = ts(f.res, frequency = 12, start = c(2005, 2))
lines((f.ts),col="blue")

##

library(BCDating)
IP = yy.ts[,3]
IP.ts = ts(data=IP, frequency=12, start = c(2005,2))
dat <- BBQ(IP.ts, mincycle = 5, minphase = 2)
summary(dat)
dat


## 
library(psych)
f_G = corr.test(scale(GDP)[-1],factor.q[-length(factor.q)],
                method = 'pearson')
f_G
