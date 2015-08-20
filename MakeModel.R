dat <- read.csv("Weather_publicfile.csv",header=TRUE)

library(bbmle,MASS)

betall <- function(m,v,y){
  #constraints: m<1, v<1
  #m: mean
  #v: variance
  a <- (m^2 * (1-m))/v - m
  #a <- a[!is.na(a)]
  
  b <- (m * (1-m)^2)/v - (1-m)
  #b <- b[!is.na(b)]
  #if(a>0 && b>0){
  -sum(dbeta(y,a,b,log=TRUE))
  #}
  #else{-1e50}
  #penalize monstrously if specified mean & variance cannot exist for beta distr
}

normll <- function(m,sd,y){
  -sum(dnorm(y,m,sd,log=TRUE))
}


#########################################
dat$TurnoutP <- dat$Turnout/100
dat$TurnoutP[which(dat$TurnoutP==1)]<-.9999
#change 100% turnout data to 99.99% to make it work in the Beta distribution
#dat<-dat[-which(dat$Turnout==100),]
# tmp <- c()
# for(x in seq(0,1,.05)){
#   tmp <- c(tmp,betall(x,.1,test))
# }
#m.beta <- mle2(betall,start=list(m=.5,v=.01),data=list(y=dat$TurnoutP),method="L-BFGS-B",lower=c(m=.25,v=.00001),upper=c(m=.75,v=3))
##estimate parameters of beta for the data itself
m.beta <- mle2(betall,start=list(m=.5,v=.01),data=list(y=dat$TurnoutP))
##################################

#######logi1stic model of rain+snow
log.model <- glm(TurnoutP ~ Rain+Snow, data=dat,family="binomial")

a1 <- coef(log.model)[2]
a2 <- coef(log.model)[3]
a3 <- coef(log.model)[1]

####simple linear model
linear.Turnout <- function(a1,a2,a3,rd, snd,tnt,sd1){
  m1 <- a1*rd+a2*snd+a3
  #m1.norm <- abs(m1/max(m1))
  #m1.norm[which(m1.norm==1)]<-.9999
  #m1.norm <- transform.r(m1)
  #m1.norm <- m1[-c(which(m1 > 1),which(m1 < 0))]
  #v1 <- b1*rd+b2j
  #v1 <- m1.norm*(1-m1.norm) #http://data.princeton.edu/wws509/notes/c3s1.html
  #v1[which(v1==0)]<-.000001
  #v1 <- var(tnt/100)
  normll(m1,snd,tnt)
}

m.simple <- mle2(linear.Turnout, start=list(snd=sd(dat$TurnoutP),a1=a1,a2=a2,a3=a3),data=list(rd=dat$Rain,sd=dat$Snow,tnt=dat$TurnoutP),
                method="Nelder-Mead")

c2 <- coef(m.simple)
test1 <- c2[1]*dat$Rain+c2[2]*dat$Snow+c2[3]

hist(test1-dat$TurnoutP)
##############################
#PTurnout <- function
###full model
log.model <- glm(TurnoutP ~ Rain_Dev+Snow_Dev+PcntBlack+ZPcntHSGrad+FarmsPerCap+Closing, data=dat,family="binomial")
coef.full <- as.numeric(coef(log.model))

linear.Turnout <- function(a1,a2,a3,a4,a5,a6,a7,rainD,snowD,ptblack,hsgrad,farms,close,snd,tnt){
  m1 <- a1+a2*rainD+a3*snowD+a4*ptblack+a5*hsgrad+a6*farms+a7*close
  normll(m1,snd,tnt)
}

m.full <- mle2(linear.Turnout,start=list(snd=sd(dat$TurnoutP),
              a1=coef.full[1],a2=coef.full[2],a3=coef.full[3],a4=coef.full[4],a5=coef.full[5],a6=coef.full[6],a7=coef.full[7]),
              data=list(tnt=dat$TurnoutP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
              farms=dat$FarmsPerCap,close=dat$Closing),method="L-BFGS-B",control=list(maxit=100000,
              trace=5),lower=c(snd=1e-7,a1=-Inf,a2=-Inf,a3=-Inf,a4=-Inf,a5=-Inf,a6=-Inf,a7=-Inf))

m.profile <- profile(m.full)
#works
coef.fit <- as.numeric(coef(m.full))
m.fit <- coef.fit[1]+coef.fit[2]*dat$Rain_Dev+coef.fit[3]*dat$Snow_Dev+
  coef.fit[4]*dat$PcntBlack+coef.fit[5]*dat$ZPcntHSGrad+coef.fit[6]*dat$FarmsPerCap+coef.fit[7]*dat$Closing

#m.profile <- profile(m.full)
plot(m.profile)
#didnot converge, so restart with the profile values
# coef.step2 <- coef(m.profile)
# m.step2 <- mle2(step.Turnout,start=list(snd=coef.step2[8],
#               a1=coef.step2[1],a2=coef.step2[2],a3=coef.step2[3],a4=coef.step2[4],a5=coef.step2[5],a6=coef.step2[6],a7=coef.step2[7]),
#               data=list(tnt=dat$TurnoutP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
#               farms=dat$FarmsPerCap,close=dat$Closing),method="Nelder-Mead",control=list(maxit=100000),skip.hessian=FALSE)
# 
# coef.step3 <- coef(profile(m.step2))
# m.step2 <- mle2(step.Turnout,start=list(snd=coef.step3[8],
#                 a1=coef.step3[1],a2=coef.step2[2],a3=coef.step3[3],a4=coef.step3[4],a5=coef.step3[5],a6=coef.step3[6],a7=coef.step3[7]),
#                 data=list(tnt=dat$TurnoutP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
#                 farms=dat$FarmsPerCap,close=dat$Closing),method="Nelder-Mead",control=list(maxit=100000),skip.hessian=FALSE)

##############################################################
#AIC comparison
log.modelstep <- step(log.model)
log.step <- step(log.modelstep)
coef.step <- as.numeric(coef(log.step))
#snowD
step.Turnout <- function(a1,a2,a4,a5,a6,a7,rainD,snowD,ptblack,hsgrad,farms,close,snd,tnt){
  m1 <- a1+a2*rainD+a4*ptblack+a5*hsgrad+a6*farms+a7*close
  normll(m1,snd,tnt)
}

m.step <- mle2(step.Turnout,start=list(snd=sd(dat$TurnoutP),
              a1=coef.step[1],a2=coef.step[2],a4=coef.step[3],a5=coef.step[4],a6=coef.step[5],a7=coef.step[6]),
              data=list(tnt=dat$TurnoutP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
              farms=dat$FarmsPerCap,close=dat$Closing),method="L-BFGS-B",control=list(maxit=100000,
              trace=5),lower=c(snd=1e-7,a1=-Inf,a2=-Inf,a4=-Inf,a5=-Inf,a6=-Inf,a7=-Inf))

m.profileStep <- profile(m.step)
#works
coef.fitStep <- as.numeric(coef(m.step))
m.fitStep <- coef.fitStep[1]+coef.fitStep[2]*dat$Rain_Dev+#coef.fitStep[3]*dat$Snow_Dev+
  coef.fitStep[3]*dat$PcntBlack+coef.fitStep[4]*dat$ZPcntHSGrad+coef.fitStep[5]*dat$FarmsPerCap+coef.fitStep[6]*dat$Closing

m.profileStep <- profile(m.step)

tp.aic <- AICtab(m.full,m.step)
print(tp.aic)

###################################################
#make residual plots

diffs.full <- data.frame(cbind(m.fit,dat$TurnoutP))
ggplot(diffs.full,aes(x=m.fit,y=V2-m.fit))+geom_point(alpha=.05)+xlab("Fitted")+ylab("Residuals")+ggtitle("Fitted vs Residual for Turnout")
ggplot(diffs.full,aes(x=V2-m.fit))+geom_histogram()+xlab("Predicted - Observed")+ylab("Count")+ggtitle("Differences Between Observed and Expected: Turnout Model")

diffs.full2 <- data.frame(cbind(m.fitStep,dat$TurnoutP))
ggplot(diffs.full2,aes(x=m.fitStep,y=V2-m.fitStep))+geom_point(alpha=.05)+xlab("Fitted")+ylab("Residuals")+ggtitle("Fitted vs Residual for Turnout Step")
ggplot(diffs.full2,aes(x=V2-m.fitStep))+geom_histogram()+xlab("Predicted - Observed")+ylab("Count")+ggtitle("Differences Between Observed and Expected: Turnout Step Model")


#ggplot(diffs.full,aes(x=V2-m.fit))+geom_histogram()+ggtitle("Residuals of Turnout Model")
#ggplot(diffs.full2,aes(x=V2-m.fitStep))+geom_histogram()+ggtitle("Residuals of Turnout Step Model")
