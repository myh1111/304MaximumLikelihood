dat <- read.csv("Weather_publicfile.csv",header=TRUE)
dat$GOPVoteShareP <- dat$GOPVoteShare/100
library(bbmle,MASS)

normll <- function(m,sd,y){
  -sum(dnorm(y,m,sd,log=TRUE))
}

VSlog.modelFull <- glm(GOPVoteShareP ~ Rain_Dev+Snow_Dev+PcntBlack+ZPcntHSGrad+FarmsPerCap+Closing, data=dat,family="binomial")
coef.fullVS <- as.numeric(coef(VSlog.modelFull))

full.GOPVoteShare <- function(a1,a2,a3,a4,a5,a6,a7,rainD,snowD,ptblack,hsgrad,farms,close,snd,tnt){
  m1 <- a1+a2*rainD+a3*snowD+a4*ptblack+a5*hsgrad+a6*farms+a7*close
  normll(m1,snd,tnt)
}

m.fullG <- mle2(full.GOPVoteShare,start=list(snd=sd(dat$GOPVoteShareP),
              a1=coef.fullVS[1],a2=coef.fullVS[2],a3=coef.fullVS[3],a4=coef.fullVS[4],a5=coef.fullVS[5],a6=coef.fullVS[6],a7=coef.fullVS[7]),
              data=list(tnt=dat$GOPVoteShareP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
              farms=dat$FarmsPerCap,close=dat$Closing),method="L-BFGS-B",control=list(maxit=100000,
              trace=5),lower=c(snd=1e-7,a1=-Inf,a2=-Inf,a3=-Inf,a4=-Inf,a5=-Inf,a6=-Inf,a7=-Inf))

m.profileG <- profile(m.fullG)
plot(m.profileG)
#works
coef.fitG <- as.numeric(coef(m.fullG))
m.fitG <- coef.fitG[1]+coef.fitG[2]*dat$Rain_Dev+coef.fitG[3]*dat$Snow_Dev+
  coef.fitG[4]*dat$PcntBlack+coef.fitG[5]*dat$ZPcntHSGrad+coef.fitG[6]*dat$FarmsPerCap+coef.fitG[7]*dat$Closing

plot(m.fitG-dat$GOPVoteShareP)
########################################################3
#STEP MODEL
#########################################################
#log.modelstepVS <- stepAIC(m.full)
log.step2 <- step(VSlog.modelFull)
coef.step2 <- as.numeric(coef(log.step2))
#snowD
step.Turnout <- function(a1,a2,a3,a4,a5,a6,a7,rainD,snowD,ptblack,hsgrad,farms,close,snd,tnt){
  m1 <- a1+a2*rainD+a3*snowD+a4*ptblack+a5*hsgrad+a6*farms+a7*close
  normll(m1,snd,tnt)
}

m.stepG <- mle2(step.Turnout,start=list(snd=sd(dat$GOPVoteShareP),
               a1=coef.step2[1],a2=coef.step2[2],a3=coef.step2[3],a4=coef.step2[4],a5=coef.step2[5],a6=coef.step2[6],a7=coef.step2[7]),
               data=list(tnt=dat$GOPVoteShareP,rainD=dat$Rain_Dev,snowD=dat$Snow_Dev,ptblack=dat$PcntBlack,hsgrad=dat$ZPcntHSGrad,
               farms=dat$FarmsPerCap,close=dat$Closing),method="L-BFGS-B",control=list(maxit=100000,
               trace=5),lower=c(snd=1e-7,a1=-Inf,a2=-Inf,a3=-Inf,a4=-Inf,a5=-Inf,a6=-Inf,a7=-Inf))

m.profileStepG <- profile(m.stepG)
#works
coef.fitStepG <- as.numeric(coef(m.stepG))
m.fitStepG <- coef.fitStepG[1]+coef.fitStepG[2]*dat$Rain_Dev+coef.fitStepG[3]*dat$Snow_Dev+
  coef.fitStepG[4]*dat$PcntBlack+coef.fitStepG[5]*dat$ZPcntHSGrad+coef.fitStepG[6]*dat$FarmsPerCap+coef.fitStepG[7]*dat$Closing

#m.profileStepG <- profile(m.stepG)

tp.aicG <- AICtab(m.fullG,m.stepG)
print(tp.aicG)

###################################################
#make residual plots

diffs.full <- data.frame(cbind(m.fitG,dat$GOPVoteShareP))
ggplot(diffs.full,aes(x=m.fitG,y=V2-m.fitG))+geom_point(alpha=.05)+xlab("Fitted")+ylab("Residuals")+ggtitle("Fitted vs Residual for VoteShare Full")
ggplot(diffs.full,aes(x=V2-m.fitG))+geom_histogram()+xlab("Predicted - Observed")+ylab("Count")+ggtitle("Differences Between Observed and Expected: VoteShare Model")

diffs.full2 <- data.frame(cbind(m.fitStepG,dat$GOPVoteShareP))
ggplot(diffs.full2,aes(x=m.fitStepG,y=V2-m.fitStepG))+geom_point(alpha=.05)+xlab("Fitted")+ylab("Residuals")+ggtitle("Fitted vs Residual for VoteShare Step AIC")
ggplot(diffs.full2,aes(x=V2-m.fitStepG))+geom_histogram()+xlab("Predicted - Observed")+ylab("Count")+ggtitle("Differences Between Observed and Expected: VoteShare Step")

#ggplot(diffs.full,aes(x=V2-m.fitG))+geom_histogram()+ggtitle("Residuals of VoteShare Model")
#ggplot(diffs.full2,aes(x=V2-m.fitStepG))+geom_histogram()+ggtitle("Residuals of VoteShare Step Model")
#merge(coef(m.fullG), coef(m.stepG),all=TRUE)
t(rbind(coef(m.fullG),coef(m.stepG)))
