###############################33
#DevModel.R
#results are a biased model 
################################
####logi1stic model of rain_dev+snow_dev
log.model <- glm(TurnoutP ~ Rain_Dev+Snow_Dev, data=dat,family="binomial")

a1 <- coef(log.model)[2]
a2 <- coef(log.model)[3]
a3 <- coef(log.model)[1]

####simple linear model
linear.Turnout <- function(a1,a2,a3,rd, sd,tnt,v1){
  m1 <- a1*rd+a2*sd+a3
  m1.norm <- abs(m1/max(m1))
  #v1 <- b1*rd+b2j
  #v1 <- m1.norm*(1-m1.norm) #http://data.princeton.edu/wws509/notes/c3s1.html
  #v1[which(v1==0)]<-.000001
  #v1 <- var(tnt/100)
  m1.norm[which(m1.norm==1)]<-.9999
  betall(m1.norm,v1,tnt)
}

m.simple <- mle2(linear.Turnout, start=list(v1=v1,a1=a1,a2=a2,a3=a3),data=list(rd=dat$Rain_Dev,sd=dat$Snow_Dev,tnt=dat$TurnoutP),
                 method="Nelder-Mead" )

c2 <- coef(m.simple)
test1 <- c2[1]*dat$Rain_Dev+c2[2]*dat$Snow_Dev+c2[3]

hist(test1-dat$TurnoutP)
mean(test1-dat$TurnoutP)