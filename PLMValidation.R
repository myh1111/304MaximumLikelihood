dat <- read.csv("Weather_publicfile.csv",header=TRUE)

library(plm)

dat$StateCounty <- paste(dat$State,dat$County,sep="")

vs.1 <- plm(GOPVoteShare ~ Rain_Dev + Snow_Dev + GOPVoteShare_3MA,
            data=dat, index=c("Year","StateCounty"),model="random")

vs.2 <- plm(GOPVoteShare ~ Rain_Dev + Snow_Dev + RD_GOPVS + SD_GOPVS +
              GOPVoteShare_3MA, data=dat, index=c("Year","StateCounty"),model="random")

tnt.1 <- plm(Turnout ~ Turnout_Lag + Rain + Snow + PcntBlack + ZPcntHSGrad + FarmsPerCap + AdjIncome + Closing
             + Property + Literacy + PollTax + Motor + GubElection + SenElection,
             data=dat, index=c("Year","StateCounty"),model="random")

tnt.2 <- plm(Turnout ~ Turnout_Lag + Rain_Dev + Snow_Dev + PcntBlack
             + ZPcntHSGrad + FarmsPerCap + AdjIncome + Closing + Property
             + Literacy + PollTax + Motor + GubElection + SenElection,
             data=dat, index=c("Year","StateCounty"), model="random")

#use FIPS_County id from now on