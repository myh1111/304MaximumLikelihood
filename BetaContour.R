

a <- (m^2 * (1-m))/v - m
#a <- a[!is.na(a)]

b <- (m * (1-m)^2)/v - (1-m)

a <- matrix(nrow=101,ncol=101)
b <- matrix(nrow=101,ncol=101)
s <- seq(0,1,.01)
for(m in 1:101){
  for (v in 1:101){
    a[m,v] <- (s[m]^2 * (1-s[m]))/s[v] -s[m]
    b[m,v] <- (s[m]*(1-s[m])^2)/s[v] - (1-s[m])
  }
}
rownames(a) <- s
colnames(a) <- s
aw <- which(a>0,arr.ind=TRUE)
bw <- which(b>0,arr.ind=TRUE)

plot(aw)
points(bw)