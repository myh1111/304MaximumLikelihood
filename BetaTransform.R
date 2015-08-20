m <- runif(100,-.25,1.5)

x <- min(m)
y <- max(m)

r <- y-x

m2 <- m/r 

m3 <- m2*.6
m3.min <- min(m3)

m4 <- m3+.2-m3.min
transform.r <- function(m){
  x <- min(m)
  y <- max(m)
  r <- y-x #range
  m2 <- m*.6/r #partial transform
  m2.min <- min(m2)
  return(m2+.2-m2.min)
}

# detransform.r <- function(m.norm){
#   m2.norm <- 
# }