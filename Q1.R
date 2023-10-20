
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)

set.seed(2023)

##### Q1.i

N = 100
x = seq(-1, 1, length = N)
# x = runif(N, -1, 1)

# I can't seem to get the formulation of the legendre polynomial given to us to work in R
# Using another explicit formulation given for the polynomials

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    fact1 = factorial(q)/(factorial(k)*factorial(q-k))
    fact2 = factorial(q+k)/(factorial(k)*factorial(q))
    
    legSum = ((x-1)/2)^k * (fact1) * (fact2)
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(legFuncSum)
}

legPolyMat = matrix(0, N, 6)

for (i in 0:5){
  
  legPolyMat[, i + 1] = legFunc(x, i)
}

legPolyData = as.data.frame(cbind(x, legPolyMat))

pdf("legPoly_Q1.pdf")
ggplot(legPolyData, aes(x = x)) +
  geom_line(aes(y = V2), col = "black", linewidth = 2) +
  geom_line(aes(y = V3), col = "lightpink", linewidth = 2) +
  geom_line(aes(y = V4), col = "mediumpurple3", linewidth = 2) +
  geom_line(aes(y = V5), col = "turquoise3", linewidth = 2) +
  geom_line(aes(y = V6), col = "gold", linewidth = 2) +
  geom_line(aes(y = V7), col = "lawngreen", linewidth = 2) +
  labs(x = "X", y = expression("L"[q](x))) +
  ylim(-1, 1) +
  theme_bw(base_size = 16)
dev.off()
