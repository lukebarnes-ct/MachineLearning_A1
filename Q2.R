
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)
library(reshape2)

set.seed(2023)

##### Q2.i

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    legSum = (x)^k * (choose(q, k)) * (choose((q+k-1)/2, q))
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(2^q * legFuncSum)
}

f1 = function(alphas, x){
  
  q = length(alphas) - 1
  f = 0
  
  for(i in 0:q){
    
    f = f + (alphas[i+1]) * (x^i)
  }
  
  return(f)
}

f2 = function(betas, x){
  
  q = length(betas) - 1
  f = 0
  
  for(i in 0:q){
    
    leg = legFunc(x, i)
    f = f + (betas[i+1]) * leg
  }
  
  return(f)
}

betasTen = runif(11, -1, 1)
onesTwo = rep(1, 3)
onesTen = rep(1, 11)

dataListTwo = list()
dataListTen = list()

nSeq = 20:110
sigmaSeq = seq(0.2, 1.1, by = 0.1)

errorMat = matrix(0, length(sigmaSeq), length(nSeq))

for(s in 1:length(sigmaSeq)){
  
  nListTwo = list()
  nListTen = list()
  for(n in 1:length(nSeq)){
    
    xi = runif(nSeq[n], -1, 1)
    eps = rnorm(nSeq[n], mean = 0, sd = sigmaSeq[s]^2)
    
    target = f2(betasTen, xi)
    
    fTwo = f1(onesTwo, xi) + eps
    fTen = f1(onesTen, xi) + eps
    nListTwo[[n]] = fTwo
    nListTen[[n]] = fTen
    
    errorTwo = mean((fTwo - target)^2)
    errorTen = mean((fTen - target)^2)
    
    errorMat[s, n] = errorTen - errorTwo
    
  }
  
  dataListTwo[[s]] = nListTwo
  dataListTen[[s]] = nListTen
}

errorPlotData = melt(errorMat)

ggplot(errorPlotData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = value)) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient(low = "skyblue", high = "red") +
  theme_bw()

