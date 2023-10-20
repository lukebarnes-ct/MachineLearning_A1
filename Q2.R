
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)

set.seed(2023)

##### Q2.i

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

f2 = function(betas, x){
  
  q = length(betas) - 1
  f = 0
  
  for(i in 0:q){
    
    leg = legFunc(x, i)
    f = f + (betas[i+1]) * leg
  }
  
  return(f)
}

betas = runif(11, -1, 1)

dataList = list()

nSeq = seq(20, 110, by = 1)
sigmaSeq = seq(0.2, 1.1, by = 0.1)

for(s in 1:length(sigmaSeq)){
  
  nList = list()
  for(n in 1:length(nSeq)){
    
    xi = runif(nSeq[n], -1, 1)
    eps = rnorm(nSeq[n], mean = 0, sd = sigmaSeq[s]^2)
    nList[[n]] = f2(betas, xi) + eps
    
  }
  
  dataList[[s]] = nList
}

