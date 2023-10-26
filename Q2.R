
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)
library(reshape2)
library(tictoc)

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

dx = 0.001

errorSimulation = function(M, N, sigma){
  
  xLat = seq(-1,1, dx)
  nDX  = length(xLat) 
  gBar2 = rep(0, nDX)
  gBar10 = rep(0, nDX)
  GD2   = matrix(0, M, nDX)
  GD10   = matrix(0, M, nDX)
  
  for (i in 1:M){
    
    xi = runif(N, -1, 1)
    eps = rnorm(N, mean = 0, sd = sigma^2)
    target = f2(betasTen, xi) + eps
    
    resMod2 = lm(target~poly(xi, 2))
    resMod10 = lm(target~poly(xi, 10))
    
    resMod2Coefs = coefficients(resMod2)
    resMod10Coefs = coefficients(resMod10)
    
    resPred2 = cbind(1, poly(xLat, 2)) %*% resMod2Coefs
    resPred10 = cbind(1, poly(xLat, 10)) %*% resMod10Coefs
    
    gBar2 = gBar2 + resPred2
    gBar10 = gBar10 + resPred10
    
    GD2[i, ] = resPred2
    GD10[i, ] = resPred2
    
  }
  
  gBar2 = gBar2 / M
  gBar10 = gBar10 / M
  
  phiX = 0.5
  
  yF = f2(betasTen, xLat)
  
  bias2 = sum((gBar2 - yF)[-nDX]^2 * phiX * dx)
  
  bias10 = sum((gBar10 - yF)[-nDX]^2 * phiX * dx)
  
  ones = matrix(1, M, 1)
  
  varX2 = colSums((GD2 - ones %*% t(gBar2))^2) / M
  varX10 = colSums((GD10 - ones %*% t(gBar10))^2) / M
  
  var2 = sum(varX2[-nDX] * phiX * dx)
  var10 = sum(varX10[-nDX] * phiX * dx)
  
  E_Out2 = bias2 + var2
  E_Out10 = bias2 + var10
  
  ExpError = E_Out10 - E_Out2
  
  return(ExpError)
}

# nSeq = 20:110
nSeq = seq(20, 110, by = 5)
sigmaSeq = seq(0.2, 1.1, by = 0.05)

errorMat = matrix(0, length(sigmaSeq), length(nSeq))

M = 1000

for(s in 1:length(sigmaSeq)){
  
  for(n in 1:length(nSeq)){
    print(paste0("Sigma: ", s, ". N: ", n, "."))
    tic()
    errorMat[s, n] = errorSimulation(M, nSeq[n], sigmaSeq[s])
    toc()
  }

}

errorPlotData = melt(errorMat)
colnames(errorPlotData) = c("Sigma", "N", "Error")

ggplot(errorPlotData, aes(x = N, y = Sigma)) + 
  geom_raster(aes(fill = Error)) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient(low = "skyblue", high = "red") +
  theme_bw()

ggplot(errorPlotData, aes(x = N, y = Sigma, fill = Error)) + 
  geom_raster(interpolate = TRUE) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient2(low = "skyblue", mid = "green", high = "red") +
  theme_bw()


