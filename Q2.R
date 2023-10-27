
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

legPoly = function(x, q){
  
  legPolyMat = matrix(0, length(x), q+1)
  
  for (i in 0:q){
    
    legPolyMat[, i + 1] = legFunc(x, i)
  }
  
  return(legPolyMat)
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

dx = 0.01

errorSimulation = function(N, sigma){
  
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
    
    X2 = legPoly(xi, 2)
    X10 = legPoly(xi, 10)
    
    betaTwo = solve(t(X2) %*% X2) %*% t(X2) %*% target
    betaTen = solve(t(X10) %*% X10) %*% t(X10) %*% target
    
    resPred2 = legPoly(xLat, 2) %*% betaTwo
    resPred10 = legPoly(xLat, 10) %*% betaTen
    
    gBar2 = gBar2 + resPred2
    gBar10 = gBar10 + resPred10
     
    GD2[i, ] = resPred2
    GD10[i, ] = resPred10
    
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
  E_Out10 = bias10 + var10
  
  return(list(E2 = E_Out2, E10 = E_Out10,
              bias2 = bias2, bias10 = bias10,
              var2 = var2, var10 = var10))
}

nSeq = seq(20, 110, by = 1)
sigmaSeq = seq(0.2, 1.1, by = 0.01)

errorMat = matrix(0, length(sigmaSeq), length(nSeq))
E2Mat = matrix(0, length(sigmaSeq), length(nSeq))
E10Mat = matrix(0, length(sigmaSeq), length(nSeq))

Var2Mat = matrix(0, length(sigmaSeq), length(nSeq))
Var10Mat = matrix(0, length(sigmaSeq), length(nSeq))

Bias2Mat = matrix(0, length(sigmaSeq), length(nSeq))
Bias10Mat = matrix(0, length(sigmaSeq), length(nSeq))

M = 500

for(s in 1:length(sigmaSeq)){
  
  for(n in 1:length(nSeq)){
    print(paste0("Sigma: ", s, ". N: ", n, "."))
    tic()
    error = errorSimulation(nSeq[n], sigmaSeq[s])
    E2Mat[s, n] = error$E2
    E10Mat[s, n] = error$E10
    Var2Mat[s, n] = error$var2
    Var10Mat[s, n] = error$var10
    Bias2Mat[s, n] = error$bias2
    Bias10Mat[s, n] = error$bias10
    errorMat[s, n] = E10Mat[s, n] - E2Mat[s, n]
    toc()
  }

}

errorPlotData = melt(errorMat)
plotExpand = expand.grid(sigmaSeq, nSeq)
errorPlotData = cbind(plotExpand, errorPlotData$value)
colnames(errorPlotData) = c("Sigma", "N", "Error")

round(errorPlotData$Error, 3)

errorPlotData$Error = ifelse(errorPlotData$Error >= 0.2, 0.2, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < 0.2) & (errorPlotData$Error >= 0.1), 0.1, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < 0.1) & (errorPlotData$Error >= -0.1), 0, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < -0.1) & (errorPlotData$Error >= -0.15), -0.1, errorPlotData$Error)
errorPlotData$Error = ifelse(errorPlotData$Error < -0.15, -0.2, errorPlotData$Error)

pdf("errorPlot_Q2A.pdf")
ggplot(errorPlotData, aes(x = N, y = Sigma, fill = Error)) + 
  geom_raster(interpolate = TRUE) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradientn(colours = c("navyblue", "lightblue", "darkgreen", "gold", "maroon")) +
  theme_bw()
dev.off()

##### Q2.ii

sigma = 0.2

errorSimulationQF = function(N, qf){
  
  xLat = seq(-1,1, dx)
  nDX  = length(xLat) 
  gBar2 = rep(0, nDX)
  gBar10 = rep(0, nDX)
  GD2   = matrix(0, M, nDX)
  GD10   = matrix(0, M, nDX)
  betasTarget = runif(qf + 1, -1, 1)
  
  fx = f2(betasTarget, xLat)
  norm = sqrt(sum(fx^2) * dx)
  newBetas = betasTarget * (1/norm)
  
  for (i in 1:M){
    
    xi = runif(N, -1, 1)
    eps = rnorm(N, mean = 0, sd = sigma^2)
    
    target = f2(newBetas, xi) + eps
    
    X2 = legPoly(xi, 2)
    X10 = legPoly(xi, 10)
    
    betaTwo = solve(t(X2) %*% X2) %*% t(X2) %*% target
    betaTen = solve(t(X10) %*% X10) %*% t(X10) %*% target
    
    resPred2 = legPoly(xLat, 2) %*% betaTwo
    resPred10 = legPoly(xLat, 10) %*% betaTen
    
    gBar2 = gBar2 + resPred2
    gBar10 = gBar10 + resPred10
    
    GD2[i, ] = resPred2
    GD10[i, ] = resPred10
    
  }
  
  gBar2 = gBar2 / M
  gBar10 = gBar10 / M
  
  phiX = 0.5
  
  yF = f2(newBetas, xLat)
  
  bias2 = sum((gBar2 - yF)[-nDX]^2 * phiX * dx)
  
  bias10 = sum((gBar10 - yF)[-nDX]^2 * phiX * dx)
  
  ones = matrix(1, M, 1)
  
  varX2 = colSums((GD2 - ones %*% t(gBar2))^2) / M
  
  varX10 = colSums((GD10 - ones %*% t(gBar10))^2) / M
  
  var2 = sum(varX2[-nDX] * phiX * dx)
  var10 = sum(varX10[-nDX] * phiX * dx)
  
  E_Out2 = bias2 + var2
  E_Out10 = bias10 + var10
  
  return(list(E2 = E_Out2, E10 = E_Out10,
              bias2 = bias2, bias10 = bias10,
              var2 = var2, var10 = var10))
}

nSeq2 = seq(20, 110, by = 1)
qfSeq = seq(1, 40, by = 1)

errorMat2 = matrix(0, length(qfSeq), length(nSeq2))

M = 500

for(q in 1:length(qfSeq)){
  
  for(n in 1:length(nSeq2)){
    print(paste0("QF: ", q, ". N: ", n, "."))
    tic()
    error = errorSimulationQF(nSeq2[n], qfSeq[q])
    E2 = error$E2
    E10 = error$E10
    errorMat2[q, n] = E10 - E2
    toc()
  }
   
}

errorPlotData2 = melt(errorMat2)
plotExpand2 = expand.grid(qfSeq, nSeq2)
errorPlotData2 = cbind(plotExpand2, errorPlotData2$value)
colnames(errorPlotData2) = c("QF", "N", "Error")

round(errorPlotData2$Error, 3)

aa = 0.2
ee = -0.2

errorPlotData2$Error = ifelse(errorPlotData2$Error >= aa, 0.2, errorPlotData2$Error)
errorPlotData2$Error = ifelse(errorPlotData2$Error < ee, -0.2, errorPlotData2$Error)

pdf("errorPlot_Q2B.pdf")
ggplot(errorPlotData2, aes(x = N, y = QF, fill = Error)) + 
  geom_raster(interpolate = TRUE) +
  xlab("N") + ylab("QF") + labs(color = "Error") +
  scale_fill_gradientn(colours = c("navyblue", "lightblue", "darkgreen", "gold", "maroon")) +
  theme_bw()
dev.off()
