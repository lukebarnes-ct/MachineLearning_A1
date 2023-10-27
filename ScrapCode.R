

legFunc = function(x, q){
  
  n = length(x)
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    val = (q+k-1)/2
    fact1 = factorial(q)/(factorial(k)*factorial(q-k))
    fact2 = factorial(val)/(factorial(q)*factorial(val-q))
    
    print(paste0("Val: ", val))
    print(paste0("k: ", k))
    print(paste0("q: ", q))
    print(paste0("Fact1: ", fact1))
    print(paste0("Fact2: ", fact2))
    
    legSum = (x^k) * (fact1) * (fact2)
    
    print(legSum)
    legFuncSum = legFuncSum + legSum
  }
  
  return((2^q) * legFuncSum)
}

legFunctions = legFunc(x, 5)

plot(x, legFunctions, type = "b")

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

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    legSum = (x)^k * (choose(q, k)) * (choose((q+k-1)/2, q))
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(2^q * legFuncSum)
}

# I can't seem to get the formulation of the legendre polynomial given to us to work in R
# Using another explicit formulation given for the polynomials

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    legSum = ((x-1)/2)^k * (choose(q, k)) * (choose(q+k, k))
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(legFuncSum)
}




#####################################################################3

# betasTwo = runif(3, -1, 1)
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
    
    fTwo = f2(onesTwo, xi) + eps
    fTen = f2(onesTen, xi) + eps
    nListTwo[[n]] = fTwo
    nListTen[[n]] = fTen
    
    errorMat[s, n] = mean((fTen - fTwo)^2)
    
  }
  
  dataListTwo[[s]] = nListTwo
  dataListTen[[s]] = nListTen
}

errorPlotData = melt(errorMat)

g = f1(onesTwo, xi)
f = f2(betasTen, xi)
int = (g - f)^2 * 0.5
outIntegral(xi, onesTwo, betasTen)

outIntegral = function(x, betas, otherBetas){
  
  f = f1(betas, x)
  g = f2(otherBetas, x)
  int = (g - f)^2 * 0.5
  
  return(int)
}

sc = scale(betasTen, center = 1)

betas = function(x, y){
  
  mod = lm(y~x)
  return(mod$coefficients)
}

f1 = function(x, n){
  
  xMat = matrix(0, length(x), n)
  
  for (i in 1:n){
    xMat[, i] = x^i
  }
  
  return(xMat)
}

dataListTwo = list()
dataListTen = list()

nSeq = 20:110
sigmaSeq = seq(0.2, 1.1, by = 0.1)

errorMat = matrix(0, length(sigmaSeq), length(nSeq))

for(s in 1:length(sigmaSeq)){
  
  nListTwo = list()
  nListTen = list()
  
  xLat = seq(-1,1, 0.01)
  nDX  = length(xLat) 
  gBar = rep(0, nDX)
  GD   = matrix(0, length(nSeq), nDX)
  
  for(n in 1:length(nSeq)){
    
    xi = runif(nSeq[n], -1, 1)
    eps = rnorm(nSeq[n], mean = 0, sd = sigmaSeq[s]^2)
    target = f2(betasTen, xi) + eps
    
    resMod2 = lm(target~poly(xi, 2))
    resMod10 = lm(target~poly(xi, 10))
    
    xOut = runif(nSeq[n], -1, 1)
    epsOut = rnorm(nSeq[n], mean = 0, sd = sigmaSeq[s]^2)
    yOut = f2(betasTen, xi) + eps
    
    resPred2 = predict(resMod2, data.frame(x = xOut))
    resPred10 = predict(resMod10, data.frame(x = xOut))
    
    E_Out2 = mean((resPred2 - yOut)^2)
    E_Out10 = mean((resPred10 - yOut)^2)
    
    nListTwo[[n]] = E_Out2
    nListTen[[n]] = E_Out10
    
    errorMat[s, n] = E_Out10 - E_Out2
    
  }
  
  dataListTwo[[s]] = nListTwo
  dataListTen[[s]] = nListTen
  
  gBar = gBar / length(nSeq)
}

errorPlotData = melt(errorMat)

ggplot(errorPlotData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = value)) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient(low = "skyblue", high = "red") +
  theme_bw()

###########################################################################

xLat = seq(-1,1, dx)
sigma = 0.5

nDX  = length(xLat) 
gBar2 = rep(0, nDX)
gBar10 = rep(0, nDX)
GD2   = matrix(0, nDX, 1)
GD10   = matrix(0, nDX, 1)

N = 500
xi = runif(N, -1, 1)
eps = rnorm(N, mean = 0, sd = sigma^2)
target = f2(betasTen, xi) + eps

resMod2 = lm(target ~ poly(xi, 2))
resMod10 = lm(target ~ poly(xi, 10))

X2 = legPoly(xi, 2)
X10 = legPoly(xi, 10)

betaTwo = solve(t(X2) %*% X2) %*% t(X2) %*% target
betaTen = solve(t(X10) %*% X10) %*% t(X10) %*% target

resMod2 = lm(target ~ X2[, -1])
resMod10 = lm(target ~ X10[, -1])

resMod2Coefs = coefficients(resMod2)
resMod10Coefs = coefficients(resMod10)

resPred2 = legPoly(xLat, 2) %*% resMod2Coefs
resPred10 = legPoly(xLat, 10) %*% resMod10Coefs


resMod2Coefs = coefficients(resMod2)
resMod10Coefs = coefficients(resMod10)

resPred2 = cbind(1, poly(xLat, 2)) %*% resMod2Coefs
resPred10 = cbind(1, poly(xLat, 10)) %*% resMod10Coefs

gBar2 = gBar2 + resPred2
gBar10 = gBar10 + resPred10

GD2[i, ] = resPred2
GD10[i, ] = resPred2

require(plotly)
plot_ly(x = nSeq, y = sigmaSeq, z = errorMat, type = "contour")

ggplot(errorPlotData, aes(x = N, y = Sigma)) + 
  geom_raster(aes(fill = Error)) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient(low = "skyblue", high = "red") +
  theme_bw()

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
  E_Out10 = bias10 + var10
  
  #ExpError = E_Out10 - E_Out2
  
  #return(ExpError)
  return(list(E2 = E_Out2, E10 = E_Out10))
}

######################################################################

errorSimulation = function(N, sigma){
  
  xLat = seq(-1,1, dx)
  nDX  = length(xLat) 
  gBar2 = rep(0, nDX)
  gBar10 = rep(0, nDX)
  GD2   = matrix(0, M, nDX)
  GD10   = matrix(0, M, nDX)
  
  for (i in 1:M){
    
    xi = runif(N, -1, 1)
    eps = rnorm(N, mean = 0, sd = sigma)
    target = f2(betasTen, xi) + eps
    
    X2 = legPoly(xi, 2)
    X10 = legPoly(xi, 10)
    
    #resMod2 = lm(target ~ X2[, -1])
    #resMod10 = lm(target ~ X10[, -1])
    
    betaTwo = solve(t(X2) %*% X2) %*% t(X2) %*% target
    betaTen = solve(t(X10) %*% X10) %*% t(X10) %*% target
    
    #resMod2Coefs = coefficients(resMod2)
    #resMod10Coefs = coefficients(resMod10)
    
    resPred2 = legPoly(xLat, 2) %*% betaTwo
    resPred10 = legPoly(xLat, 10) %*% betaTen
    
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
  E_Out10 = bias10 + var10
  
  #ExpError = E_Out10 - E_Out2
  
  #return(ExpError)
  return(list(E2 = E_Out2, E10 = E_Out10, G2 = gBar2, G10 = gBar10))
}

errorPlotData$Error = ifelse(errorPlotData$Error > -0.1, 0.2, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error > -0.2) & (errorPlotData$Error < -0.1), 0, errorPlotData$Error)
errorPlotData$Error = ifelse(errorPlotData$Error < -0.2, -0.2, errorPlotData$Error)

############################################################################

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
    
    X2 = cbind(1, poly(xi, 2))
    X10 = cbind(1, poly(xi, 10))
    
    #X2 = legPoly(xi, 2)
    #X10 = legPoly(xi, 10)
    
    betaTwo = solve(t(X2) %*% X2) %*% t(X2) %*% target
    betaTen = solve(t(X10) %*% X10) %*% t(X10) %*% target
    
    X2Lat = cbind(1, poly(xLat, 2))
    X10Lat = cbind(1, poly(xLat, 10))
    
    resPred2 = X2Lat %*% betaTwo
    resPred10 = X10Lat %*% betaTen
    
    #resPred2 = legPoly(xLat, 2) %*% betaTwo
    #resPred10 = legPoly(xLat, 10) %*% betaTen
    
    #gBar2 = gBar2 + resPred2
    #gBar10 = gBar10 + resPred10
    
    GD2[i, ] = resPred2
    GD10[i, ] = resPred10
    
  }
  
  #gBar2 = gBar2 / M
  #gBar10 = gBar10 / M
  
  gBar2 = colMeans(GD2)
  gBar10 = colMeans(GD10)
  
  phiX = 0.5
  
  yF = f2(betasTen, xLat) + rnorm(nDX, mean = 0, sd = sigma^2)
  
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

ggplot(errorPlotData, aes(x = N, y = Sigma, fill = Error)) + 
  geom_raster(interpolate = TRUE) +
  xlab("N") + ylab("Sigma") + labs(color = "Error") +
  scale_fill_gradient2(low = "navyblue", mid = "darkgreen", high = "maroon") +
  theme_bw()

errorPlotData2$Error = ifelse(errorPlotData2$Error >= 0.2, 0.2, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < 0.2) & (errorPlotData2$Error >= 0.1), 0.1, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < 0.1) & (errorPlotData2$Error >= -0.1), 0, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < -0.1) & (errorPlotData2$Error >= -0.15), -0.1, errorPlotData2$Error)
errorPlotData2$Error = ifelse(errorPlotData2$Error < -0.15, -0.2, errorPlotData2$Error)

pdf("errorPlot_Q2B.pdf")
ggplot(errorPlotData2, aes(x = N, y = QF, fill = Error)) + 
  geom_raster(interpolate = TRUE) +
  xlab("N") + ylab("QF") + labs(color = "Error") +
  scale_fill_gradientn(colours = c("navyblue", "lightblue", "darkgreen", "gold", "maroon")) +
  theme_bw()
dev.off()


nSeq = seq(20, 110, by = 10)
sigmaSeq = seq(0.2, 1.1, by = 0.1)

newErrorMat = log(errorMat + 1) + min(errorMat)
errorPlotData = melt(newErrorMat)
plotExpand = expand.grid(sigmaSeq, nSeq)
errorPlotData = cbind(plotExpand, errorPlotData$value)
colnames(errorPlotData) = c("Sigma", "N", "Error")

round(errorPlotData$Error, 3)

errorPlotData$Error = ifelse(errorPlotData$Error >= 0.2, 0.2, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < 0.2) & (errorPlotData$Error >= 0.1), 0.1, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < 0.1) & (errorPlotData$Error >= -0.1), 0, errorPlotData$Error)
errorPlotData$Error = ifelse((errorPlotData$Error < -0.1) & (errorPlotData$Error >= -0.15), -0.1, errorPlotData$Error)
errorPlotData$Error = ifelse(errorPlotData$Error < -0.15, -0.2, errorPlotData$Error)

aa = 0.2
bb = 0.1
dd = -0.1
ee = -0.2

errorPlotData2$Error = ifelse(errorPlotData2$Error >= aa, 0.2, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < aa) & (errorPlotData2$Error >= bb), 0.1, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < bb) & (errorPlotData2$Error >= dd), 0, errorPlotData2$Error)
errorPlotData2$Error = ifelse((errorPlotData2$Error < dd) & (errorPlotData2$Error >= ee), -0.1, errorPlotData2$Error)
errorPlotData2$Error = ifelse(errorPlotData2$Error < ee, -0.2, errorPlotData2$Error)

###########################################################################

hVal1 = matrix(0, N, N)
hVal2 = hVal1
expComb = expand.grid(x1, x2)

for(i in 1:N){
  
  for (j in 1:N){
    
    hVal1 = H1()
  }
}