

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