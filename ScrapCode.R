

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

