
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)

set.seed(2023)

##### Q1.i

N = 100
x = seq(-1, 1, length = N)

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    legSum = (x)^k * (choose(q, k)) * (choose((q+k-1)/2, q))
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(2^q * legFuncSum)
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
  geom_line(aes(y = V5), col = "lightblue", linewidth = 2) +
  geom_line(aes(y = V6), col = "gold", linewidth = 2) +
  geom_line(aes(y = V7), col = "lawngreen", linewidth = 2) +
  labs(x = "X", y = expression("L"[q](x))) +
  ylim(-1, 1) +
  theme_bw(base_size = 16)
dev.off()


##### Q1.ii

### Choose Qf = 10

Qf = 10
nsam = (Qf + 1)*6
sam = runif(nsam, -1, 1)
coefMat = matrix(sam, (Qf+1), 6)

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

fMat = matrix(0, N, 6)

for (a in 1:3){
  
  fMat[, a] = f1(coefMat[, a], x)
}

for (b in 4:6){
  
  fMat[, b] = f2(coefMat[, b], x)
}
  
targetPlotData = data.frame("X" = x,
                            "T1" = fMat[, 1],
                            "T2" = fMat[, 2],
                            "T3" = fMat[, 3],
                            "T4" = fMat[, 4],
                            "T5" = fMat[, 5],
                            "T6" = fMat[, 6])

pdf("target1_Q1.pdf")
a = ggplot(targetPlotData, aes(x = X)) +
  geom_line(aes(y = T1), col = "black", linewidth = 2) +
  geom_line(aes(y = T2), col = "maroon", linewidth = 2) +
  geom_line(aes(y = T3), col = "purple", linewidth = 2) +
  labs(x = "X", y = "f(x)") +
  ylim(-4, 4) +
  theme_bw(base_size = 16)
dev.off()

pdf("target2_Q1.pdf")
b = ggplot(targetPlotData, aes(x = X)) +
  geom_line(aes(y = T4), col = "dodgerblue3", linewidth = 2) +
  geom_line(aes(y = T5), col = "gold", linewidth = 2) +
  geom_line(aes(y = T6), col = "darkolivegreen2", linewidth = 2) +
  labs(x = "X", y = "f(x)") +
  ylim(-4, 4) +
  theme_bw(base_size = 16)
dev.off()

pdf("targetBoth_Q1.pdf")
ggarrange(a, b, nrow = 2, ncol = 1,
          labels = c("A", "B"))
dev.off()
