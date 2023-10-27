
rm(list = ls())

### Libraries (if necessary)

library(tidyverse)
library(ggpubr)
library(tictoc)

set.seed(2023)

##### Q3.i

H1 = function(x1, x2, a, b, r){
  
  sign(x1^2 + x2^2 - r^2)
}

H2 = function(x1, x2, a, b, r){
  
  sign((x1-a)^2 + (x2-b)^2 - r^2)
}

N = 100
x1 = seq(-1, 1, length = N)
x2 = x1

hVal1.1 = matrix(0, N, N)
hVal1.2 = hVal1.1
hVal1.3 = hVal1.1

hVal2.1 = hVal1.1
hVal2.2 = hVal1.1
hVal2.3 = hVal1.1

expComb = expand.grid(x1, x2)

for(i in 1:N){
  
  for (j in 1:N){
    
    hVal1.1[i, j] = H1(x1[i], x2[j], 0, 0, 1)
    hVal1.2[i, j] = H1(x1[i], x2[j], 0, 0.25, 0.5)
    hVal1.3[i, j] = H1(x1[i], x2[j], 0.5, -0.5, 1)
    
    hVal2.1[i, j] = H2(x1[i], x2[j], 0, 0, 1)
    hVal2.2[i, j] = H2(x1[i], x2[j], 0, 0.25, 0.5)
    hVal2.3[i, j] = H2(x1[i], x2[j], 0.5, -0.5, 1)
    
  }
}

h1.1Data = melt(hVal1.1)
h1.1Data = cbind(expComb, h1.1Data$value)
colnames(h1.1Data) = c("X1", "X2", "Sign")

a = ggplot(h1.1Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

h1.2Data = melt(hVal1.2)
h1.2Data = cbind(expComb, h1.2Data$value)
colnames(h1.2Data) = c("X1", "X2", "Sign")

b = ggplot(h1.2Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

h1.3Data = melt(hVal1.3)
h1.3Data = cbind(expComb, h1.3Data$value)
colnames(h1.3Data) = c("X1", "X2", "Sign")

c = ggplot(h1.3Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

h2.1Data = melt(hVal2.1)
h2.1Data = cbind(expComb, h2.1Data$value)
colnames(h2.1Data) = c("X1", "X2", "Sign")

d = ggplot(h2.1Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

h2.2Data = melt(hVal2.2)
h2.2Data = cbind(expComb, h2.2Data$value)
colnames(h2.2Data) = c("X1", "X2", "Sign")

e = ggplot(h2.2Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

h2.3Data = melt(hVal2.3)
h2.3Data = cbind(expComb, h2.3Data$value)
colnames(h2.3Data) = c("X1", "X2", "Sign")

f = ggplot(h2.3Data, aes(x = X1, y = X2, fill = Sign)) + 
  geom_raster(interpolate = TRUE) +
  xlab("X1") + ylab("X2") + labs(color = "Sign") +
  scale_fill_gradientn(colours = c("navyblue", "gold")) +
  theme_bw() +
  theme(legend.position = "none")

pdf("circles_Q3.A.pdf")
ggarrange(a, d , b, e, c, f, ncol = 2, nrow = 3, 
          labels = c("H1.1", "H2.1", "H1.2", "H2.2", "H1.3", "H2.3"),
          hjust = -0.2,
          vjust = 12,
          align = "v")
dev.off()

##### Q3.ii

