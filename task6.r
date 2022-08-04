library(corrplot)

data <- read.csv('var_17.csv',header = T)
head(data)

states <- as.data.frame(data[,c("V1","V2", "V3", "V4","V5", "V6", "V7", "V8", "V9")])
cor_table <- cor(states)

col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#000000"))
corrplot(cor_table, method = "shade", col = col4(10), cl.length = 11, order = "AOE", addCoef.col = "red")

(ncol <- dim(data)[2])
(nrow <- dim(data)[1])
stocks <- data[,2:ncol]
rates <- stocks[2:nrow,] / stocks [1:(nrow -1),] - 1
rates <- rates[1:(nrow-2),]
pca <- prcomp(rates, scale = TRUE)
summary
tot <- pca$sdev
dim(pca$rotation)
scores <- function(ldata,V10,number)
{
  cdata <- ldata
  m <- dim(ldata)[2]
  for (i in 1:m)
    cdata[,i] <- ldata[,i] - pca$center[i]
  loads <- pca$rotation[,1:number]
  cdata <- as.matrix(cdata)
  f <- cdata %*% loads
  return (f)
}
number <- 8
fscores <- scores(rates,V9,number)
matplot(fscores,type = 'l',lty = 1, col = 1:number, main = 'Factor Scores')
stocks <- data[,2:ncol]
rates <- stocks[2:nrow,] / stocks [1:(nrow-1),] - 1
rates <- rates[1:(nrow-2),]
head(rates)
ss <- summary(pca)
mp <- barplot(pca$sdev,main = "Standard deviation explained", col = "blue")
mp <- barplot(ss$importance[2,],main = "Importance of Factors.", col = "blue")
his <- ss$importance[3,]
his[1:9]<- NA
r <- as.matrix(cbind(ss$importance[3,],his))
mp <- matplot(1:9,r,type = 'h',lty = 1, lwd = 10,main = "Cumulative proportion.", col = c("blue",'red')) 
abline(h = 0.8,col = 'black')
number <- 8
fscores <- scores(rates,ss,number)
means <- c(mean(fscores[,1]),mean(fscores[,2]),mean(fscores[,3]),mean(fscores[,4]),mean(fscores[,5]),mean(fscores[,6]),mean(fscores[,7]),mean(fscores[,8]))
means

fscores <- as.matrix(fscores)
cor(fscores)
cor_table <- cor(fscores)
col4 <- colorRampPalette(c("#7F0000","#FF7F00", "#7FFF7F", "#007FFF","#000000"))
corrplot(cor_table, method = "shade", col = col4(10), cl.length = 11, order = "AOE", addCoef.col = "red")

number <- 4
fscores <- scores(rates,ss,number)
matplot(fscores,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores')
means <- c(mean(fscores[,1]),mean(fscores[,2]),mean(fscores[,3]),mean(fscores[,4]))
means


tot <- ss$importance[2,]
rownames(ss$rotation)
matplot(cbind(ss$rotation[,1],ss$rotation[,2],ss$rotation[,3]),type = 'b',pch=21,lwd = 2,
        col = c("blue","green","magenta"),main= "Graph",ylab = "loadings",xlab="maturity",lty=1 )
restoreData<- function(fscores,loadings,center)
{
  npca <- dim(fscores)[2]
  myeigen <- t(loadings[,1:npca])
  rest <- fscores %*%myeigen
  m <- length(center)
  if (m == dim(rest)[2])
  {
    for (i in 1:m)
      rest[,i] <- rest[,i]+center[i]
  }
  return(rest)
}
rest <- restoreData(fscores,pca$rotation,pca$center)
head(rest)
r <- unlist(rates[1,])
matplot(cbind(rest[1,],r),type ='b',pch=21,lwd = 2,main = 'Restored rates',col = c('blue','green'),lty = 1)
legend('topleft',c('original rates','restored'),lty=1,lwd=2,col=c('blue','green') )

