dat<-read.csv("/Users/chuntamy/Desktop/MVA/PCA/HW 4.4 food data.csv", row.name="City")
dat  #[Print data]
cov(dat) #covariance matrix
summary(dat)
eigen(cov(dat))
eigen(cov(dat))$values   #[eigenvalues]
#[1] 216.79440  79.12794  62.26846  34.67047  22.22005

eigen(cov(dat))$vector
#[5,] -0.3471543  0.06289354 -0.32619100 -0.6070257 -0.63296724

eigenvectormatrix=as.matrix(eigen(cov(dat))$vector)    #[eigenvectors]
#           [,1]        [,2]        [,3]       [,4]        [,5]
#[1,] -0.4529089 -0.05515147  0.21435116  0.6856702 -0.52511130
#[2,] -0.7146773 -0.48679539 -0.02261341 -0.1338116  0.48358009
#[3,] -0.3391656  0.75632931 -0.43256354  0.1976187  0.29456459
#[4,] -0.2203644  0.42895099  0.81242257 -0.3231370  0.05470465
eigenvectormatrix

#均值化修正
CM=as.matrix(colMeans(dat))
CMM=t(CM%*%matrix(1,nrow=1,nrow(dat)))
datcormatrix=as.matrix(dat-CMM)

#PC score
pcscore=datcormatrix%*%eigenvectormatrix  #[Principal compoments scores ]
pcscore
cor(cbind(pcscore, dat), method="pearson")  #[loading]
plot(pcscore[,1],pcscore[,2])
text(pcscore[,1:2])

pca<-princomp(dat, cor=F) #[use mean-corrected data] 
pca
#Standard deviations:
#Comp.1    Comp.2    Comp.3    Comp.4    Comp.5 
#14.413927  8.708096  7.724891  5.764189  4.614566 

summary(pca)
variance = pca $sdev^2 / sum(pca $sdev^2)

#作圖scree plot選擇幾個主成份
library(ggplot2)
qplot(c(1:5), variance) +
  geom_line() +
  xlab('Number of Principal Component-1') + ylab('Variance Explained') +
  ggtitle('Scree plot') + ylim(0, 1)

##################################################################

#Based on standardized data
std<-scale(dat)  #standardized correlted data
std
cov(std)  #covariance
eigen(cov(std))

eigen(cov(std))$values   #[eigenvalues]
#2.4393555 0.9295911 0.8332378 0.5328728 0.2649429

eigen(cov(std))$vector   #[eigenvectors]
#           [,1]        [,2]       [,3]        [,4]        [,5]
#[1,] -0.5099267 -0.05649609  0.4017162  0.53197875  0.54074549
#[2,] -0.5200718  0.27761601  0.4074371 -0.07148075 -0.69378685
#[3,] -0.3973106 -0.09940133 -0.7684773  0.43041438 -0.23759156
#[4,] -0.2909422 -0.87675286  0.0713631 -0.37257819 -0.05243928
#[5,] -0.4764421  0.37571444 -0.2774329 -0.62275040  0.40872301

eigenvectormatrix_sd=as.matrix(eigen(cov(std))$vector)  
eigenvectormatrix_sd
sddatmatrix=as.matrix(std)
sddatmatrix

pcscore_sd=sddatmatrix%*%eigenvectormatrix_sd  #[Principal compoments scores ]
pcscore_sd
cor(cbind(pcscore_sd, std), method="pearson")  #[loading]
plot(pcscore_sd[,1],pcscore_sd[,2])
text(pcscore_sd[,1:2])

pca2<-princomp(std, cor=F) #[use std data] 
pca2
summary(pca2)
variance = pca2 $sdev^2 / sum(pca2 $sdev^2)
library(ggplot2)
qplot(c(1:5), variance) +
  geom_line() +
  xlab('Number of Principal Component-2') + ylab('Variance Explained') +
  ggtitle('Scree plot') + ylim(0, 1)

