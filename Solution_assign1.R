library(readxl)
library(Mass)
library(MASS)
library(xlsx)
setwd("E:/Texas A&M/609-PED")
nfldata2 <- read_excel("nfldata2.xlsx")
View(nfldata2)
mat<- as.data.frame(nfldata2)
summary(mat)
#Matrix by difference of points method
P=matrix(data = 0, nrow = 32, ncol = 32)
for (i in 1:256) {
  k=mat$Winner[i]
  for (j in 1:256) {
    l=mat$Loser[j]
    if(i==j){
      P[l,k]=0.85*mat$diff[i]
    }
  }
}
S= matrix(data=0.15, nrow=32, ncol=32)
Pfinal=P+S
for(i in 1:32){
  for (j in 1:32) {
    Pfinal[i,j] = Pfinal[i,j]/sum(Pfinal[i,1:32])
    }
}
write.xlsx(Pfinal, "E:/Texas A&M/609-PED/Pfinal_1.xlsx")
##Compute Pinf
eig=eigen(Pfinal)
rvect=eig$vectors
lvect=ginv(eig$vectors)
lam<-eig$values
rvect%*%diag(lam)%*%ginv(rvect)
Pinf_1<-Re(lvect[1,]/sum(lvect[1,]))
Pinf_1
write.xlsx(Pinf_1, "E:/Texas A&M/609-PED/Pinf_1.xlsx")

#matrix by assigning W/L scores individually 
P2=matrix(data = 0, nrow = 32, ncol = 32)
for (i in 1:256) {
  k=mat$Winner[i]
  for (j in 1:256) {
    l=mat$Loser[j]
    if(i==j){
      P2[k,l]=mat$PL[i]
      P2[l,k]=mat$PW[i]
    }
  }
}
Pfinal_2 <- P2
for(i in 1:32){
  for (j in 1:32) {
    Pfinal_2[i,j] = P2[i,j]/sum(P2[i,1:32])
  }
}
write.xlsx(Pfinal_2, "E:/Texas A&M/609-PED/Pfinal_2.xlsx")
eig=eigen(Pfinal_2)
rvect=eig$vectors
lvect=ginv(eig$vectors)
lam<-eig$values
rvect%*%diag(lam)%*%ginv(rvect)
Pinf_2<-Re(lvect[1,]/sum(lvect[1,]))
Pinf_2
write.xlsx(Pinf_2, "E:/Texas A&M/609-PED/Pinf_2.xlsx")

## Random Walk Method
P3=matrix(data = 0, nrow = 32, ncol = 32)
for (i in 1:256) {
  k=mat$Winner[i]
  for (j in 1:256) {
    l=mat$Loser[j]
    if(i==j){
      P3[l,k]=1
    }
  }
}
Pfinal_3 <- P3
for(i in 1:32){
if(sum(Pfinal_3[i,])==0){
  Pfinal_3[i,]=1/32
   }
}
for(i in 1:32){
  for (j in 1:32) {
    Pfinal_3[i,j] = Pfinal_3[i,j]/sum(Pfinal_3[i,1:32])
  }
}
write.xlsx(Pfinal_3, "E:/Texas A&M/609-PED/Pfinal_3.xlsx")
eig=eigen(Pfinal_3)
rvect=eig$vectors
lvect=ginv(eig$vectors)
lam<-eig$values
rvect%*%diag(lam)%*%ginv(rvect)
Pinf_3<-Re(lvect[1,]/sum(lvect[1,]))
Pinf_3
write.xlsx(Pinf_3, "E:/Texas A&M/609-PED/Pinf_3.xlsx")

