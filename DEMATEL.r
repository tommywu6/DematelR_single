Acknowledgements. 
I would like to express my sincere thanks to Professor Wei-Wen Wu and Hau-Ting Nian for helping the completion of this R code in terms of DEMATEL.
#.............................................................
setwd("")
Z=read.csv("myfile.csv", header=TRUE, sep=",", row.names=1)
D=c(); R=c()
maxsum=sum(Z[1,])
for (i in 1:nrow(Z)) {
   maxsum=max(maxsum, sum(Z[i,]))
}
X=Z/maxsum
X1= diag(nrow(X)) - X   
X2=solve(X1)    
T = as.matrix(X) %*%X2
for (i in 1:nrow(T)) {
   D[i]=sum(T[i,])
}
for (i in 1:ncol(T)) {
   R[i]=sum(T[,i])
}
x=D+R; y=D-R    
Criterion = row(Z)[,1]  
df=data.frame(Criterion, D,R, x, y)
df$Criterion = paste("C", Criterion, sep="")
colnames(df)<-c("Criterion", "D","R", "D+R", "D-R")
plot(x, y, type='p',
main="DEMATEL & XXX Model ", sub="The causal diagram",xlab="D+R", ylab="D-R",cex=0.6) 
abline(h=mean(y),v=mean(x))
text(x, y, df$Criterion)
View(df)
