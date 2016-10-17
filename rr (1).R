A<-1:24
dim(A)<-c(12,2)
A<-fix(A)
colnames(A)<-c("x","y")
A

A<-as.data.frame(A)
B<-lm(y~x,data = A)
summary(B)
B
confint(B,level = 0.99)
plot(B,which = 1)
