### EXERCISE 10

# Imagine a cancer cell in a tumor that spontaneously exhibited a mutation that confers drug 
# resistance. The mutation does not have any positive or negative effects on growth rate of 
# that sub-population when the cancer drug is absent. 
# However, when the cancer drug is present the mutant sub-population grows at 50% of its growth
# rate in the absence of the drug and the non-mutant sub-population declines rapidly. 
# The model we will use to represent the growth of the two sub-populations is this:

 ## set the working directory
setwd('/Users/claudiaveraarias/Documents/ND_Classes/Fall_Semester_2019/Biocomputing/R/W14_BC')

    ## oppen packages
library(ggplot2)

    ## 1) Parameters
N0=1
M0=1
r=0.2
K=1000000
time=350
rNT=0.9
rMT=0.5
    ## 2) equation 
Ns=numeric(length = time)
Ns[1]=N0
Ms=numeric(length = time)
Ms[1]=M0

Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)-Ns[i]*rNT
Ms[i+1]=Ms[i]+r*Ms[i]*(1-(Ns[i]+Ms[i])/K)-Ms[i]*rMT
Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)
    
    ## 3)Loop
popsim1<-function(N0=1,M0=1,r=0.2,K=1000000,time=350,rNT=0.9,rMT=0.5){
  Ns=numeric(length = time)
  Ns[1]=N0
  Ms=numeric(length = time)
  Ms[1]=M0
  for(i in 1:350){
    if(i<=100){
      Ms[i+1]=Ms[i]+r*Ms[i]*(1-(Ns[i]+Ms[i])/K)-Ms[i]*rMT
    }else if(i>=999900)
    Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)-Ns[i]*rNT
  }else{
    Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)
  }
  return(Ns)
  return(Ms)
}

   ## 4)Data frame

Tumor1=data.frame(time=1:351)
Tumor1$Mutant<-popsim1(rNT = -0.9,rMT = -0.5)
Tumor1$NoMutant<-popsim()
Tumor1$NoCancer<-popsim1(rNT = 0,rMT = 0)
  
   ## 5)Plot 

dim(Tumor)
ggplot(data=Tumor)+
  geom_line(aes(x=time,y=Mutant),col="blue")+
  geom_line(aes(x=time,y=NoMutant),col="pink")+
  geom_line(aes(x=time,y=NoCancer),col="purple")+
  theme_classic()



###Dic8
#parameters
K=1000000
KN=999900
KM=100
r=0.1
rN=-0.1
rM=-0.5
time=365
N0=1
M0=0

#equation
N <- numeric(length = time)
M <- numeric(length = time)

N[1]=N0
M[1]=M0

N[i+1]=N[i]+r*N[i]*(1-(N[i]+M[i])/K)-N[i]*rN
M[i+1]=M[i]+r*M[i]*(1-(N[i]+M[i])/K)-M[i]*rM

# loop

Drug<- function (N0=1, M0=0, r=0.1, K=1000000, rM=0.5,rN=0.9){
  for (i in 1:(time-1)){
  if(i<=100){
    Mu<-M[i+1]=M[i]+r*M[i]*(1-(N[i]+M[i])/K)-M[i]*rM
  }else{
    No<-N[i+1]=N[i]+r*N[i]*(1-(N[i]+M[i])/K)-N[i]*rN
  }
  V<-cbind(Mu+No)
  }
}

Drug1<-data.frame(time-1)
Drug1$Mutant<-Drug(rNT=0)
Drug1$NoMutant<-Drug(rMT=0)
Drug1$NoDrug<-Drug(rMT=0,rNT=0)

dim(Drug1)

ggplot(data=Drug1)+
  geom_line(aes(x=time,y=Mutant),col="blue")+
  geom_line(aes(x=time,y=NoMutant),col="pink")+
  geom_line(aes(x=time,y=NoDrug),col="purple")+
  theme_classic()
