### EXERCISE 10

# Imagine a cancer cell in a tumor that spontaneously exhibited a mutation that confers drug 
# resistance. The mutation does not have any positive or negative effects on growth rate of 
# that sub-population when the cancer drug is absent. 
# However, when the cancer drug is present the mutant sub-population grows at 50% of its growth
# rate in the absence of the drug and the non-mutant sub-population declines rapidly. 
# The model we will use to represent the growth of the two sub-populations is this:

    ## 1) Parameters
N0=0
M0=0
rN=0.1
rM=0.1
K=1000000
m=1/10000
time=50
rTNM=-0.1 #grow rate of cells non-mutant with drug treatment
rTMC=0.5 #mutant cells grow at the 50% of they capacity when the treatment is present

    ## 2) equation 
N0=0
N=numeric(length=time)
N[1]=N0

M0=0
M=numeric(length = time)
M[1]=M0


N(t+1)=N[t]+rN*N[t](1-((N[t]+M[t])/K))
M(t+1)=M[t]+rM*M[t](1-((N[t]+M[t])/K))

    
#### may be

Nc<-numeric(time)
Mc<-numeric(time)
Nc0<-0
Mc0<-50
Nc[1]<-Nc0
Mc[1]<-Mc0
K=1000000
r=0.1
time=50
test<-function(M0=50,r=0.1,K=100,time=50){
  Mc<-numeric(time)
  Mc0<-50
  Mc[1]<-Mc0
   for(i in 1:(time-1)){
Mc[i+1]<-Mc[i]+r*Mc[i]*(1-(999900+Mc[i])/K)+0.5*Mc[i]
}
return(Mc)
}



##fuction that works
prueba3<-function(Nc0=50,r=0.1,K=1000000,time=1000){
  Nc<-numeric(time)
  Nc0<-50
  Nc[1]<-Nc0
  for(i in 1:(time-1)){
    Nc[i+1]<-Nc[i]+r*Nc[i]*(1-(Nc[i]+100)/K)
  }
  return(Nc)
}
### 350



### Don't work :(

Test1<-function(Nc0=10000,Mc0=1,r=0.1,K=1000000,time=500){
  Nc<-numeric(time)
  Nc0<-10000
  Nc[1]<-Nc0
  Mc<-numeric(time)
  Mc0<-1
  Mc[1]<-Mc0
  for(i in 1:(time-1)){
    if(i<=100)
    Mc[i+1]<-Mc[i]+r*Mc[i]*(1-(999900+Mc[i])/K)-Mc[i]*0.5
  }else{
    Nc[i+1]<-Nc[i]+r*Nc[i]*(1-(Nc[i]+100)/K)-Nc[i]*-0.1
  }
  return(Nc)
  return(Mc)
}




#### other
#initial data
N0=50
M0=1
r=0.1
K=1000000
time=350
Ns=numeric(length = time)
Ns[1]=N0
Ms=numeric(length = time)
Ms[1]=M0

rNT=-0.1

#simulation
for(i in 1:200){
  Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)-Ns[i]*rNT
}

#plot
sim<-data.frame(time=1:length(Ns),N=Ns)
ggplot(data=sim,aes(x=time,y=N))+geom_line()+theme_classic()





#function

popsim<-function(N0=1,M0=1,r=0.2,K=1000000,time=350,rNT=-0.1){
Ns=numeric(length = time)
Ns[1]=N0
Ms=numeric(length = time)
Ms[1]=M0
for(i in 1:350){
  Ns[i+1]=Ns[i]+r*Ns[i]*(1-(Ns[i]+Ms[i])/K)-Ns[i]*rNT
}
return(Ns)
return(Ms)
}

Tumor=data.frame(time=1:351)
Tumor$Mutant<-popsim(rNT = 0.5)
Tumor$NoMutant<-popsim()
Tumor$NoCancer<-popsim(rNT = 0)

dim(Tumor)
ggplot(data=Tumor)+
  geom_line(aes(x=time,y=Mutant),col="blue")+
  geom_line(aes(x=time,y=NoMutant),col="pink")+
  geom_line(aes(x=time,y=NoCancer),col="purple")+
  theme_classic()
