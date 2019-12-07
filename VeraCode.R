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
