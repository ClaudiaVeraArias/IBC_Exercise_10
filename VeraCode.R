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

    
