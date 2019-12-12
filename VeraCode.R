### EXERCISE 10

# Imagine a cancer cell in a tumor that spontaneously exhibited a mutation that confers drug 
# resistance. The mutation does not have any positive or negative effects on growth rate of 
# that sub-population when the cancer drug is absent. 
# However, when the cancer drug is present the mutant sub-population grows at 50% of its growth
# rate in the absence of the drug and the non-mutant sub-population declines rapidly. 
# The model we will use to represent the growth of the two sub-populations is this:

    ## set the working directory

setwd('/Users/claudiaveraarias/Documents/ND_Classes/Fall_Semester_2019/Biocomputing/R/W14_BC')


    ## Packages

library(ggplot2)


    ## 1) Parameters

K=1000000
r=0.1
Rn=-0.1
Rm=0.05
n0=99
m0=1
time=800
n<-numeric(length = time)
m<-numeric(length = time)
n[1]=n0
m[1]=m0

    ## 2) equations
       
        #No treatment

    n[t+1]=n[t]+r*n[t]*(1-((n[t]+m[t])/K)) 
    m[t+1]=m[t]+r*m[t]*(1-((n[t]+m[t])/K))

        # Treatment

    n[t+1]=n[t]+Rn*n[t]*(1-((n[t]+m[t])/K)) 
    m[t+1]=m[t]+Rm*m[t]*(1-((n[t]+m[t])/K))


   ## 3) Function and loop

cancer <- function(n0=99, m0=1, r=0.1, Rn=-0.1, Rm=0.05, K=1000000, time=800){
  n<-numeric(length = time)
  m<-numeric(length = time)
  n[1]=n0
  m[1]=m0
  
  for(t in 1:(time-1)){
    print(t)
  if(t < 250){
    n[t+1]=n[t]+r*n[t]*(1-((n[t]+m[t])/K)) 
    m[t+1]=m[t]+r*m[t]*(1-((n[t]+m[t])/K))
  }
  else
  {
    n[t+1]=n[t]+Rn*n[t]*(1-((n[t]+m[t])/K)) 
    m[t+1]=m[t]+Rm*m[t]*(1-((n[t]+m[t])/K))
  }
  }
  return(data.frame(n,m))
}

    ## 4) Data frame with all the data

PlotCancer <- cancer()
head(PlotCancer)
PlotCancer$time = 1:nrow(PlotCancer)

   ## 5) Plot

ggplot(data=PlotCancer)+
  geom_line(aes(x = time, y = n), col = "blue")+
  geom_line(aes(x = time, y = m), col = "green")+
  ylab("# Cells") + xlab("# Days") +
  theme_classic()
