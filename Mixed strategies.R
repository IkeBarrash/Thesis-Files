rm(list=ls())
#possible 3D graphs:
#W and R
library(ggplot2)
library(tidyverse)
#Test values
T=1
A=5
E=.25
W=1
R=.5
#graphed values
T=8
A=12
E=.25
W=6
R=.5
#standardized values
T=1
A=1
E=.25
W=1
R=.5

P = -(T*(1-R))/(-A*E-W-R*A*E+R*W-T-R*A)
Q = A/(W+A-E*A)


U.Squo = matrix(c(-E*A-W-R*(E*A-W),0,T-A-R*(T+A),T-R*T),nrow=2,ncol=2,byrow=TRUE)
U.Rise = matrix(c(E*A-W,0,T+A,T),nrow=2,ncol=2,byrow=TRUE)
lab.Squo = matrix(c("AC","BC","AD","BD"),nrow=2,ncol=2,byrow=TRUE)
# P*U.Squo[1,1]+(1-P)*U.Squo[1,2]
# P*U.Squo[2,1]+(1-P)*U.Squo[2,2]
# P*U.Squo[2,1]+U.Squo[2,2]-P*U.Squo[2,2]
# 
# P*U.Squo[1,1] = P*U.Squo[2,1]+U.Squo[2,2]-P*U.Squo[2,2]
# P*(U.Squo[1,1]+U.Squo[2,2]-U.Squo[2,1]) = U.Squo[2,2]
# P = U.Squo[2,2]/(U.Squo[1,1]+U.Squo[2,2]-U.Squo[2,1])
# 
# P = (T-R*T)/((-(E)*A-W-R*((E)*A-W))+(T-R*T)-(T-A-R*(T+A)))
# P = (T-R*T)/((-E*A-W-R*E*A+R*W)+(T-R*T)-(T-A-R*T-R*A))
# P = (T-R*T)/((-E*A-W-R*E*A+R*W)-(-A-R*A))
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)

# Q*U.Rise[1,1]+(1-Q)*U.Rise[2,1]
# Q*U.Rise[1,2]+(1-Q)*U.Rise[2,2]
# Q*U.Rise[1,1]-Q*U.Rise[1,2]+Q*U.Rise[2,2]-Q*U.Rise[2,1]=U.Rise[2,2]-U.Rise[2,1]
# Q*U.Rise[1,1]+Q*U.Rise[2,2]-Q*U.Rise[2,1]=U.Rise[2,2]-U.Rise[2,1]
Q = (U.Rise[2,2]-U.Rise[2,1])/(U.Rise[1,1]+U.Rise[2,2]-U.Rise[2,1])


#standardized values
T=1
A=1
E=.25
W=1
R=.5
i=1
#Mixed Strategies for R
R = seq(-.2,1,.0001)
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
Q = rep(A/(W+A-E*A),length(P))
#Pure Strategies for R
for(i in 1:length(Q)){
  if((-E*A-W-R[i]*(E*A-W)>T-A-R[i]*(T+A))&(0>T-R[i]*T)){
    Q[i]=1
    if(E*A-W>0){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((-E*A-W-R[i]*(E*A-W)<T-A-R[i]*(T+A))&(0<T-R[i]*T)){
    Q[i]=0
    if(T+A>T){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((E*A-W>0)&(T+A>T)){
    P[i]=1
    if(-E*A-W-R[i]*(E*A-W)>T-A-R[i]*(T+A)){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
  if((E*A-W<0)&(T+A<T)){
    P[i]=0
    if(0>T-R[i]*T){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
}
#Graphing R
ggplot() + theme(text = element_text(size=13)) +
  geom_line(aes(x=R,y=P, colour = "P-Rising Power Aggression Strategy"),size = 1.2) + 
  geom_line(aes(x=R,y=Q, colour = "Q-Status Quo Power Contain Strategy"),size = 1.2) +  
  geom_line(aes(x=R,y=0)) + 
  geom_line(aes(x=R,y=1)) +  
  labs(x="Relative Concern", y = "Mix Probabilities") + theme(legend.position = "bottom") + 
  guides(colour = guide_legend("Probabilities")) + scale_y_continuous(limits = c(0, 1)) +
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))


#Mixed strategies for T
R=.5
T = seq(-15,1.5,.0005)
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
Q = rep(A/(W+A-E*A),length(P))

#Pure Strategies for T
for(i in 1:length(Q)){
  if((-E*A-W-R*(E*A-W)>T[i]-A-R*(T[i]+A))&(0>T[i]-R*T[i])){
    Q[i]=1
    if(E*A-W>0){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((-E*A-W-R*(E*A-W)<T[i]-A-R*(T[i]+A))&(0<T[i]-R*T[i])){
    Q[i]=0
    if(T[i]+A>T[i]){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((E*A-W>0)&(T[i]+A>T[i])){
    P[i]=1
    if(-E*A-W-R*(E*A-W)>T[i]-A-R*(T[i]+A)){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
  if((E*A-W<0)&(T[i]+A<T[i])){
    P[i]=0
    if(0>T[i]-R*T[i]){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
}
#Graph for T
ggplot() + theme(text = element_text(size=13)) +
  geom_line(aes(x=T,y=P, colour = "P-Rising Power Aggression Strategy"), size=1.2) + 
  geom_line(aes(x=T,y=Q, colour = "Q-Status Quo Power Contain Strategy"), size=1.2) +  
  geom_line(aes(x=T,y=0)) + 
  geom_line(aes(x=T,y=1)) +  
  labs(x="Trade value", y = "Mix Probabilities") + theme(legend.position = "bottom") + guides(colour = guide_legend("Probabilities"))+scale_y_continuous(limits = c(0, 1))+
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))

#Mixed strategies for A
T = 1
A=seq(-1,5,.0002)
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
Q = A/(W+A-E*A)
#Pure strategies for A
#Pure Strategies for T
for(i in 1:length(Q)){
  if((-E*A[i]-W-R*(E*A[i]-W)>T-A[i]-R*(T+A[i]))&(0>T-R*T)){
    Q[i]=1
    if(E*A[i]-W>0){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((-E*A[i]-W-R*(E*A[i]-W)<T-A[i]-R*(T+A[i]))&(0<T-R*T)){
    Q[i]=0
    if(T+A[i]>T){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((E*A[i]-W>0)&(T+A[i]>T)){
    P[i]=1
    if(-E*A[i]-W-R*(E*A[i]-W)>T-A[i]-R*(T+A[i])){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
  if((E*A[i]-W<0)&(T+A[i]<T)){
    P[i]=0
    if(0>T-R*T){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
}
#Plot of A
ggplot() + theme(text = element_text(size=13)) +
  geom_line(aes(x=A,y=P, colour = "P-Rising Power Aggression Strategy"), size=1.2) + 
  geom_line(aes(x=A,y=Q, colour = "Q-Status Quo Power Contain Strategy"), size=1.2) +  
  geom_line(aes(x=A,y=0)) + 
  geom_line(aes(x=A,y=1)) +  
  labs(x="Aggressive Capacity", y = "Mix Probabilities") + theme(legend.position = "bottom") + guides(colour = guide_legend("Probabilities")) + scale_y_continuous(limits = c(0, 1))+
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))

#Mixed Strategies for E
A = 1
E = seq(.001,1,.0001)
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
Q = A/(W+A-E*A)
#Pure Strategies for E
for(i in 1:length(Q)){
  if((-E[i]*A-W-R*(E[i]*A-W)>T-A-R*(T+A))&(0>T-R*T)){
    Q[i]=1
    if(E[i]*A-W>0){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((-E[i]*A-W-R*(E[i]*A-W)<T-A-R*(T+A))&(0<T-R*T)){
    Q[i]=0
    if(T+A>T){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((E[i]*A-W>0)&(T+A>T)){
    P[i]=1
    if(-E[i]*A-W-R*(E[i]*A-W)>T-A-R*(T+A)){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
  if((E[i]*A-W<0)&(T+A<T)){
    P[i]=0
    if(0>T-R*T){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
}
#Plot for E
ggplot() + theme(text = element_text(size=13)) +
  geom_line(aes(x=E,y=P, colour = "P-Rising Power Aggression Strategy"), size=1.2) + 
  geom_line(aes(x=E,y=Q, colour = "Q-Status Quo Power Contain Strategy"), size=1.2) +  
  geom_line(aes(x=E,y=0)) + 
  geom_line(aes(x=E,y=1)) +  
  labs(x="Effectiveness given Containment", y = "Mix Probabilities") + theme(legend.position = "bottom") + guides(colour = guide_legend("Probabilities")) + scale_y_continuous(limits = c(0, 1))+
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))

#Mixed Strategies for W
E = .25
W = seq(-.2,1.6,.0005)
P = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
Q = A/(W+A-E*A)

#Pure Strategies for W
for(i in 1:length(Q)){
  if((-E*A-W[i]-R*(E*A-W[i])>T-A-R*(T+A))&(0>T-R*T)){
    Q[i]=1
    if(E*A-W[i]>0){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((-E*A-W[i]-R*(E*A-W[i])<T-A-R*(T+A))&(0<T-R*T)){
    Q[i]=0
    if(T+A>T){
      P[i]=1
    }
    else{
      P[i]=0
    }
  }
  if((E*A-W[i]>0)&(T+A>T)){
    P[i]=1
    if(-E*A-W[i]-R*(E*A-W[i])>T-A-R*(T+A)){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
  if((E*A-W[i]<0)&(T+A<T)){
    P[i]=0
    if(0>T-R*T){
      Q[i]=1
    }
    else{
      Q[i]=0
    }
  }
}
#Plot of W
ggplot() + theme(text = element_text(size=13)) +
  geom_line(aes(x=W,y=P, colour = "P-Rising Power Aggression Strategy"), size=1.2) + 
  geom_line(aes(x=W,y=Q, colour = "Q-Status Quo Power Contain Strategy"), size=1.2) +  
  geom_line(aes(x=W,y=0)) + 
  geom_line(aes(x=W,y=1)) +  
  labs(x="Risk of War", y = "Mix Probabilities") + theme(legend.position = "bottom") + guides(colour = guide_legend("Probabilities"))+ scale_y_continuous(limits = c(0, 1))+
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))


#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
#**************************************3D-Code**********************************
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************

#****************************************************************************
#********************************Opium War***********************************
#****************************************************************************
#For Chinese 1840
T = 0
A = 1
W = .25
E = .1
R = 0
val1 = seq(-1,4,.05)
val2 = seq(-.5,1,.015)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 0

for(T in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = p.equ
    #If there is a pure strategy for the status-quo power determining which it is and best response
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)&(CB>DB)){
        #If A is best response to C
        if(AC>BC){
          z[x,y] = 1
        }
        #If B is best response to C
        else{
          z[x,y] = 0
        }
      }
      #If D is dominant
      else{
        #If A is best response to D
        if(AD>BD){
          z[x,y] = 1
        }
        #If B is best response to D
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the rising power determining which it is
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If rising power ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)|(AD>BD)){
        z[x,y] = 1
      }
      #If B is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}

#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 210, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "red",
      xlab = "Trade at Risk", ylab = "British Hawkishness",
      zlab = "Probability of Chinese Aggression", ticktype = "detailed"
)

#For UK 1840
T = 0
A = 1
W = .25
E = .1
R = 0
val1 = seq(-1,4,.05)
val2 = seq(-.5,1,.015)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 1

for(T in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = q.equ
    #If there is a pure strategy for the rising power determining which it is and best response
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)&(AD>BD)){
        #If C is best response to A
        if(CA>DA){
          z[x,y] = 1
        }
        #If D is best response to A
        else{
          z[x,y] = 0
        }
      }
      #If B is dominant
      else{
        #If C is best response to B
        if(CB>DB){
          z[x,y] = 1
        }
        #If D is best response to B
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the SQ power determining which it is
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If rising power ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)|(CB>DB)){
        z[x,y] = 1
      }
      #If D is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}
#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 60, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "cyan",
      xlab = "Trade at Risk", ylab = "British Hawkishness", 
      zlab = "Probability of British Containment", ticktype = "detailed"
)
#****************************************************************************
#********************************Korean War**********************************
#****************************************************************************
#For Chinese 1950
T = .25
A = 0
W = 2
E = .5
R = .5
val1 = seq(-1,5,.06)
val2 = seq(-.2,1,.012)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 1

for(A in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = p.equ
    #If there is a pure strategy for the status-quo power determining which it is and best response
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)&(CB>DB)){
        #If A is best response to C
        if(AC>BC){
          z[x,y] = 1
        }
        #If B is best response to C
        else{
          z[x,y] = 0
        }
      }
      #If D is dominant
      else{
        #If A is best response to D
        if(AD>BD){
          z[x,y] = 1
        }
        #If B is best response to D
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the rising power determining which it is
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If rising power ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)|(AD>BD)){
        z[x,y] = 1
      }
      #If B is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}

#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 210, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "red",
      xlab = "Aggressive Utility", ylab = "U.S. Hawkishness",
      zlab = "Probability of Chinese Aggression", ticktype = "detailed"
)

#For US 1950
T = .25
A = 0
W = 2
E = .5
R = .5
val1 = seq(-1,5,.06)
val2 = seq(-.2,1,.012)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 1

for(A in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = q.equ
    #If there is a pure strategy for the rising power determining which it is and best response
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)&(AD>BD)){
        #If C is best response to A
        if(CA>DA){
          z[x,y] = 1
        }
        #If D is best response to A
        else{
          z[x,y] = 0
        }
      }
      #If B is dominant
      else{
        #If C is best response to B
        if(CB>DB){
          z[x,y] = 1
        }
        #If D is best response to B
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the SQ power determining which it is(round to deal with float errors)
    if((round(p.equ,5)>1)|(p.denom==0)|(round(p.equ,5)<0)){
      #If rising power ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)|(CB>DB)){
        z[x,y] = 1
      }
      #If D is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}
#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 335, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "cyan",
      xlab = "Aggressive Utility", ylab = "U.S. Hawkishness",
      zlab = "Probability of U.S. Containment", ticktype = "detailed"
)
#****************************************************************************
#********************************Cold War************************************
#****************************************************************************
#For Chinese 1960-2010
T = 0
A = 1
W = 0
E = .5
R = .5
val1 = seq(0,1.5,.015)
val2 = seq(0,1.5,.015)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 1

for(T in val1){
  x = x + 1
  y = 0
  for(W in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = p.equ
    #If there is a pure strategy for the status-quo power determining which it is and best response
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)&(CB>DB)){
        #If A is best response to C
        if(AC>BC){
          z[x,y] = 1
        }
        #If B is best response to C
        else{
          z[x,y] = 0
        }
      }
      #If D is dominant
      else{
        #If A is best response to D
        if(AD>BD){
          z[x,y] = 1
        }
        #If B is best response to D
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the rising power determining which it is
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If rising power ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)|(AD>BD)){
        z[x,y] = 1
      }
      #If B is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}

#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 210, zlim = c(0,1),
      ltheta = 180, lphi = 25, r = 10, d = 50, shade=.75, col = "red",
      xlab = "Trade at Risk", ylab = "Risk of War",
      zlab = "Probability of Chinese Aggression", ticktype = "detailed"
)

#For US 1960-2010
T = 0
A = 1
W = 0
E = .5
R = .5
val1 = seq(0,1.5,.015)
val2 = seq(0,1.5,.015)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 0
for(T in val1){
  x = x + 1
  y = 0
  for(W in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = q.equ
    #If there is a pure strategy for the rising power determining which it is and best response
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)&(AD>BD)){
        #If C is best response to A
        if(CA>DA){
          z[x,y] = 1
        }
        #If D is best response to A
        else{
          z[x,y] = 0
        }
      }
      #If B is dominant
      else{
        #If C is best response to B
        if(CB>DB){
          z[x,y] = 1
        }
        #If D is best response to B
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the SQ power determining which it is(round to deal with float errors)
    if((round(p.equ,5)>1)|(p.denom==0)|(round(p.equ,5)<0)){
      #If rising power ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)|(CB>DB)){
        z[x,y] = 1
      }
      #If D is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}
#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 150, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "cyan",
      xlab = "Trade at Risk", ylab = "Risk of War",
      zlab = "Probability of U.S. Containment", ticktype = "detailed"
)
#********************************Future**************************************
#For Chinese Future
T = 2
A = 1.5
W = 1
E = 0
R = 0
val1 = seq(0,1,.01)
val2 = seq(-.2,1,.012)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 1

for(E in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = p.equ
    #If there is a pure strategy for the status-quo power determining which it is and best response
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>DA)&(CB>DB)){
        #If A is best response to C
        if(AC>BC){
          z[x,y] = 1
        }
        #If B is best response to C
        else{
          z[x,y] = 0
        }
      }
      #If D is dominant
      else{
        #If A is best response to D
        if(AD>BD){
          z[x,y] = 1
        }
        #If B is best response to D
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the rising power determining which it is
    if((q.equ>=1)|(q.denom==0)|(q.equ<=0)){
      #If rising power ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)|(AD>BD)){
        z[x,y] = 1
      }
      #If B is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}

#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 210, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "red",
      xlab = "Effectiveness given Containment", ylab = "U.S. Hawkishness",
      zlab = "Probability of Chinese Aggression", ticktype = "detailed"
)

#For NEW US Future
T = 1
A = 1.5
W = 1
E = 0
R = 0
val1 = seq(0,1,.01)
val2 = seq(-.2,1,.012)
x = 0
y = 0
z=matrix(nrow = length(val1),ncol=length(val2))
set_undef = 0

for(E in val1){
  x = x + 1
  y = 0
  for(R in val2){
    y = y + 1
    p.equ = (T-R*T)/(-E*A-W-R*E*A+R*W+A+R*A)
    p.denom = (-E*A-W-R*E*A+R*W+A+R*A)
    q.equ = A/(W+A-E*A)
    q.denom = W+A-E*A
    AC = E*A-W
    AD = T+A
    BC = 0
    BD = T
    CA = -E*A-W-R*(E*A-W)
    CB = 0
    DA = T-A-R*(T+A)
    DB = T-R*T
    #If there is no pure strategy
    z[x,y] = q.equ
    #If there is a pure strategy for the rising power determining which it is and best response
    if((q.equ>1)|(q.denom==0)|(q.equ<0)){
      #If ratio is undefined
      if(q.denom==0){
        z[x,y] = set_undef
      }
      #If A is dominant
      else if((AC>BC)&(AD>BD)){
        #If C is best response to A
        if(CA>DA){
          z[x,y] = 1
        }
        #If D is best response to A
        else{
          z[x,y] = 0
        }
      }
      #If B is dominant
      else{
        #If C is best response to B
        if(CB>DB){
          z[x,y] = 1
        }
        #If D is best response to B
        else{
          z[x,y] = 0
        }
      }
    }
    #If there is a pure strategy for the SQ power determining which it is
    if((p.equ>1)|(p.denom==0)|(p.equ<0)){
      #If rising power ratio is undefined
      if(p.denom==0){
        z[x,y] = set_undef
      }
      #If C is dominant
      else if((CA>=DA)|(CB>=DB)){
        z[x,y] = 1
      }
      #If D is dominant
      else{
        z[x,y] = 0
      }
    }
  }
}

#Val 1 goes first val 2 goes second
persp(val1, val2, z, phi = 45, theta = 60, zlim = c(0,1),
      ltheta = 0, lphi = 25, r = 10, d = 50, shade=.75, col = "cyan",
      xlab = "Effectiveness given Containment", ylab = "U.S. Hawkishness",
      zlab = "Probability of U.S. Containment", ticktype = "detailed"
)

