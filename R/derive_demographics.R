# This script/function is intended to derive a bunch of demographic outputs from populations in COMPADRE and COMADRE w GPS coordinates
# Authors: Rob Salguero-Gomez
# Last modified: 11 March 2019 by Sam Levin

# rm(list=ls())


library(maps)
library(scales)
library(MASS)
library(popbio)
library(popdemo)
library(Matrix)
library(dplyr)


###Functions

#Function to trim down lx and mx at a given percentage before stationary convergence
qsdConvergence <- function(survMatrix, beginLife){
  uDim = dim(survMatrix)
  eig = eigen.analysis(survMatrix)
  qsd = eig$stable.stage
  qsd = as.numeric(t(matrix(qsd / sum(qsd))))

  #Set up a cohort
  nzero = rep(0, uDim[1]) #Set a population vector of zeros
  nzero[beginLife] = 1 #Set the first stage to =1
  n = nzero #Rename for convenience

  #Iterate the cohort (n= cohort population vector, p = proportional structure)
  dist = p = NULL
  survMatrix1 <- survMatrix
  for (j in 1:1500){ #j represent years of iteration
    p = n / sum(n) #Get the proportional distribution
    dist[j] = 0.5 * (sum(abs(p - qsd)))
    n = survMatrix1 %*% n #Multiply the u and n matrices to iterate
  }
  #Find the ages for convergence to 0.1, 0.05, and 0.01
  pick1 = min(which(dist < 0.1))
  pick2 = min(which(dist < 0.05))
  pick3 = min(which(dist < 0.01))
  convage = c(pick1, pick2, pick3)
  return(convage)
}


#Function to determine probability of reaching reproduction, age at maturity and reproductive lifespan (Code adapted from H. Caswell's matlab code):
lifeTimeRepEvents <- function(matU, matF, startLife = 1){
  uDim = dim(matU)[1]
  surv = colSums(matU)
  repLifeStages = colSums(matF)
  repLifeStages[which(repLifeStages>0)] = 1

  if(missing(matF) | missing(matU)){stop('matU or matF missing')}
  if(sum(matF,na.rm=T)==0){stop('matF contains only 0 values')}

  #Mean life expectancy (from Caswell's fundamental matrix approach 2001)
  N = solve(diag(matDim) - matU)
  Eta=colSums(N)[startLife]

  out = data.frame(Eta = Eta)

  #Max lifespan (from Morris and Doak book 2002)
  popVector=rep(0,dim(matU)[1])
  popVector[startLife]=100
  lifespanLeftover=matrix(0,1000,1)
  for (n in 1:1000){
    lifespanLeftover[n]=sum(popVector)
    popVector=matU%*%popVector
  }
  Lmax=min(which(lifespanLeftover<1))
  if(Lmax==Inf) {Lmax=999}

  out$Lmax=Lmax

  #Probability of survival to first reprod event
  Uprime = matU
  Uprime[,which(repLifeStages==1)] = 0
  Mprime = matrix(0,2,uDim)
  for (p in 1:uDim[1]) {
    if (repLifeStages[p]==1) Mprime[2,p] = 1 else
      Mprime[1,p] = 1-surv[p]
  }
  Bprime = Mprime%*%(ginv(diag(uDim)-Uprime))
  pRep = Bprime[2,startLife]

  out$pRep = pRep

  #Age at first reproduction (La; Caswell 2001, p 124)
  D = diag(c(Bprime[2,]))
  Uprimecond = D%*%Uprime%*%ginv(D)
  expTimeReprod = colSums(ginv(diag(uDim)-Uprimecond))
  La = expTimeReprod[startLife]

  out$La = La

  #Mean life expectancy conditional on entering the life cycle in the first reproductive stage
  firstRepLifeStage = min(which(repLifeStages==1))
  N = solve(diag(uDim[1])-matU)
  meanRepLifeExpectancy = colSums(N)[firstRepLifeStage]

  out$meanRepLifeExpectancy = meanRepLifeExpectancy

  #Life expectancy from mean maturity
  remainingMatureLifeExpectancy = colSums(N)[startLife]-La

  out$remainingMatureLifeExpectancy = remainingMatureLifeExpectancy

  return(out)
}

#Function to create a life table
makeLifeTable<-function(matU, matF = NULL, matC = NULL, startLife = 1, nSteps = 1000){

  matDim = ncol(matU)

  #Age-specific survivorship (lx) (See top function on page 120 in Caswell 2001):
  matUtemp = matU
  survivorship = array(NA, dim = c(nSteps, matDim))
  for (o in 1:nSteps){
    survivorship[o, ] = colSums(matUtemp %*% matU)
    matUtemp = matUtemp %*% matU
  }

  lx = survivorship[, startLife]
  lx = c(1, lx[1:(length(lx) - 1)])

  #Make room for dx and qx under assumption of 0.5 in age gap distributions

  #Start to assemble output object
  out = data.frame(x = 0:(length(lx)-1),lx = lx)

  if(!missing(matF)){
    if(sum(matF,na.rm=T)==0){
      warning("matF contains only 0 values")
    }
    #Age-specific fertility (mx, Caswell 2001, p. 120)
    ageFertility = array(0, dim = c(nSteps, matDim))
    fertMatrix = array(0, dim = c(nSteps, matDim))
    matUtemp2 = matU
    e = matrix(rep(1, matDim))
    for (q in 1:nSteps) {
      fertMatrix = matF %*% matUtemp2 * (as.numeric((ginv(diag(t(e) %*% matUtemp2)))))
      ageFertility[q, ] = colSums(fertMatrix)
      matUtemp2 = matUtemp2 %*% matU
    }
    mx = ageFertility[, startLife]
    mx = c(0, mx[1:(length(mx) - 1)])
    out$mx = mx
  }

  if(!missing(matC)){
    if(sum(matC,na.rm=T)==0){
      warning("matC contains only 0 values")
    }
    #Age-specific clonality (cx)
    ageClonality = array(0, dim = c(nSteps, matDim))
    clonMatrix = array(0, dim = c(nSteps, matDim))
    matUtemp2 = matU
    e = matrix(rep(1, matDim))
    for (q in 1:nSteps) {
      clonMatrix = matC %*% matUtemp2 * (as.numeric((ginv(diag(t(e) %*% matUtemp2)))))
      ageClonality[q, ] = colSums(clonMatrix)
      matUtemp2 = matUtemp2 %*% matU
    }
    cx = ageClonality[, startLife]
    cx = c(0, cx[1:(length(cx) - 1)])
    out$cx = cx
  }

  return(out)
}

#Function to calculate summary vital rates (not stage-dependent)
vitalRate <- function(matU, matF, matC=NULL){

  matA=matU+matF+matC
  matDim=dim(matA)[1]

  out = data.frame("SurvivalSSD"=NA,"ProgressionSSD"=NA,"RetrogressionSSD"=NA,"ReproductionSSD"=NA,"ClonalitySSD"=NA)

  #Extracting SSD corrected vital rate values
  SSD=eigen.analysis(matA)$stable.stage
  f=colSums(matF)
  out$ReproductionSSD=mean(f*SSD)
  c=colSums(matC)
  out$ClonalitySSD=mean(c*SSD)

  #Preparing survival-independent matrix to calculate individual growth rates
  uDistrib=matrix(NA,ncol=matDim,nrow=matDim)
  u=colSums(matU)
  out$SurvivalSSD=mean(u*SSD)

  #Making matrix for transitions conditional on survival
  for (j in which(u>0)) uDistrib[,j]=matU[,j]/u[j]
  UPrime=uDistrib
  UPrime[is.na(UPrime)]=0
  CPrime=colSums(matC)
  CPrime[is.na(CPrime)]=0
  UCPrime=UPrime+CPrime

  #Extracting proxy to individual progressive growth rate
  UCPrimeGrowth=UCPrime
  UCPrimeGrowth[upper.tri(UCPrime, diag = T)]=NA
  UCPrimeGrowth[matDim,matDim]=UCPrime[matDim,matDim]  #Putting back the last element of stasis bc there is likely growth on the top of class
  out$ProgressionSSD=mean(colSums(UCPrimeGrowth,na.rm=T)*SSD)
  #Extracting proxy to individual retrogressive growth rate
  UCPrimeShrinkage=UCPrime
  UCPrimeShrinkage[lower.tri(UCPrime, diag = T)]=NA
  out$RetrogressionSSD=mean(colSums(UCPrimeShrinkage,na.rm=T)*SSD)

  return(out)
}


#Function to calculate stage-dependent summary vital rates (pre-rep and reproductive)
vitalRateStage <- function(matU, matF, matC=NULL){

  matA=matU+matF+matC
  matDim=dim(matA)[1]

  #Determining prereproductive and reproductive stages
  firstRep=min(which(colSums(matF)>0))
  if (firstRep==1) {preRep=0} else {preRep=1:(firstRep-1)}
  rep=firstRep:matDim

  out = data.frame("SurvivalSSDPreRep"=NA,"ProgressionSSDPreRep"=NA,"RetrogressionSSDPreRep"=NA,"ClonalitySSDPreRep"=NA,
                   "SurvivalSSDRep"=NA,"ProgressionSSDRep"=NA,"RetrogressionSSDRep"=NA,"ReproductionSSDRep"=NA,"ClonalitySSDRep"=NA)

  #Extracting SSD corrected vital rate values
  SSD=eigen.analysis(matA)$stable.stage
  f=colSums(matF)
  out$ReproductionSSDRep=mean(f[rep]*SSD[rep])

  c=colSums(matC)
  out$ClonalitySSDPreRep=mean(c[preRep]*SSD[preRep])
  out$ClonalitySSDRep=mean(c[rep]*SSD[rep])

  #Preparing survival-independent matrix to calculate individual growth rates
  uDistrib=matrix(NA,ncol=matDim,nrow=matDim)
  u=colSums(matU)
  out$SurvivalSSDPreRep=mean(u[preRep]*SSD[preRep])
  out$SurvivalSSDRep=mean(u[rep]*SSD[rep])

  #Making matrix for transitions conditional on survival
  for (j in which(u>0)) uDistrib[,j]=matU[,j]/u[j]
  UPrime=uDistrib
  UPrime[is.na(UPrime)]=0
  CPrime=colSums(matC)
  CPrime[is.na(CPrime)]=0
  UCPrime=UPrime+CPrime

  #Extracting proxy to individual progressive growth rate
  UCPrimeGrowth=UCPrime
  UCPrimeGrowth[upper.tri(UCPrime, diag = T)]=NA
  UCPrimeGrowth[matDim,matDim]=UCPrime[matDim,matDim]  #Putting back the last element of stasis bc there is likely growth on the top of class
  out$ProgressionSSDPreRep=mean(colSums(UCPrimeGrowth,na.rm=T)[preRep]*SSD[preRep])
  out$ProgressionSSDRep=mean(colSums(UCPrimeGrowth,na.rm=T)[rep]*SSD[rep])

  #Extracting proxy to individual retrogressive growth rate
  UCPrimeShrinkage=UCPrime
  UCPrimeShrinkage[lower.tri(UCPrime, diag = T)]=NA
  out$RetrogressionSSDPreRep=mean(colSums(UCPrimeShrinkage,na.rm=T)[preRep]*SSD[preRep])
  out$RetrogressionSSDRep=mean(colSums(UCPrimeShrinkage,na.rm=T)[rep]*SSD[rep])

  return(out)
}



#Function to calculate vital rate level sensitivities and elasticities
vitalRatePerturbation <- function(matU, matF, matC=NULL,pert=0.001){

  matA=matU+matF+matC
  aDim=dim(matA)[1]
  fakeA=matA
  sensA=elasA=matrix(NA,aDim,aDim)
  lambda=Re(eigen(matA)$values[1])

  propU=matU/matA
  propU[is.nan(propU)]=0
  propProg=propRetrog=propU
  propProg[upper.tri(propU,diag=T)]=0
  propRetrog[lower.tri(propU,diag=T)]=0
  propStasis=matrix(Diagonal(aDim)*diag(propU),aDim,aDim)
  propF=matF/matA
  propF[is.nan(propF)]=0
  propC=matC/matA
  propC[is.nan(propC)]=0

  #for (i in 1:aDim){
  #  for (j in 1:aDim){
  #     fakeA=matA
  #     fakeA[i,j]=fakeA[i,j]+pert
  #     lambdaPert=eigen(fakeA)$values[1]
  #     sensA[i,j]=(lambda-lambdaPert)/(matA[i,j]-fakeA[i,j])
  #  }
  #}
  #sensA=Re(sensA)

  sensA=eigen.analysis(matA,zero=F)$sensitivities

  #Survival-independent A matrix
  uIndep=matrix(NA,aDim,aDim)
  u=colSums(matU)
  for (j in which(u>0)) uIndep[,j]=matA[,j]/u[j]
  sensSigmaA=uIndep*sensA

  #Little fix for semelparous species
  uPrime=u
  #uPrime[u==0]=0.001
  elasSigmaA=t(t(sensSigmaA)*uPrime)/lambda
  elasA=sensA*matA/lambda

  #Extracting survival vital rate
  uDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(u>0)) uDistrib[,j]=matU[,j]/u[j]
  #Extracting fecundity vital rates:
  f=colSums(matF)
  fDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(f>0)) fDistrib[,j]=matF[,j]/f[j]
  #Extracting clonality vital rates:
  c=colSums(matC)
  cDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(c>0)) cDistrib[,j]=matC[,j]/c[j]

  SuDistrib=sensA*uDistrib
  SfDistrib=sensA*fDistrib
  ScDistrib=sensA*cDistrib

  out = data.frame("SSurvival"=NA,"SGrowth"=NA,"SShrinkage"=NA,"SReproduction"=NA,"SClonality"=NA,
                   "ESurvival"=NA,"EGrowth"=NA,"EShrinkage"=NA,"EReproduction"=NA,"EClonality"=NA)

  #Still to be done
  out$SSurvival=sum(sensSigmaA,na.rm=T)
  out$SGrowth=sum(sensA*uDistrib*propProg,na.rm=T)
  out$SShrinkage=sum(sensA*uDistrib*propRetrog,na.rm=T)
  out$SReproduction=sum(sensA*fDistrib*propF,na.rm=T)
  out$SClonality=sum(sensA*cDistrib*propC,na.rm=T)

  EuDistrib=sensA*uDistrib*matrix(u,nrow=aDim,ncol=aDim,byrow=T)/lambda
  EfDistrib=sensA*fDistrib*matrix(f,nrow=aDim,ncol=aDim,byrow=T)/lambda
  EcDistrib=sensA*cDistrib*matrix(c,nrow=aDim,ncol=aDim,byrow=T)/lambda

  out$ESurvival=sum(elasSigmaA,na.rm=T)
  out$EGrowth=sum(EuDistrib*propProg,na.rm=T)
  out$EShrinkage=sum(EuDistrib*propRetrog,na.rm=T)
  out$EReproduction=sum(EfDistrib*propF,na.rm=T)
  out$EClonality=sum(EcDistrib*propC,na.rm=T)

  return(out)
}


#Function to calculate vital rate level sensitivities and elasticities as a function of pre-reproductive or reproductive (including post-reproductive) stages
vitalRatePerturbationStage <- function(matU, matF, matC=NULL, pert=0.001){

  matA=matU+matF+matC
  aDim=dim(matA)[1]

  #Determining prereproductive and reproductive stages
  firstRep=min(which(colSums(matF)>0))
  if (firstRep==1) {preRep=0} else {preRep=1:(firstRep-1)}
  rep=firstRep:aDim

  fakeA=matA
  sensA=elasA=matrix(NA,aDim,aDim)
  lambda=Re(eigen(matA)$values[1])

  propU=matU/matA
  propU[is.nan(propU)]=0
  propProg=propRetrog=propU
  propProg[upper.tri(propU,diag=T)]=0
  propRetrog[lower.tri(propU,diag=T)]=0
  propStasis=matrix(Diagonal(aDim)*diag(propU),aDim,aDim)
  propF=matF/matA
  propF[is.nan(propF)]=0
  propC=matC/matA
  propC[is.nan(propC)]=0

  #for (i in 1:aDim){
  #  for (j in 1:aDim){
  #     fakeA=matA
  #     fakeA[i,j]=fakeA[i,j]+pert
  #     lambdaPert=eigen(fakeA)$values[1]
  #     sensA[i,j]=(lambda-lambdaPert)/(matA[i,j]-fakeA[i,j])
  #  }
  #}
  #sensA=Re(sensA)

  sensA=eigen.analysis(matA,zero=F)$sensitivities

  #Survival-independent A matrix
  uIndep=matrix(NA,aDim,aDim)
  u=colSums(matU)
  for (j in which(u>0)) uIndep[,j]=matA[,j]/u[j]
  sensSigmaA=uIndep*sensA

  #Little fix for semelparous species
  uPrime=u
  #uPrime[u==0]=0.001
  elasSigmaA=t(t(sensSigmaA)*uPrime)/lambda
  elasA=sensA*matA/lambda

  #Extracting survival vital rate
  uDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(u>0)) uDistrib[,j]=matU[,j]/u[j]
  #Extracting fecundity vital rates:
  f=colSums(matF)
  fDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(f>0)) fDistrib[,j]=matF[,j]/f[j]
  #Extracting clonality vital rates:
  c=colSums(matC)
  cDistrib=matrix(0,ncol=aDim,nrow=aDim)
  for (j in which(c>0)) cDistrib[,j]=matC[,j]/c[j]

  SuDistrib=sensA*uDistrib
  SfDistrib=sensA*fDistrib
  ScDistrib=sensA*cDistrib

  out = data.frame("SSurvivalPreRep"=NA,"SGrowthPreRep"=NA,"SShrinkagePreRep"=NA,"SClonalityPreRep"=NA,
                   "SSurvivalRep"=NA,"SGrowthRep"=NA,"SShrinkageRep"=NA,"SReproductionRep"=NA,"SClonalityRep"=NA,
                   "ESurvivalPreRep"=NA,"EGrowthPreRep"=NA,"EShrinkagePreRep"=NA,"EClonalityPreRep"=NA,
                   "ESurvivalRep"=NA,"EGrowthRep"=NA,"EShrinkageRep"=NA,"EReproductionRep"=NA,"EClonalityRep"=NA)

  out$SSurvivalPreRep=sum(sensSigmaA[,preRep],na.rm=T)
  out$SGrowthPreRep=sum((sensA*uDistrib*propProg)[,preRep],na.rm=T)
  out$SShrinkagePreRep=sum((sensA*uDistrib*propRetrog)[,preRep],na.rm=T)
  out$SClonalityPreRep=sum((sensA*cDistrib*propC)[,preRep],na.rm=T)

  out$SSurvivalRep=sum(sensSigmaA[,rep],na.rm=T)
  out$SGrowthRep=sum((sensA*uDistrib*propProg)[,rep],na.rm=T)
  out$SShrinkageRep=sum((sensA*uDistrib*propRetrog)[,rep],na.rm=T)
  out$SReproductionRep=sum((sensA*fDistrib*propF)[,rep],na.rm=T)
  out$SClonalityRep=sum((sensA*cDistrib*propC)[,rep],na.rm=T)

  EuDistrib=sensA*uDistrib*matrix(u,nrow=aDim,ncol=aDim,byrow=T)/lambda
  EfDistrib=sensA*fDistrib*matrix(f,nrow=aDim,ncol=aDim,byrow=T)/lambda
  EcDistrib=sensA*cDistrib*matrix(c,nrow=aDim,ncol=aDim,byrow=T)/lambda

  out$ESurvivalPreRep=sum(elasSigmaA[,preRep],na.rm=T)
  out$EGrowthPreRep=sum((elasA*uDistrib*propProg)[,preRep],na.rm=T)
  out$EShrinkagePreRep=sum((elasA*uDistrib*propRetrog)[,preRep],na.rm=T)
  out$EClonalityPreRep=sum((elasA*cDistrib*propC)[,preRep],na.rm=T)

  out$ESurvivalRep=sum(elasSigmaA[,rep],na.rm=T)
  out$EGrowthRep=sum((EuDistrib*propProg)[,rep],na.rm=T)
  out$EShrinkageRep=sum((EuDistrib*propRetrog)[,rep],na.rm=T)
  out$EReproductionRep=sum((EfDistrib*propF)[,rep],na.rm=T)
  out$EClonalityRep=sum((EcDistrib*propC)[,rep],na.rm=T)

  return(out)
}



# #Upload COMPADRE and COMADRE

#Upload COMPADRE and COMADRE

source('R/subset_orchids.R')

orchids$metadata$unique_id <- paste(orchids$metadata$SpeciesAccepted,
                                    orchids$metadata$Journal,
                                    orchids$metadata$PublicationYear,
                                    orchids$metadata$MatrixDimension)

d <- orchids
long <- nrow(d$metadata)

###Loop to obtain demographic quantities
# long  <- nrow(compadre$metadata)[1]
output <- data.frame("SpeciesAuthor"=rep(NA,long),
                     "SpeciesAccepted"=rep(NA,long),
                     "Family"=rep(NA,long),
                     "Class"=rep(NA,long),
                     "Kingdom"=rep(NA,long),
                     "Authors"=rep(NA,long),
                     "Journal"=rep(NA,long),
                     "YearPublication"=rep(NA,long),
                     "Country"=rep(NA,long),
                     "Continent"=rep(NA,long),
                     "Ecoregion"=rep(NA,long),
                     "MatrixDimension"=rep(NA,long),

                     "Population"=rep(NA,long),
                     "StartYear"=rep(NA,long),
                     "StartMonth"=rep(NA,long),
                     "StartSeason"=rep(NA,long),
                     "EndYear"=rep(NA,long),
                     "EndMonth"=rep(NA,long),
                     "EndSeason"=rep(NA,long),
                     "Treatment"=rep(NA,long),
                     "MatrixComposite"=rep(NA,long),
                     "Lat"=rep(NA,long),
                     "Lon"=rep(NA,long),

                     "GenT"=rep(NA,long),
                     "H"=rep(NA,long),
                     "Lmean"=rep(NA,long),
                     "Lmax"=rep(NA,long),
                     "pRep"=rep(NA,long),
                     "La"=rep(NA,long),
                     "La.mean"=rep(NA,long),
                     "La.omega"=rep(NA,long),

                     "SurvSSD"=rep(NA,long),
                     "GrowSSD"=rep(NA,long),
                     "ShriSSD"=rep(NA,long),
                     "RepSSD"=rep(NA,long),
                     "CloSSD"=rep(NA,long),

                     "SurvSSDPreRep"=rep(NA,long),
                     "GrowSSDPreRep"=rep(NA,long),
                     "ShriSSDPreRep"=rep(NA,long),
                     "CloSSDPreRep"=rep(NA,long),

                     "SurvSSDRep"=rep(NA,long),
                     "GrowSSDRep"=rep(NA,long),
                     "ShriSSDRep"=rep(NA,long),
                     "RepSSDRep"=rep(NA,long),
                     "CloSSDRep"=rep(NA,long),

                     "Esurv"=rep(NA,long),
                     "Egrow"=rep(NA,long),
                     "Eshri"=rep(NA,long),
                     "Erep"=rep(NA,long),
                     "Eclo"=rep(NA,long),
                     "Ssurv"=rep(NA,long),
                     "Sgrow"=rep(NA,long),
                     "Sshri"=rep(NA,long),
                     "Srep"=rep(NA,long),
                     "Sclo"=rep(NA,long),

                     "EsurvPreRep"=rep(NA,long),
                     "EgrowPreRep"=rep(NA,long),
                     "EshriPreRep"=rep(NA,long),
                     "EcloPreRep"=rep(NA,long),
                     "SsurvPreRep"=rep(NA,long),
                     "SgrowPreRep"=rep(NA,long),
                     "SshriPreRep"=rep(NA,long),
                     "ScloPreRep"=rep(NA,long),

                     "EsurvRep"=rep(NA,long),
                     "EgrowRep"=rep(NA,long),
                     "EshriRep"=rep(NA,long),
                     "ErepRep"=rep(NA,long),
                     "EcloRep"=rep(NA,long),
                     "SsurvRep"=rep(NA,long),
                     "SgrowRep"=rep(NA,long),
                     "SshriRep"=rep(NA,long),
                     "SrepRep"=rep(NA,long),
                     "ScloRep"=rep(NA,long),

                     "Lambda"=rep(NA,long),
                     "LambdaAway"=rep(NA,long),
                     "Rho"=rep(NA,long),
                     "Pi"=rep(NA,long),
                     "Reactivity"=rep(NA,long),
                     "FirstStepAtt"=rep(NA,long),
                     "MaxAmp"=rep(NA,long),
                     "MaxAtt"=rep(NA,long),
                     "InertiaUp"=rep(NA,long),
                     "InertiaLow"=rep(NA,long),
                     "InertiaTotal"=rep(NA,long),
                     "KreissUp"=rep(NA,long),
                     "KreissLow"=rep(NA,long),
                     "KreissTotal"=rep(NA,long))

count <- 0

# sink(file = "Logs/derive_demo_log.txt")
# sink(file = "Logs/derive_demo_log.txt", type = 'message')

#for (i in 1:long){
for (i in 1:100){


  count <- count+1

  output[i, c(
    "SpeciesAuthor",
    "SpeciesAccepted",
    "Family",
    "Class",
    "Kingdom",
    "Authors",
    "Journal",
    "YearPublication",
    "Country",
    "Continent",
    "Ecoregion",
    "Lat",
    "Lon",
    "StartYear",
    "StartSeason",
    "StartMonth",
    "EndYear",
    "EndSeason",
    "EndMonth",
    "Population",
    "Treatment",
    "MatrixComposite"
  )] <-
    unlist(lapply(d$metadata[count, c(
      "SpeciesAuthor",
      "SpeciesAccepted",
      "Family",
      "Class",
      "Kingdom",
      "Authors",
      "Journal",
      "YearPublication",
      "Country",
      "Continent",
      "Ecoregion",
      "Lat",
      "Lon",
      "MatrixStartYear",
      "MatrixStartSeason",
      "MatrixStartMonth",
      "MatrixEndYear",
      "MatrixEndSeason",
      "MatrixEndMonth",
      "MatrixPopulation",
      "MatrixTreatment",
      "MatrixComposite"
    )], as.character))

  #The calculations here employed define the beginning of life when an individual become established. Thus, we do not consider transitions from the "prop" stages
  lifeStages <- d$matrixClass[[count]][1]
  notProp <- min(which(lifeStages != "prop"))
  matU <- d$mat[[count]]$matU
  matF <- d$mat[[count]]$matF
  matC <- d$mat[[count]]$matC
  matA <- matU+matF+matC

  output$MatrixDimension[i]=matDim=dim(matU)[1]

  try({
    output[i,c("Lmean","Lmax","pRep","La","La.mean","La.omega")] <- lifeTimeRepEvents(matU,matF,notProp)
  })

  try({
    QSD <- qsdConvergence(matU, notProp)

    lifeTable <- makeLifeTable(matU,
                               matF,
                               matC,
                               notProp,
                               1000)[1:QSD[1],]
    lx <- lifeTable$lx
    mx <- lifeTable$mx
  })


  #Keyfitz' entropy
  try({
    output$H[i] <- as.numeric(
      -t(lx[!is.na(lx)]) %*% log(lx[!is.na(lx)]) / sum(lx[!is.na(lx)])
      )
  })

  #lambda
  try({
    output$Lambda[i] <- Lambda <- eigen.analysis(matA)$lambda1
    output$LambdaAway[i] <- abs(Lambda-1)
  })

  #Demetrius entropy (H):
  try({
    if (sum(matF)>0) {
      r=log(Lambda)
      limGo <- min(length(mx[!is.na(mx)]), length(lx[!is.na(lx)]))
      lxmx <- lx[1:limGo] * mx[1:limGo]
      lxmx <- lx[!is.na(lx)] * mx[!is.na(mx)]
      for (r in 1:limGo) {
        if (lxmx[r] == 0) {
          lxmx[r] <- 1
        }
      }
      loglxmx <- log(lxmx)
      loglxmx[which(lxmx == 0)] <- NA
      # exp(r*c(1:length(lxmx))) %*% (lxmx)
      output[i,"S"] <- abs(sum(lxmx*loglxmx) / sum(lxmx))
    } else {
      output[i,"S"] <- NA
    }
  })

  #Net reproductive rate (R0; Caswell 2001, p 126)
  try({
    N <- solve(diag(nrow = dim(matU)[1]) - matU)
    R <- (matF + matC) %*% N
    R0 <- Re(eigen(R)$values[1])
    output$R0[i] <- R0
    if (is.infinite(R0) | sum(matF, matC) == 0) {
      output$R0[i] <- NA
    }
  })

  #Generation time (T; Caswell 2001, p 129)
  try({
    output$GenT[i] <- GenT <- abs(log(R0)/log(Lambda))
    if (is.infinite(GenT) | sum(matF, matC) == 0) {
      output$GenT[i] <- NA
    }
  })

  rm(Lambda, r,
     lx,
     mx,
     lifeTable,
     QSD,
     matDim,
     matA,
     matC,
     matU,
     matF,
     lifeStages,
     notProp,
     GenT, R0, N,
     limGo, loglxmx, lxmx)

}

# sink()
# sink(type = 'message')
# close(file = 'Logs/derive_demo_log.txt')

write.csv(output,
          file = 'Data/Csv/orchid_demog_output.csv',
          row.names = FALSE)

# write.csv(output,"C:/cloud/Dropbox/sApropos/mentees/intern/demog_orchids.csv",
#           row.names=F)


output$GenT %>% log %>% hist
output$R0 %>% log %>% hist

