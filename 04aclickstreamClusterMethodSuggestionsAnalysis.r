#This script illustrates some of the analyses that can be done with this method once the similarity network has been produced and a clustering has been found.  
#If you use code from this script, please cite:
#Authors (2019). Double network-based method for identifying behavioral patterns in clickstream data. MethodX. DOI: XXX

#First load 01clickstreamClusterMethodSession.r
#Then load 02clickstreamClusterMethodSessionNetworks.r
#Then load 03clickstreamClusterMethodSessionSimilarityNetwork.r

####DESCRIPTIVE STATISTICS#####
med<-vector() #vector for median values of network measures
mea<-vector() #vector for mean values of network measures
sd<-vector() #vector for standard deviations of network measures
min<-vector() #vector for min values of network measures
max<-vector() #vector for max values of network measures

for (i in 2:26){#for all sessions
  med[i-1]<-median(description[,i],na.rm=T)
  mea[i-1]<-mean(description[,i],na.rm = T)
  sd[i-1]<-sd(description[,i],na.rm = T)
  min[i-1]<-min(description[,i],na.rm=T)
  max[i-1]<-max(description[,i],na.rm=T)
}

medps<-vector() #same as above but only for problem-solving sessions
meaps<-vector()
sdps<-vector()
minps<-vector()
maxps<-vector()

for (i in 2:26){#for problem-solving sessions
  medps[i-1]<-median(description[ps,i])
  meaps[i-1]<-mean(description[ps,i],na.rm = T)
  sdps[i-1]<-sd(description[ps,i],na.rm = T)
  minps[i-1]<-min(description[ps,i])
  maxps[i-1]<-max(description[ps,i])
}

#######################################
########ANALYSES OF CLUSTERS###########
######################################

#Matrix of membership
memMat<-matrix(0,ncol=12,nrow=231)
for (i in 1:length(overlap[,1])){
  memMat[overlap$node[i],overlap$group[i]]<-overlap$flow[i]#for each node for each cluster, list the flow as calculated by Infomap
}
memMat<-memMat/rowSums(memMat) #normalized membership matrix: replace raw flow value with fractional flow value.
memMat[is.na(memMat)]<-0 # replace NAs with 0s
memMat[17,11]<-1
memMat[147,12]<-1

Fmemb<-colSums(memMat) #To be used in mean and standard deviaton calculations
#Network measures for fractional membership. 
#For all 23 network measures do the following:
WmeanN<-vector()
WconfN<-vector()
for (i in 1:10){
  wN<-memMat[memMat[,i]!=0,i]*description$N[overlap$session.no[overlap$group==i]]
  WmeanN[i]<-sum(wN)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$N[overlap$session.no[overlap$group==i]]-WmeanN[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfN[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanL<-vector()
WconfL<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$L[overlap$session.no[overlap$group==i]]
  WmeanL[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$L[overlap$session.no[overlap$group==i]]-WmeanL[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfL[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanRho<-vector()
WconfRho<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$d[overlap$session.no[overlap$group==i]]
  WmeanRho[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$d[overlap$session.no[overlap$group==i]]-WmeanRho[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfRho[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanDiam<-vector()
WconfDiam<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$diam[overlap$session.no[overlap$group==i]]
  WmeanDiam[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$diam[overlap$session.no[overlap$group==i]]-WmeanDiam[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfDiam[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanApl<-vector()
WconfApl<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$apl[overlap$session.no[overlap$group==i]]
  WmeanApl[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$apl[overlap$session.no[overlap$group==i]]-WmeanApl[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfApl[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanMut<-vector()
WconfMut<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$mu[overlap$session.no[overlap$group==i]]
  WmeanMut[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$mu[overlap$session.no[overlap$group==i]]-WmeanMut[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfMut[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanTE<-vector()
WconfTE<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$te[overlap$session.no[overlap$group==i]]
  WmeanTE[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$te[overlap$session.no[overlap$group==i]]-WmeanTE[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfTE[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanSI<-vector()
WconfSI<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$si[overlap$session.no[overlap$group==i]]
  WmeanSI[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$si[overlap$session.no[overlap$group==i]]-WmeanSI[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfSI[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanClu<-vector()
WconfClu<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$cl[overlap$session.no[overlap$group==i]]
  WmeanClu[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$cl[overlap$session.no[overlap$group==i]]-WmeanClu[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfClu[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanEnt<-vector()
WconfEnt<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$S[overlap$session.no[overlap$group==i]]
  WmeanEnt[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$S[overlap$session.no[overlap$group==i]]-WmeanEnt[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfEnt[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanX1<-vector()
WconfX1<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X1[overlap$session.no[overlap$group==i]]
  WmeanX1[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X1[overlap$session.no[overlap$group==i]]-WmeanX1[i])^2
  WX1D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX1[i]<-1.96*WX1D/sqrt(sum(memMat[,i]))
}

WmeanX2<-vector()
WconfX2<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X2[overlap$session.no[overlap$group==i]]
  WmeanX2[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X2[overlap$session.no[overlap$group==i]]-WmeanX2[i])^2
  WX2D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX2[i]<-1.96*WX2D/sqrt(sum(memMat[,i]))
}

WmeanX3<-vector()
WconfX3<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X3[overlap$session.no[overlap$group==i]]
  WmeanX3[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X3[overlap$session.no[overlap$group==i]]-WmeanX3[i])^2
  WX3D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX3[i]<-1.96*WX3D/sqrt(sum(memMat[,i]))
}

WmeanX4<-vector()
WconfX4<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X4[overlap$session.no[overlap$group==i]]
  WmeanX4[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X4[overlap$session.no[overlap$group==i]]-WmeanX4[i])^2
  WX4D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX4[i]<-1.96*WX4D/sqrt(sum(memMat[,i]))
}

WmeanX5<-vector()
WconfX5<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X5[overlap$session.no[overlap$group==i]]
  WmeanX5[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X5[overlap$session.no[overlap$group==i]]-WmeanX5[i])^2
  WX5D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX5[i]<-1.96*WX5D/sqrt(sum(memMat[,i]))
}

WmeanX6<-vector()
WconfX6<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X6[overlap$session.no[overlap$group==i]]
  WmeanX6[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X6[overlap$session.no[overlap$group==i]]-WmeanX6[i])^2
  WX6D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX6[i]<-1.96*WX6D/sqrt(sum(memMat[,i]))
}

WmeanX7<-vector()
WconfX7<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X7[overlap$session.no[overlap$group==i]]
  WmeanX7[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X7[overlap$session.no[overlap$group==i]]-WmeanX7[i])^2
  WX7D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX7[i]<-1.96*WX7D/sqrt(sum(memMat[,i]))
}

WmeanX8<-vector()
WconfX8<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X8[overlap$session.no[overlap$group==i]]
  WmeanX8[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X8[overlap$session.no[overlap$group==i]]-WmeanX8[i])^2
  WX8D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX8[i]<-1.96*WX8D/sqrt(sum(memMat[,i]))
}



WmeanX9<-vector()
WconfX9<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X9[overlap$session.no[overlap$group==i]]
  WmeanX9[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X9[overlap$session.no[overlap$group==i]]-WmeanX9[i])^2
  WX9D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX9[i]<-1.96*WX9D/sqrt(sum(memMat[,i]))
}


WmeanX10<-vector()
WconfX10<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X10[overlap$session.no[overlap$group==i]]
  WmeanX10[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X10[overlap$session.no[overlap$group==i]]-WmeanX10[i])^2
  WX10D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX10[i]<-1.96*WX10D/sqrt(sum(memMat[,i]))
}


WmeanX11<-vector()
WconfX11<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X11[overlap$session.no[overlap$group==i]]
  WmeanX11[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X11[overlap$session.no[overlap$group==i]]-WmeanX11[i])^2
  WX11D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX11[i]<-1.96*WX11D/sqrt(sum(memMat[,i]))
}


WmeanX12<-vector()
WconfX12<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X12[overlap$session.no[overlap$group==i]]
  WmeanX12[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X12[overlap$session.no[overlap$group==i]]-WmeanX12[i])^2
  WX12D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX12[i]<-1.96*WX12D/sqrt(sum(memMat[,i]))
}


WmeanX13<-vector()
WconfX13<-vector()
for (i in 1:10){
  wL<-memMat[memMat[,i]!=0,i]*description$X13[overlap$session.no[overlap$group==i]]
  WmeanX13[i]<-sum(wL)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(description$X13[overlap$session.no[overlap$group==i]]-WmeanX13[i])^2
  WX13D<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfX13[i]<-1.96*WX13D/sqrt(sum(memMat[,i]))
}
#Network measures considering full memberships only
meanNf<-vector()
confNf<-vector()
for (i in 1:10){
  meanNf[i]<-mean((memMat[,i]*description$N[ps])[memMat[,i]==1])
  confNf[i]<-1.96*sd((memMat[,i]*description$N[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$N[ps])[memMat[,i]==1]))
}

meanLf<-vector()
confLf<-vector()
for (i in 1:10){
  meanLf[i]<-mean((memMat[,i]*description$L[ps])[memMat[,i]==1])
  confLf[i]<-1.96*sd((memMat[,i]*description$L[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$L[ps])[memMat[,i]==1]))
}

meanRhof<-vector()
confRhof<-vector()
for (i in 1:10){
  meanRhof[i]<-mean((memMat[,i]*description$d[ps])[memMat[,i]==1])
  confRhof[i]<-1.96*sd((memMat[,i]*description$d[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$d[ps])[memMat[,i]==1]))
}

meanDiamf<-vector()
confDiamf<-vector()
for (i in 1:10){
  meanDiamf[i]<-mean((memMat[,i]*description$diam[ps])[memMat[,i]==1])
  confDiamf[i]<-1.96*sd((memMat[,i]*description$diam[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$diam[ps])[memMat[,i]==1]))
}

meanAplf<-vector()
confAplf<-vector()
for (i in 1:10){
  meanAplf[i]<-mean((memMat[,i]*description$apl[ps])[memMat[,i]==1])
  confAplf[i]<-1.96*sd((memMat[,i]*description$apl[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$apl[ps])[memMat[,i]==1]))
}

meanMutf<-vector()
confMutf<-vector()
for (i in 1:10){
  meanMutf[i]<-mean((memMat[,i]*description$mu[ps])[memMat[,i]==1])
  confMutf[i]<-1.96*sd((memMat[,i]*description$mu[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$mu[ps])[memMat[,i]==1]))
}

meanTEf<-vector()
confTEf<-vector()
for (i in 1:10){
  meanTEf[i]<-mean((memMat[,i]*description$te[ps])[memMat[,i]==1])
  confTEf[i]<-1.96*sd((memMat[,i]*description$te[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$te[ps])[memMat[,i]==1]))
}

meanSIf<-vector()
confSIf<-vector()
for (i in 1:10){
  meanSIf[i]<-mean((memMat[,i]*description$si[ps])[memMat[,i]==1])
  confSIf[i]<-1.96*sd((memMat[,i]*description$si[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$si[ps])[memMat[,i]==1]))
}

meanCluf<-vector()
confCluf<-vector()
for (i in 1:10){
  meanCluf[i]<-mean((memMat[,i]*description$cl[ps])[memMat[,i]==1])
  confCluf[i]<-1.96*sd((memMat[,i]*description$cl[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$cl[ps])[memMat[,i]==1]))
}

meanEntf<-vector()
confEntf<-vector()
for (i in 1:10){
  meanEntf[i]<-mean((memMat[,i]*description$S[ps])[memMat[,i]==1])
  confEntf[i]<-1.96*sd((memMat[,i]*description$S[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$S[ps])[memMat[,i]==1]))
}

meanX1f<-vector()
confX1f<-vector()
for (i in 1:10){
  meanX1f[i]<-mean((memMat[,i]*description$X1[ps])[memMat[,i]==1])
  confX1f[i]<-1.96*sd((memMat[,i]*description$X1[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X1[ps])[memMat[,i]==1]))
}

meanX2f<-vector()
confX2f<-vector()
for (i in 1:10){
  meanX2f[i]<-mean((memMat[,i]*description$X2[ps])[memMat[,i]==1])
  confX2f[i]<-1.96*sd((memMat[,i]*description$X2[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X2[ps])[memMat[,i]==1]))
}

meanX3f<-vector()
confX3f<-vector()
for (i in 1:10){
  meanX3f[i]<-mean((memMat[,i]*description$X3[ps])[memMat[,i]==1])
  confX3f[i]<-1.96*sd((memMat[,i]*description$X3[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X3[ps])[memMat[,i]==1]))
}

meanX4f<-vector()
confX4f<-vector()
for (i in 1:10){
  meanX4f[i]<-mean((memMat[,i]*description$X4[ps])[memMat[,i]==1])
  confX4f[i]<-1.96*sd((memMat[,i]*description$X4[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X4[ps])[memMat[,i]==1]))
}

meanX5f<-vector()
confX5f<-vector()
for (i in 1:10){
  meanX5f[i]<-mean((memMat[,i]*description$X5[ps])[memMat[,i]==1])
  confX5f[i]<-1.96*sd((memMat[,i]*description$X5[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X5[ps])[memMat[,i]==1]))
}

meanX6f<-vector()
confX6f<-vector()
for (i in 1:10){
  meanX6f[i]<-mean((memMat[,i]*description$X6[ps])[memMat[,i]==1])
  confX6f[i]<-1.96*sd((memMat[,i]*description$X6[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X6[ps])[memMat[,i]==1]))
}

meanX7f<-vector()
confX7f<-vector()
for (i in 1:10){
  meanX7f[i]<-mean((memMat[,i]*description$X7[ps])[memMat[,i]==1])
  confX7f[i]<-1.96*sd((memMat[,i]*description$X7[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X7[ps])[memMat[,i]==1]))
}

meanX8f<-vector()
confX8f<-vector()
for (i in 1:10){
  meanX8f[i]<-mean((memMat[,i]*description$X8[ps])[memMat[,i]==1])
  confX8f[i]<-1.96*sd((memMat[,i]*description$X8[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X8[ps])[memMat[,i]==1]))
}

meanX9f<-vector()
confX9f<-vector()
for (i in 1:10){
  meanX9f[i]<-mean((memMat[,i]*description$X9[ps])[memMat[,i]==1])
  confX9f[i]<-1.96*sd((memMat[,i]*description$X9[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X9[ps])[memMat[,i]==1]))
}

meanX10f<-vector()
confX10f<-vector()
for (i in 1:10){
  meanX10f[i]<-mean((memMat[,i]*description$X10[ps])[memMat[,i]==1])
  confX10f[i]<-1.96*sd((memMat[,i]*description$X10[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X10[ps])[memMat[,i]==1]))
}

meanX11f<-vector()
confX11f<-vector()
for (i in 1:10){
  meanX11f[i]<-mean((memMat[,i]*description$X11[ps])[memMat[,i]==1])
  confX11f[i]<-1.96*sd((memMat[,i]*description$X11[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X11[ps])[memMat[,i]==1]))
}

meanX12f<-vector()
confX12f<-vector()
for (i in 1:10){
  meanX12f[i]<-mean((memMat[,i]*description$X12[ps])[memMat[,i]==1])
  confX12f[i]<-1.96*sd((memMat[,i]*description$X12[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X12[ps])[memMat[,i]==1]))
}

meanX13f<-vector()
confX13f<-vector()
for (i in 1:10){
  meanX13f[i]<-mean((memMat[,i]*description$X13[ps])[memMat[,i]==1])
  confX13f[i]<-1.96*sd((memMat[,i]*description$X13[ps])[memMat[,i]==1])/sqrt(length((memMat[,i]*description$X13[ps])[memMat[,i]==1]))
}
#Cluster Scores
###Considering fractional membership#############
WmeanPCA1<-vector()
WconfPCA1<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,1]
  WmeanPCA1[i]<-sum(ws)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(ws[ws!=0]-WmeanPCA1[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfPCA1[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanPCA2<-vector()
WconfPCA2<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,2]
  WmeanPCA2[i]<-sum(ws)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(ws[ws!=0]-WmeanPCA2[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfPCA2[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanPCA3<-vector()
WconfPCA3<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,3]
  WmeanPCA3[i]<-sum(ws)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(ws[ws!=0]-WmeanPCA3[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfPCA3[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanPCA4<-vector()
WconfPCA4<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,4]
  WmeanPCA4[i]<-sum(ws)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(ws[ws!=0]-WmeanPCA4[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfPCA4[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}

WmeanPCA5<-vector()
WconfPCA5<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,5]
  WmeanPCA5[i]<-sum(ws)/sum(Fmemb[i])
  wD<-memMat[memMat[,i]!=0,i]*(ws[ws!=0]-WmeanPCA5[i])^2
  WSD<-sqrt(sum(wD)/(sum(Fmemb[i])-1))
  WconfPCA5[i]<-1.96*WSD/sqrt(sum(memMat[,i]))
}


WmeanPCA<-data.frame(WmeanPCA1,WmeanPCA2,WmeanPCA3,WmeanPCA4,WmeanPCA5)
WconfPCA<-data.frame(WconfPCA1,WconfPCA2,WconfPCA3,WconfPCA4,WconfPCA5)

####Scores full membership only#############
WmeanPCA1f<-vector()
WconfPCA1f<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,1]
  WmeanPCA1f[i]<-mean(ws[memMat[,i]==1])
  WconfPCA1f[i]<-1.96*(sd(ws[memMat[,i]==1])/sqrt(length(ws[memMat[,i]==1])))
}

WmeanPCA2f<-vector()
WconfPCA2f<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,2]
  WmeanPCA2f[i]<-mean(ws[memMat[,i]==1])
  WconfPCA2f[i]<-1.96*(sd(ws[memMat[,i]==1])/sqrt(length(ws[memMat[,i]==1])))
}

WmeanPCA3f<-vector()
WconfPCA3f<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,3]
  WmeanPCA3f[i]<-mean(ws[memMat[,i]==1])
  WconfPCA3f[i]<-1.96*(sd(ws[memMat[,i]==1])/sqrt(length(ws[memMat[,i]==1])))
}

WmeanPCA4f<-vector()
WconfPCA4f<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,4]
  WmeanPCA4f[i]<-mean(ws[memMat[,i]==1])
  WconfPCA4f[i]<-1.96*(sd(ws[memMat[,i]==1])/sqrt(length(ws[memMat[,i]==1])))
}

WmeanPCA5f<-vector()
WconfPCA5f<-vector()
for (i in 1:10){
  ws<-memMat[,i]*pca.net$scores[ps,5]
  WmeanPCA5f[i]<-mean(ws[memMat[,i]==1])
  WconfPCA5f[i]<-1.96*(sd(ws[memMat[,i]==1])/sqrt(length(ws[memMat[,i]==1])))
}

WmeanPCAf<-data.frame(WmeanPCA1f,WmeanPCA2f,WmeanPCA3f,WmeanPCA4f,WmeanPCA5f)
WconfPCAf<-data.frame(WconfPCA1f,WconfPCA2f,WconfPCA3f,WconfPCA4f,WconfPCA5f)

###FUNCTION TO PLOT SCORES####
ppp<-function(avg,sdev){
  a<-min(avg-sdev)
  b<-max(avg+sdev)
  x<-1:length(avg)
  plot(x, avg, ylim=range(c(a, b)),  pch=19, xlab="Cluster", ylab="Mean +/- 95% conf. int."
  )
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  
}

par(mfrow=c(5,1))
ppp(WmeanPCA[,1],WconfPCA[,1])
ppp(WmeanPCA[,2],WconfPCA[,2])
ppp(WmeanPCA[,3],WconfPCA[,3])
ppp(WmeanPCA[,4],WconfPCA[,4])
ppp(WmeanPCA[,5],WconfPCA[,5])

par(mfrow=c(5,1))
ppp(WmeanPCAf[,1],WconfPCAf[,1])
ppp(WmeanPCAf[,2],WconfPCAf[,2])
ppp(WmeanPCAf[,3],WconfPCAf[,3])
ppp(WmeanPCAf[,4],WconfPCAf[,4])
ppp(WmeanPCAf[,5],WconfPCAf[,5])

######ANALYSIS OF TIME###########
###Descriptive statistics######
weeks<-vector()
for (i in 1:length(bb)){
  weeks[i]<-networkArray[[i]]$week
  
}


year<-vector()
for (i in 1:length(bb)){
  year[i]<-networkArray[[i]]$year
  
}

hour<-vector()
for (i in 1:length(bb)){
  hour[i]<-as.character(networkArray[[i]]$hour)
  
}

day<-vector()
for (i in 1:length(bb)){
  day[i]<-as.character(networkArray[[i]]$day)
  
}

par(mfrow=c(2,2))
plot(table(year[ps]),main="By year")
plot(table(weeks[ps]),main="By week")
plot(sort(table(day[ps])),main="By day")
plot(sort(table(hour[ps])),main="By hour")

par(mfrow=c(2,2))
plot(table(year),main="By year")
plot(table(weeks),main="By week")
plot(sort(table(day)),main="By day")
plot(sort(table(hour)),main="By hour")


###Cluster duration distributions#####
##All nodes that have flowin a cluster
par(mfrow=c(5,2))
plot(density(aa[overlap$session.no[overlap$group==1],3]), main="Cluster 1")
plot(density(aa[overlap$session.no[overlap$group==2],3]), main="Cluster 2")
plot(density(aa[overlap$session.no[overlap$group==3],3]), main="Cluster 3")
plot(density(aa[overlap$session.no[overlap$group==4],3]), main="Cluster 4")
plot(density(aa[overlap$session.no[overlap$group==5],3]), main="Cluster 5")
plot(density(aa[overlap$session.no[overlap$group==6],3]), main="Cluster 6")
plot(density(aa[overlap$session.no[overlap$group==7],3]), main="Cluster 7")
plot(density(aa[overlap$session.no[overlap$group==8],3]), main="Cluster 8")
plot(density(aa[overlap$session.no[overlap$group==9],3]), main="Cluster 9")
plot(density(aa[overlap$session.no[overlap$group==10],3]), main="Cluster 10")

##Nodes with largest flow in cluster (hard partitioning)
Amem<-vector()
for(i in 1:231){
  Amem[i]<-which.max(memMat[i,]) #Find the cluster in which a node has maximum flow
  
}
durProb<-aa[ps,3]
par(mfrow=c(5,2))
plot(density(durProb[Amem==1]), main="Cluster 1")
plot(density(durProb[Amem==2]), main="Cluster 2")
plot(density(durProb[Amem==3]), main="Cluster 3")
plot(density(durProb[Amem==4]), main="Cluster 4")
plot(density(durProb[Amem==5]), main="Cluster 5")
plot(density(durProb[Amem==6]), main="Cluster 6")
plot(density(durProb[Amem==7]), main="Cluster 7")
plot(density(durProb[Amem==8]), main="Cluster 8")
plot(density(durProb[Amem==9]), main="Cluster 9")
plot(density(durProb[Amem==10]), main="Cluster 10")
dev.off()
#####SEGREGATION CALCULATIONS
##SEGREGATION###
##since segregation is defined on unique memberships, we assign sessions to the groups within which they have the largest flow. 

Amem<-vector()
for(i in 1:231){
  Amem[i]<-which.max(memMat[i,]) #Find the cluster in which a node has maximum flow
  
}
source("segregationMeasure.r")
#Depending on the speed of your computer set N_it to an appropriate number Here, we chose 1000. 
timeVar<-c("Hour","Day","Week", "Year", "Tc 1h", "TC 1.5h", "TC 2h", "TC 2.5h", "TC 3h")
Z<-vector()

resampleX(Amem,hour[ps],1,1000)
Z[1]<-resampleX(Amem,hour[ps],2,1000)

resampleX(Amem,day[ps],1,1000)
Z[2]<-resampleX(Amem,day[ps],2,1000)

resampleX(Amem,weeks[ps],1,1000)
Z[3]<-resampleX(Amem,weeks[ps],2,1000)
weeks[ps][Amem==1]# Seems like one cluster has an over representations of weeks

resampleX(Amem,year[ps],1,1000)
Z[4]<-resampleX(Amem,year[ps],2,1000)

resampleX(Amem,timeClass1,1,1000)
Z[5]<-resampleX(Amem,timeClass1,2,1000)

resampleX(Amem,timeClass15,1,1000)
Z[6]<-resampleX(Amem,timeClass15,2,1000)

resampleX(Amem,timeClass2,1,1000)
Z[7]<-resampleX(Amem,timeClass2,2,1000)

resampleX(Amem,timeClass25,1,1000)
Z[8]<-resampleX(Amem,timeClass25,2,1000)

resampleX(Amem,timeClass3,1,1000)
Z[9]<-resampleX(Amem,timeClass3,2,1000)


x<-as.table(Z)
names(x)<-timeVar
barplot(x, ylim=c(-2,5), space = 2,border = NA, ylab="Z-score", xlab="Time variable")
abline(h=0)
abline(h=-2)
rect(0,-1.96,30,1.96, col= rgb(166/255,190/255,204/255,alpha=0.5),border=NA)

####CALCULATIONS OF SHOW/HIDE mu-MEASURE####
###Functions to calculate mu and mean + CI of mus.
calcMu<-function(g){
  N_s<-sum(strength(g,V(g)$showhide=="show",mode="in"))
  N_h<-sum(strength(g,V(g)$showhide=="hide",mode="in"))
  mu<-(N_s-N_h)/(N_s+N_h)
  res<-c(N_s,N_h,mu)
  return(res)
  
}



sdMu<-function(muX,memMat,i){
  x<-data.frame(muX[memMat[,i]!=0],memMat[memMat[,i]!=0,i])
  x<-x[complete.cases(x),]
  meanMu<-sum(x[,1],na.rm=T)/sum(x[!is.na(x[,1]),2])
  deviations<-x[,1]-meanMu
  nominator<-sum(x[,2]*deviations^2)
  V<-sum(x[,2])
  standardDev<-sqrt(nominator/V)
  CI<-1.96*standardDev/sqrt(V)
  res<-data.frame(meanMu,CI)
  return(res)
}

#Calculate mus
mus<-matrix(data=NA,ncol=3,nrow=length(ps))
colnames(mus)<-c("N_show","N_hide", "mu")
for(i in 1:length(ps)){
  mus[i,]<-calcMu(networkArray[[ps[i]]])
  
}
mu1<-mus[,3]*memMat[,1]
mu2<-mus[,3]*memMat[,2]
mu3<-mus[,3]*memMat[,3]
mu4<-mus[,3]*memMat[,4]
mu5<-mus[,3]*memMat[,5]
mu6<-mus[,3]*memMat[,6]
mu7<-mus[,3]*memMat[,7]
mu8<-mus[,3]*memMat[,8]
mu9<-mus[,3]*memMat[,9]
mu10<-mus[,3]*memMat[,10]



#Calculate mean and CI of mus per cluster. 
meanConfMu_n<-matrix(0,ncol=2,nrow=10)
colnames(meanConfMu_n)<-c("meanMu","confIntMu")
meanConfMu_n[1,]<-as.matrix(sdMu(mu1,memMat,1))
meanConfMu_n[2,]<-as.matrix(sdMu(mu2,memMat,2))
meanConfMu_n[3,]<-as.matrix(sdMu(mu3,memMat,3))
meanConfMu_n[4,]<-as.matrix(sdMu(mu4,memMat,4))
meanConfMu_n[5,]<-as.matrix(sdMu(mu5,memMat,5))
meanConfMu_n[6,]<-as.matrix(sdMu(mu6,memMat,6))
meanConfMu_n[7,]<-as.matrix(sdMu(mu7,memMat,7))
meanConfMu_n[8,]<-as.matrix(sdMu(mu8,memMat,8))
meanConfMu_n[9,]<-as.matrix(sdMu(mu9,memMat,9))
meanConfMu_n[10,]<-as.matrix(sdMu(mu10,memMat,10))

#Plot to see that SDs are large
x<-c(1:10)
plot(x,meanConfMu_n[,1],ylim=range(c(meanConfMu_n[,1]-meanConfMu_n[,2], meanConfMu_n[,1]+meanConfMu_n[,2])),
     pch=19,  ylab="Mean mu +/- SD", xlab = "Cluster"
)

arrows(x, meanConfMu_n[,1]-meanConfMu_n[,2], x, meanConfMu_n[,1]+meanConfMu_n[,2], length=0.05, angle=90, code=3)


# hack: we draw arrows but with very special "arrowheads"
arrows(z, meanMux-confMux, z, meanMux+confMux, length=0.05, angle=90, code=3)

###########################
#x1-x10

x1<-data.frame(mu1[memMat[,1]!=0],memMat[memMat[,1]!=0,1])
x2<-data.frame(mu2[memMat[,2]!=0],memMat[memMat[,2]!=0,2])
x3<-data.frame(mu3[memMat[,3]!=0],memMat[memMat[,3]!=0,3])
x4<-data.frame(mu4[memMat[,4]!=0],memMat[memMat[,4]!=0,4])
x5<-data.frame(mu5[memMat[,5]!=0],memMat[memMat[,5]!=0,5])
x6<-data.frame(mu6[memMat[,6]!=0],memMat[memMat[,6]!=0,6])
x7<-data.frame(mu7[memMat[,7]!=0],memMat[memMat[,7]!=0,7])
x8<-data.frame(mu8[memMat[,8]!=0],memMat[memMat[,8]!=0,8])
x9<-data.frame(mu9[memMat[,9]!=0],memMat[memMat[,9]!=0,9])
x10<-data.frame(mu10[memMat[,10]!=0],memMat[memMat[,10]!=0,10])

############################
#Group Mus with two groups
############################
names(x1)<-c("cont","weight")
names(x2)<-c("cont","weight")
names(x3)<-c("cont","weight")
names(x7)<-c("cont","weight") 
names(x6)<-c("cont","weight")
xA<-rbind(x1,x2,x3,x6,x7)
xA<-xA[complete.cases(xA),]
meanMuA<-sum(xA[,1],na.rm=T)/sum(xA[!is.na(xA[,1]),2])
deviations<-xA[,1]-meanMuA
nominator<-sum(xA[,2]*deviations^2)
V<-sum(xA[,2])
standardDev<-sqrt(nominator/V)
CIA<-1.96*standardDev/sqrt(V)

 
names(x4)<-c("cont","weight")
names(x5)<-c("cont","weight")  
names(x8)<-c("cont","weight")
names(x9)<-c("cont","weight")
names(x10)<-c("cont","weight")

xB<-rbind(x4,x5,x8,x9,x10)
xB<-xB[complete.cases(xB),]
meanMuB<-sum(xB[,1],na.rm=T)/sum(xB[!is.na(xB[,1]),2])
deviations<-xB[,1]-meanMuB
nominator<-sum(xB[,2]*deviations^2)
V<-sum(xB[,2])
standardDev<-sqrt(nominator/V)
CIB<-1.96*standardDev/sqrt(V)

meanMuG<-c(meanMuA,meanMuB)
CIG<-c(CIA,CIB)
x<-c(1.2,1.8)
plot(x,meanMuG,xlim=c(1,2), ylim=range(c(meanMuG-CIG, meanMuG+CIG)),xaxt = "n",
     pch=19,  ylab="Mean mu (95% CI)", xlab = "Group"
)
axis(1, at=x, labels=c("A","B"))
arrows(x, meanMuG-CIG, x, meanMuG+CIG, length=0.05, angle=90, code=3)

############################
#Group Mus with three groups
############################

names(x1)<-c("cont","weight")
names(x2)<-c("cont","weight")  
names(x6)<-c("cont","weight")
x126<-rbind(x1,x2,x6)
x126<-x126[complete.cases(x126),]
meanMu126<-sum(x126[,1],na.rm=T)/sum(x126[!is.na(x126[,1]),2])
meanMuA<-meanMu126
deviations<-x126[,1]-meanMu126
nominator<-sum(x126[,2]*deviations^2)
V<-sum(x126[,2])
standardDev<-sqrt(nominator/V)
CIA<-1.96*standardDev/sqrt(V)

names(x3)<-c("cont","weight")
names(x7)<-c("cont","weight")  
xB<-rbind(x3,x7)
xB<-xB[complete.cases(xB),]
meanMuB<-sum(xB[,1],na.rm=T)/sum(xB[!is.na(xB[,1]),2])
deviations<-xB[,1]-meanMuB
nominator<-sum(xB[,2]*deviations^2)
V<-sum(xB[,2])
standardDev<-sqrt(nominator/V)
CIB<-1.96*standardDev/sqrt(V)

names(x4)<-c("cont","weight")
names(x5)<-c("cont","weight")  
names(x8)<-c("cont","weight")
names(x9)<-c("cont","weight")
names(x10)<-c("cont","weight")

xC<-rbind(x4,x5,x8,x9,x10)
xC<-xC[complete.cases(xC),]
meanMuC<-sum(xC[,1],na.rm=T)/sum(xC[!is.na(xC[,1]),2])
deviations<-xC[,1]-meanMuC
nominator<-sum(xC[,2]*deviations^2)
V<-sum(xC[,2])
standardDev<-sqrt(nominator/V)
CIC<-1.96*standardDev/sqrt(V)

meanMuG<-c(meanMuA,meanMuB,meanMuC)
CIG<-c(CIA,CIB,CIC)
x<-c(1.1,1.5,1.9)
plot(x,meanMuG,xlim=c(1,2),ylim=range(c(meanMuG-CIG, meanMuG+CIG)),xaxt="n",
     pch=19,  ylab="Mean mu (95% CI)", xlab = "Group"
)
axis(1, at=x, labels=c("A","B","C"))
arrows(x, meanMuG-CIG, x, meanMuG+CIG, length=0.05, angle=90, code=3)

############################
#Group Mus with four groups
############################

names(x1)<-c("cont","weight")
names(x2)<-c("cont","weight")  
names(x6)<-c("cont","weight")
x126<-rbind(x1,x2,x6)
x126<-x126[complete.cases(x126),]
meanMu126<-sum(x126[,1],na.rm=T)/sum(x126[!is.na(x126[,1]),2])
meanMuA<-meanMu126
deviations<-x126[,1]-meanMu126
nominator<-sum(x126[,2]*deviations^2)
V<-sum(x126[,2])
standardDev<-sqrt(nominator/V)
CIA<-1.96*standardDev/sqrt(V)

names(x3)<-c("cont","weight")
names(x7)<-c("cont","weight")  
xB<-rbind(x3,x7)
xB<-xB[complete.cases(xB),]
meanMuB<-sum(xB[,1],na.rm=T)/sum(xB[!is.na(xB[,1]),2])
deviations<-xB[,1]-meanMuB
nominator<-sum(xB[,2]*deviations^2)
V<-sum(xB[,2])
standardDev<-sqrt(nominator/V)
CIB<-1.96*standardDev/sqrt(V)

names(x4)<-c("cont","weight")
names(x5)<-c("cont","weight")
xC<-rbind(x4,x5)
xC<-xC[complete.cases(xC),]
meanMuC<-sum(xC[,1],na.rm=T)/sum(xC[!is.na(xC[,1]),2])
deviations<-xC[,1]-meanMuC
nominator<-sum(xC[,2]*deviations^2)
V<-sum(xC[,2])
standardDev<-sqrt(nominator/V)
CIC<-1.96*standardDev/sqrt(V)

names(x8)<-c("cont","weight")
names(x9)<-c("cont","weight")
names(x10)<-c("cont","weight")

xD<-rbind(x8,x9,x10)
xD<-xD[complete.cases(xD),]
meanMuD<-sum(xD[,1],na.rm=T)/sum(xD[!is.na(xD[,1]),2])
deviations<-xD[,1]-meanMuD
nominator<-sum(xD[,2]*deviations^2)
V<-sum(xD[,2])
standardDev<-sqrt(nominator/V)
CID<-1.96*standardDev/sqrt(V)

meanMuG<-c(meanMuA,meanMuB,meanMuC,meanMuD)
CIG<-c(CIA,CIB,CIC,CID)
x<-c(1.1,1.367,1.634,1.9)
plot(x,meanMuG,xlim=c(1,2),ylim=range(c(meanMuG-CIG, meanMuG+CIG)),xaxt="n",
     pch=19,  ylab="Mean mu (95% CI)", xlab = "Cluster"
)
axis(1, at=x, labels=c("A","B","C","D"))
arrows(x, meanMuG-CIG, x, meanMuG+CIG, length=0.05, angle=90, code=3)


############################
#Group Mus with three other groups
############################

names(x1)<-c("cont","weight")
names(x2)<-c("cont","weight")  
names(x6)<-c("cont","weight")
x126<-rbind(x1,x2,x6)
x126<-x126[complete.cases(x126),]
meanMu126<-sum(x126[,1],na.rm=T)/sum(x126[!is.na(x126[,1]),2])
meanMuA<-meanMu126
deviations<-x126[,1]-meanMu126
nominator<-sum(x126[,2]*deviations^2)
V<-sum(x126[,2])
standardDev<-sqrt(nominator/V)
CIA<-1.96*standardDev/sqrt(V)

names(x3)<-c("cont","weight")
names(x7)<-c("cont","weight")
names(x4)<-c("cont","weight")
names(x5)<-c("cont","weight")
xB<-rbind(x3,x4,x5,x7)
xB<-xB[complete.cases(xB),]
meanMuB<-sum(xB[,1],na.rm=T)/sum(xB[!is.na(xB[,1]),2])
deviations<-xB[,1]-meanMuB
nominator<-sum(xB[,2]*deviations^2)
V<-sum(xB[,2])
standardDev<-sqrt(nominator/V)
CIB<-1.96*standardDev/sqrt(V)

names(x8)<-c("cont","weight")
names(x9)<-c("cont","weight")
names(x10)<-c("cont","weight")

xD<-rbind(x8,x9,x10)
xD<-xD[complete.cases(xD),]
meanMuD<-sum(xD[,1],na.rm=T)/sum(xD[!is.na(xD[,1]),2])
deviations<-xD[,1]-meanMuD
nominator<-sum(xD[,2]*deviations^2)
V<-sum(xD[,2])
standardDev<-sqrt(nominator/V)
CID<-1.96*standardDev/sqrt(V)

meanMuG<-c(meanMuA,meanMuB,meanMuD)
CIG<-c(CIA,CIB,CID)
x<-c(1.1,1.5,1.9)
plot(x,meanMuG,xlim=c(1,2),ylim=range(c(meanMuG-CIG, meanMuG+CIG)),xaxt="n",
     pch=19,  ylab="Mean mu (95% CI)", xlab = "Group"
)
axis(1, at=x, labels=c("A","B","C"))
arrows(x, meanMuG-CIG, x, meanMuG+CIG, length=0.05, angle=90, code=3)