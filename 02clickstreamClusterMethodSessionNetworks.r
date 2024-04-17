#This script will produce click graphs depicting user sessions on the wiki textbook page.
#Then it will calculate 23 network measures for each of these sessions. 
#Finally, it will run a principal component analysis with rotated components. 
#You will need to have igraph and psych packages installed. 
#If you use code from this script, please cite:
#Authors (2019). Double network-based method for identifying behavioral patterns in clickstream data. MethodX. DOI: XXX
#First load clickstreamClusterMethodSession.r
#######MAKING SESSSION NETWORKS#############
library(igraph)
make.click.graph<-function(nSession){
  #need to use clicks as read above. 
  
  b<-as.matrix(nSession)
  
  cols<-c(69,70,71,43,44)
  #Information in columns
  #Column 44: document_id - a unique number for each document
  #Column 45  target_id - a unique number for each target. Targets may have the same id under different pages. 
  #           What uniquely determines a "button" is then the document_id and target_id (if we are right)
  #Column 45 target_url - what is written in the browser (except when it says "navigationX(XX);" in which case we believe you are navigating within a page)
  #Column 59  dom_element_id - Contains NavToggleXX, Math-Jax..., and other
  #Column 61  dom_element_tag - Standard HTML mark-up types (A, HTML, H3, LI, IMG, ...)
  #Column 62: dom_element_text- information about what was clicked (Problem: xxx, Titles, Question x, [show], [hide])
  time<-as.numeric(b[,11]) #Calculating delta ts to use as a link attribute. 
  time_1<-time[1:length(time)-1]
  time_2<-time[2:length(time)]
  delta_t<-time_2-time_1
  x<-vector() #creating an adjacency list
  for(i in 1:length(b[,1])){
    x[i]=paste(as.character(b[i,cols]),sep="",collapse="_") #Each row in the session table becomes a node 
  }
  sourceNode<-x[1:length(x)-1] # nodes to which actions point
  targetNode<-x[2:length(x)] #nodes from which actions point
  edgelist<-cbind(sourceNode,targetNode) #the finished adjacency list
  g<-graph.edgelist(as.matrix(edgelist),directed=T) #use igraph to make an graph from the adjacency list
  E(g)$delta_t<-delta_t #Links of graph get the delta_t attribute. 
  E(g)$weight<-1 #The weight attribute of each link is set to 1. 
  g$week<-as.numeric(b[1,18]) #graph level attributes for later analyses. 
  g$date<-as.numeric(b[1,12])
  g$day<-as.character(b[1,16])
  g$year<-as.numeric(b[1,13])
  g$hour<-as.numeric(b[1,46])
  g$session_id<-as.numeric(b[1,3])
  g$ip_address<-as.character(b[1,20])
  g$userName<-as.character(b[1,31])
  if("edit"%in%nSession$type){#Label with session type.
    g$stype<-"editSession"
  } else if("to problem"%in%nSession$type) {
    g$stype<-"problemSession"
  } else
    g$stype<-"otherSession"
  a<-length(V(g)) # begin label each node with different type attributes
  m<-matrix(NA,nrow = a,ncol=5)
  for(i in 1:a){
    m[i,]<-strsplit(V(g)$name[[i]],"_")[[1]]
    
  }
  V(g)$interaction<-m[,1]
  V(g)$showhide<-m[,2]
  V(g)$hintsol<-m[,3]
  V(g)$document_id<-m[,4]
  V(g)$target_id<-m[,5]
  
  
  return(g)  
}
networkArray<-list()#Create all networks
for (i in 1:length(bb)){
  networkArray[[i]]<-make.click.graph(sessionArray[[i]])
  
}

#######CALCULATE STRUCTURAL MEASURES ON NETWORKS#############
###Calculations on networks take a while. If you do not want to wait, feel free to use the following .csv:
description<-read.csv("characteristicsAll.csv")
description<-data.frame(description)

###Otherwise, this is the code for generating that data file:
###Structural network measures that are not included in igraph:
source("tarEnt.r") #Please cite Authors 2013 if you use this. 
source("searchInf.r") #Please cite Authors 2013 if you use this. 

#Given graph, g, the following function calculates the structural measures used in the article:
characteristics<-function(g){
  #call functions searchInf.r and TargetEntropy.r first
  #load igraph before use
  g<-simplify(g,edge.attr.comb = sum) #Most measures will respond well to multiple links Here, we sum links to produce weighted links
  N<-vcount(g) #Number of nodes
  L<-ecount(g) #number of links
  plf<-power.law.fit(degree.distribution(g)) #Fitting the degree distribution to a power law
  a<-plf$alpha #The power law coefficient
  p<-plf$KS.p #The KS-p-value for the fit
  S<--sum(degree.distribution(g)*log(degree.distribution(g)),na.rm=T) #Entropy of the degree distribution
  d<-graph.density(g) #Graph density
  m<-motifs(g)[-c(1,2,4)] #Motifs, excluding trivial ones
  diam<-diameter(g) #Diameter of the graph
  cl<-transitivity(g) #Clustering coefficient (also called transitivity)
  mu<-length(which(is.mutual(g))) #No. of mutual links
  te<-mean(TargetEntropy(g),na.rm=T) #Target entropy -- this takes some time
  sMat<-sInfMatrix(g) #Search information matrix. This takes some time. 
  si<-sum(sMat,na.rm=T)/length(which(!is.na(sMat))) #Total search information. 
  apl<-average.path.length(g) #Average path length
  
  result<-data.frame(N,L,a,p,S,d,t(m),diam,cl,mu,te,si,apl)
  return(result)
}

#characterize<-matrix(0,ncol=25,nrow=length(networkArray))
#for (i in 1:length(networkArray)){
 # characterize[i,]<-as.numeric(characteristics(networkArray[[i]]))
#}

#description<-data.frame(data.frame)

#########PCA#################
library(psych)
fa.parallel(description[-c(1,4,5)]) # A parallel anlalyses ot estimate the optimal number of components. This number varies between 4 and 6 when we run fa.parallel. 
pca.net<-principal(description[-c(1,4,5)],nfactor=5,missing=T,impute="mean") #We have chosen 5 components. Columns 1, 4, and 5 were not meaningful in our analyses, so they are left out. 
