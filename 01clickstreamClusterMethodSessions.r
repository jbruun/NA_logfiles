#This R-script will convert the OWA_clickstream20nov2014.csv to an array of sessions
#Then it will produce a time-class measure and classify sessions accordingly
#Finally, it will produce a measure for the relative difference in show clicks and hide clicks
#If you use code from this script, please cite:
#Authors (2019). Double network-based method for identifying behavioral patterns in clickstream data. MethodX. DOI: XXX

############################################
########PRELIMINARIES#######################
############################################

#Read data and define column names
#Download OWA_clickstream20nov2014.csv from this Google Drive folder: 
#https://drive.google.com/drive/folders/1tTyesPnyqpqhKJzz5qKpQfUQIAJJEY6I?usp=sharing
clicks<-read.csv("OWA_clickstream20nov2014.csv", header = F)
names(clicks)<-c("id","visitor_id","session_id","site_id","referer_id","ua_id","host_id","os_id","location_id","referring_search_term_id","timestamp","yyyymmdd","year","month","day","dayofweek","dayofyear","weekofyear","last_req","ip_address","is_new_visitor","is_repeat_visitor","language","days_since_prior_session","days_since_first_session","num_prior_sessions","medium","source_id","ad_id","campaign_id","user_name","cv1_name","cv1_value","cv2_name","cv2_value","cv3_name","cv3_value","cv4_name","cv4_value","cv5_name","cv5_value","last_impression_id","document_id","target_id","target_url","hour","minute","second","msec","click_x","click_y","page_width","page_height","position","approx_position","dom_element_x","dom_element_y","dom_element_name","dom_element_id","dom_element_value","dom_element_tag","dom_element_text","dom_element_class","dom_element_parent_id","tag_id","placement_id","ad_group_id","host")

#----------------
#These actions serve to make interpretations simpler when analysing sessions and networks
#Search for "http://vnt.nmi3.org/wiki/index.php" and replace with "wiki:"
newClicks<-gsub("http://tvnt.nmi3.org/wiki/index.php", "wiki:", clicks$target_url)
newClicks<-gsub("http://vnt.nmi3.org/wiki/index.php", "wiki:", newClicks)

#Also, this url: "javascript://toggleNavigationBar" signals "navigation" on a web-page
newClicks<-gsub("javascript:toggleNavigationBar", "navigation:", newClicks)
newClicks<-gsub("javascript://toggleNavigationBar", "navigation:", newClicks)
clicks$target_url<-newClicks
#----------------

#Distinguish between clicks and other actions. 
clicks$interactionType<-"other"
interactionClick<-grep("A",clicks$dom_element_tag)
clicks$interactionType[interactionClick]<-"hyperref"

#Distinguish between types of actions
clicks$type<-"other"
# We want to be able to find out which sessions explicitly displays interactions with problems
problem<-grep("Problem:",clicks$dom_element_text) 
# Some sessions involve editing, which is done by administrators of the site only. 
edit<-grep("Edit",clicks$dom_element_text)
#Other types of sessions
question<-grep("Question", clicks$dom_element_text)
exercise<-grep("Exercises",clicks$dom_element_text)
simProj<-grep("Simulation project",clicks$dom_element_text)
#Whether the action involved clicking a show/hide button
show<-grep("show",clicks$dom_element_text)
hide<-grep("hide",clicks$dom_element_text)
#integrating all this in type parameter:
clicks$type[show]<-"show"
clicks$type[hide]<-"hide"
clicks$type[problem]<-"to problem"
clicks$type[question]<-"to question"
clicks$type[exercise]<-"to exercise"
clicks$type[simProj]<-"to sim project"
clicks$type[edit]<-"edit"

#Identification of problems by their name 
problemID<-unique(clicks$target_id[clicks$type=="to problem"])
table(as.character(clicks$dom_element_text[which(clicks$target_id==problemID[1])]))
problemName<-vector()
problemName[1]<-"Velocity Selector"
problemName[2]<-"Attenuation neutron beam"
problemName[3]<-"Select materials neutron scattering experiment"
problemName[4]<-"Moderator temperature"
problemName[5]<-"Beam port"
problemName[6]<-"Neutr guide sys"
problemName[7]<-"Bragg scatt-monochrom"
problemName[8]<-"Collimator"
problemName[9]<-"Pinhole collim"
problemName[10]<-"Form fact spher"
problemName[11]<-"Estim circ area"
problemName[12]<-"Struc fact dilu"
problemName[13]<-"Fourier transf"
problemName[14]<-"Cross section"
problemName[15]<-"H as mod"
problemName[16]<-"Vali semi-clas appr"
problemName[17]<-"Guide system"
problemName[18]<-"SANS q-range & res"
problemName[19]<-"Poly-disp spheres"
problemName[20]<-"Bilayer Liposomes"
problemName[21]<-"Calc SANS-SAXS micell"
problemName[22]<-"Not a problem!"
problemName[23]<-"SANS-SAXS micell"
problemName[24]<-"Sim SANS"
problemName[25]<-"Bragg Bravais"
problemName[26]<-"Use of ITC"
problemName[27]<-"Bragg Non-Bravais"
problemName[28]<-"Clas lat vib 1-dim"
problemName[29]<-"Be filter"
problemName[30]<-"Form fact cyl"

clicks$problemName<-"not problem"
for (i in 1:length(problemName)){
  clicks$problemName[clicks$target_id==problemID[i]]<-problemName[i]
}

for (i in 1:length(problemName)){
  clicks$problemName[clicks$document_id==problemID[i]]<-problemName[i]
}


#Finding target_ids of NavToggles
#Here we check each page with a show/hide feature to find out whether the show/hide is related to a problem
navProb<-data.frame(problem=clicks$problemName,docID=clicks$document_id,type=clicks$type,targetID=clicks$target_id,ID=clicks$dom_element_id)
navToggleTargetID<-vector()
navToggleTargetID[1]<-names(table(navProb$targetID[navProb$ID=="NavToggle1" & navProb$problem!="not problem"]))[1]
navToggleTargetID[2]<-names(table(navProb$targetID[navProb$ID=="NavToggle2" & navProb$problem!="not problem"]))[1]
navToggleTargetID[3]<-names(table(navProb$targetID[navProb$ID=="NavToggle3" & navProb$problem!="not problem"]))[1]
navToggleTargetID[4]<-names(table(navProb$targetID[navProb$ID=="NavToggle4" & navProb$problem!="not problem"]))[1]
navToggleTargetID[5]<-names(table(navProb$targetID[navProb$ID=="NavToggle5" & navProb$problem!="not problem"]))[1]
navToggleTargetID[6]<-names(table(navProb$targetID[navProb$ID=="NavToggle6" & navProb$problem!="not problem"]))[1]
navToggleTargetID[7]<-names(table(navProb$targetID[navProb$ID=="NavToggle7" & navProb$problem!="not problem"]))[1]
navToggleTargetID[8]<-names(table(navProb$targetID[navProb$ID=="NavToggle8" & navProb$problem!="not problem"]))[1]
navToggleTargetID[9]<-names(table(navProb$targetID[navProb$ID=="NavToggle9" & navProb$problem!="not problem"]))[1]
navToggleTargetID[10]<-names(table(navProb$targetID[navProb$ID=="NavToggle10" & navProb$problem!="not problem"]))[2]
navToggleTargetID<-as.numeric(navToggleTargetID)

#We create a new type that signifies whether a solution or hint was clicked. 
#This was done manually by aligning with the actual webpage in question
clicks$type2<-"not hint/sol"
clicks$type2[clicks$document_id==problemID[2] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[2] & clicks$target_id==navToggleTargetID[2] ]<- "solution"

clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[4] ]<- "hint"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[5] ]<- "solution"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[6] ]<- "solution"
clicks$type2[clicks$document_id==problemID[3] & clicks$target_id==navToggleTargetID[7] ]<- "solution"

clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[2] ]<- "hint"
clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[4] ]<- "solution"
clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[5] ]<- "solution"
clicks$type2[clicks$document_id==problemID[4] & clicks$target_id==navToggleTargetID[6] ]<- "solution"

clicks$type2[clicks$document_id==problemID[5] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[5] & clicks$target_id==navToggleTargetID[2] ]<- "hint"
clicks$type2[clicks$document_id==problemID[5] & clicks$target_id==navToggleTargetID[3] ]<- "hint"
clicks$type2[clicks$document_id==problemID[5] & clicks$target_id==navToggleTargetID[4] ]<- "solution"
clicks$type2[clicks$document_id==problemID[5] & clicks$target_id==navToggleTargetID[5] ]<- "solution"

clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[4] ]<- "hint"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[5] ]<- "solution"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[6] ]<- "hint"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[7] ]<- "solution"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[8] ]<- "hint"
clicks$type2[clicks$document_id==problemID[6] & clicks$target_id==navToggleTargetID[9] ]<- "solution"

clicks$type2[clicks$document_id==problemID[7] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[7] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[7] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[7] & clicks$target_id==navToggleTargetID[4] ]<- "solution"

clicks$type2[clicks$document_id==problemID[8] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[8] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[8] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[8] & clicks$target_id==navToggleTargetID[4] ]<- "solution"

clicks$type2[clicks$document_id==problemID[10] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[10] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[10] & clicks$target_id==navToggleTargetID[3] ]<- "solution"

clicks$type2[clicks$document_id==problemID[11] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[11] & clicks$target_id==navToggleTargetID[2] ]<- "solution"

clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[2] ]<- "hint"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[4] ]<- "hint"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[5] ]<- "solution"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[6] ]<- "hint"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[7] ]<- "solution"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[8] ]<- "hint"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[9] ]<- "solution"
clicks$type2[clicks$document_id==problemID[13] & clicks$target_id==navToggleTargetID[10] ]<- "solution"

clicks$type2[clicks$document_id==problemID[14] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[14] & clicks$target_id==navToggleTargetID[2] ]<- "hint"
clicks$type2[clicks$document_id==problemID[14] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[14] & clicks$target_id==navToggleTargetID[4] ]<- "solution"

clicks$type2[clicks$document_id==problemID[15] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[15] & clicks$target_id==navToggleTargetID[2] ]<- "solution"

clicks$type2[clicks$document_id==problemID[16] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[16] & clicks$target_id==navToggleTargetID[2] ]<- "solution"

clicks$type2[clicks$document_id==problemID[17] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[17] & clicks$target_id==navToggleTargetID[2] ]<- "solution"

clicks$type2[clicks$document_id==problemID[18] & clicks$target_id==navToggleTargetID[1] ]<- "hint"
clicks$type2[clicks$document_id==problemID[18] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[18] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[18] & clicks$target_id==navToggleTargetID[4] ]<- "solution"

clicks$type2[clicks$document_id==problemID[19] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[19] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[19] & clicks$target_id==navToggleTargetID[3] ]<- "solution"

clicks$type2[clicks$document_id==problemID[23] & clicks$target_id==navToggleTargetID[1] ]<- "hint"

clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[1] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[2] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[3] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[4] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[5] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[6] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[7] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[8] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[9] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[10] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[11] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[12] ]<- "solution"
clicks$type2[clicks$document_id==problemID[25] & clicks$target_id==navToggleTargetID[13] ]<- "solution"
########END PRELIMINARIES#######################

########WORK WITH SESSION#######################
#First give each session a number, link that to the session id, and duration of each session
#This can be done with the following function
session.durations<-function(d,min.time){
  id<-unique(d$session_id) #Extract unique session ids
  
  duration<-vector()
  for (i in 1:length(id)){#For each unique id, find the maximum and minimum timestamp to find duration
    duration[i]<-max(d$timestamp[d$session_id==id[i]])-min(d$timestamp[d$session_id==id[i]])
    
  }
  sessionNo<-c(1:length(id))#provide numbering starting from 1 for easier reference later on
  sessions<-data.frame(sessionNo,id,duration)
  sessions<-sessions[sessions$duration>min.time,] #return only sessions above a certain duration (min.time measured in seconds)
  return(sessions)   
}

aa<-session.durations(clicks,300) #identify sessions longer than five minutes
bb<-aa$sessionNo #Extract session numbers

#Function for making sessions
makeSession<-function(d,n){
  #d is the dataset, here "clicks"
  #n is the session number
  id<-unique(d$session_id)  #Extract unique session ids
  session<-d[d$session_id==id[n],] #dump actions in table
  session<-session[order(session$timestamp),] #make sure actions are ordered according to timestamp
  return(session)
}

#Make array of sessions
sessionArray <-list()
for (i in 1:length(bb)){
  sessionArray[[i]]<-makeSession(clicks,bb[i])
  
}

#Classify sessions in terms of type
type<-vector()
for (i in 1:length(bb)){
  if("edit"%in%sessionArray[[i]]$type){#We are not analysing sessions in which content was edited
    type[i]<-"editSession"
  } else if("to problem"%in%sessionArray[[i]]$type) {#We analyse a non-edit session in which a problem was visited
    type[i]<-"problemSession"
  } else
    type[i]<-"otherSession"
  
}

ps<-which(type=="problemSession") #identifying the 231 problem sessions (duration> 5 minutes)

#---- Timeclass categorisation --- 
durps<-aa$duration[ps] #Find durations of problem sessions
#First estimate with outliers of more than 
density(durps) # This yields a first quartile of 148529s > 41 hours
length(durps[durps<=148529]) #229 sessions in first quartile is not meaningful
#Different possible time class categorisations:
density(durps[durps<3600]) 
timeClass1<-vector()
timeClass1[durps<625.5]<-"short"
timeClass1[durps>=625.5 & durps<1943.5]<-"middle"
timeClass1[durps>=1943.5 & durps<3261.5]<-"long"
timeClass1[durps>=3261.5 & durps<4579.5]<-"extensive"
timeClass1[durps>=4579.5]<-"very extensive"

density(durps[durps<5400]) 
timeClass15<-vector()
timeClass15[durps<884.4]<-"short"
timeClass15[durps>=884.4 & durps<2836.5]<-"middle"
timeClass15[durps>=2836.5 & durps<4788.6]<-"long"
timeClass15[durps>=4788.6 & durps<6740.7]<-"extensive"
timeClass15[durps>=6740.7]<-"very extensive"

density(durps[durps<7200])

timeClass2<-vector() 
timeClass2[durps<1155]<-"short"
timeClass2[durps>=1155 & durps<3734]<-"middle"
timeClass2[durps>=3734 & durps<6313]<-"long"
timeClass2[durps>=6313 & durps<8891]<-"extensive"
timeClass2[durps>=8891]<-"very extensive"

density(durps[durps<9000]) 
timeClass25<-vector()
timeClass25[durps<1395]<-"short"
timeClass25[durps>=1395 & durps<4624]<-"middle"
timeClass25[durps>=4624 & durps<7854]<-"long"
timeClass25[durps>=7854]<-"extensive"

density(durps[durps<10800]) #Using 3 hours as a boundary yields quartiles 1467,5038,8609, max=12179<10800, so 8609 becomes top boundary. 
timeClass3<-vector()
timeClass3[durps<1467]<-"short"
timeClass3[durps>=1467 & durps<5038]<-"middle"
timeClass3[durps>=5038 & durps<8609]<-"long"
timeClass3[durps>=8609]<-"extensive"

#These time class variables are used later on in relation to Segregation calculations.

#-------Measure for analysing relationg between showing and hiding hints/solutions
calcMu<-function(s){#s is a session
  N_s<-length(which(s$type=="show")) #Number of clicks on show
  N_h<-length(which(s$type=="hide")) #Number of clicks on hide
  mu<-(N_s-N_h)/(N_s+N_h) #mu measure
  res<-c(N_s,N_h,mu)
  return(res)
  
}

mus<-matrix(0,nrow=231,ncol=3) #mus for problem-solving sessions
colnames(mus)<-c("N_s","N_h","mu")
for(i in 1:length(ps)){
  mus[i,]<-calcMu(sessionArray[ps[i]][[1]])
}

musx<-mus[,3]
ordmusx<-vector()
ordmusx[is.na(musx)]<-"NAmusx"
ordmusx[musx==1]<-"ONEmusx"
ordmusx[musx==0]<-"ZEROmusx"
ordmusx[musx<0]<-"NEGAmusx"
ordmusx[0<musx & musx<1]<-"REALmusx"

table(ordmusx)

musx<-musx[!is.na(musx)]

musAll<-matrix(0,nrow=2184,ncol=3)
for(i in 1:2184){
  musAll[i,]<-calcMu(sessionArray[[i]])
}

MUS<-musAll[,3]
MUS<-MUS[bb[!(bb%in%bb[ps])]]#Selecting all sessions that are not problems.
ordMUS<-vector()
ordMUS[is.na(MUS)]<-"NAMUS"
ordMUS[MUS==1]<-"ONEMUS"
ordMUS[MUS==0]<-"ZEROMUS"
ordMUS[MUS<0]<-"NEGAMUS"
ordMUS[0<MUS & MUS<1]<-"REALmusx"

table(ordMUS)

MUS<-MUS[!is.na(MUS)]

wilcox.test(MUS,musx)


