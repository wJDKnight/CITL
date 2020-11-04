library(bnlearn)
source("functions_s.R")

n_nodes=50
NOISE=1 #random niose
SNOSIE=0.2 #noise of h0 in each stage
SSIZE=1000 #each stage size, if only one stage, it is larger
ONESTAGE = F
INCLUDE_S = F #whether include stages in construction 
LATENT=F #whether consider latent variables
LAT_p=0.9 #after remove some variables, how many remains
discretized=F #whether to discretize simulated data
test_method = "mi-g-sh" #CI test method,depending on whether discretizing data

#the maximum allowed size of the conditioning sets
maxsx =floor(sqrt(n_nodes))
#maxsx=NULL

mlog=runif(3,0,2)
print(mlog)
ran=random.graph(c(LETTERS[1:(n_nodes/2)],letters[1:(n_nodes/2)]))
models<-modelstring(ran)
#get a list with effect$cause
cause_list<-list()
for(s in strsplit(chartr('][',"  ",models)," ")[[1]]){
  if(nchar(s)>0) {
    node=strsplit(chartr("|"," ",s)," ")[[1]][1]
    causes=strsplit(strsplit(chartr("|"," ",s)," ")[[1]][2],":")[[1]]
    cause_list[[node]]=causes
  }
}


c0<-function(x){
  cause_names=cause_list[[x]]
  if (is.na(cause_names)) return(data.frame(x=rep(0,SSIZE)))
  return(simu_data[paste0(cause_names,"_",0)])
}
c1<-function(x){
  cause_names=cause_list[[x]]
  if (is.na(cause_names)) return(data.frame(x=rep(0,SSIZE)))
  return(simu_data[paste0(cause_names,"_",1)])
}

simu_data=data.frame(stage=rep.int(1,SSIZE))
for (h in node.ordering(ran)) {
  a1=0.8
  a2=0.7
  b1=1
  b2=1
  e=rnorm(SSIZE,mean = 0,sd=NOISE)
  h0=rlnorm(SSIZE,meanlog = mlog[1],sdlog = SNOSIE)
  h1=apply(a1*h0+b1*c0(h)+e,1,sum)
  h2=apply(a2*h1+b2*c1(h)+e,1,sum)
  simu_data[[paste0(h,"_",0)]]=h0
  simu_data[[paste0(h,"_",1)]]=h1
  simu_data[[paste0(h,"_",2)]]=h2
}
simu_data_1<-simu_data

#second stage, mean log =1
#set.seed(778)
if(ONESTAGE){simu_data_2=data.frame()} else {simu_data=data.frame(stage=rep.int(2,SSIZE))
for (h in node.ordering(ran)) {
  a1=0.8
  a2=0.7
  b1=1
  b2=1
  e=rnorm(SSIZE,mean = 0,sd=NOISE)
  h0=rlnorm(SSIZE,meanlog = mlog[2],sdlog = SNOSIE)
  h1=apply(a1*h0+b1*c0(h)+e,1,sum)
  h2=apply(a2*h1+b2*c1(h)+e,1,sum)
  simu_data[[paste0(h,"_",0)]]=h0
  simu_data[[paste0(h,"_",1)]]=h1
  simu_data[[paste0(h,"_",2)]]=h2
}
simu_data_2<-simu_data}


#second stage, mean log =2
#set.seed(779)
if(ONESTAGE) {simu_data_3=data.frame()} else {simu_data=data.frame(stage=rep.int(3,SSIZE))
for (h in node.ordering(ran)) {
  a1=0.8
  a2=0.7
  b1=1
  b2=1
  e=rnorm(SSIZE,mean = 0,sd=NOISE)
  h0=rlnorm(SSIZE,meanlog = mlog[3],sdlog = SNOSIE)
  h1=apply(a1*h0+b1*c0(h)+e,1,sum)
  h2=apply(a2*h1+b2*c1(h)+e,1,sum)
  simu_data[[paste0(h,"_",0)]]=h0
  simu_data[[paste0(h,"_",1)]]=h1
  simu_data[[paste0(h,"_",2)]]=h2
}
simu_data_3<-simu_data
}


simu_data<-rbind(simu_data_1,simu_data_2,simu_data_3)


continuous_simu_data<-simu_data[2:(1+3*n_nodes)]

#discretization
###here, min is 0, so the 0 was not a center
discretize_kmeans<-function(x){
  
  as.factor(kmeans(x,centers=4,iter.max = 15)$cluster)
}

if(discretized==T){d_simu_data<-as.data.frame(apply(continuous_simu_data, 2, discretize_kmeans))}  else{d_simu_data<-as.data.frame(continuous_simu_data)}
#only current expression


simu_current<-d_simu_data[seq.int(2,n_nodes*3,3)]
if(LATENT) {
  s<-sample.int(n_nodes,floor(n_nodes*LAT_p))
  simu_current<-simu_current[,s]
}
if (INCLUDE_S) simu_current<-cbind(simu_current,stages_99=as.factor(simu_data$stage))
bn.pc<-pc.stable(simu_current,max.sx = maxsx,test = test_method)
all_bn_arcs<-all.bn.arcs(bn.pc)
nodup_bn_arcs<-all_bn_arcs[!duplicated(all_bn_arcs),]
half_bidi_bn_arcs<-rm.bidirect(nodup_bn_arcs)$half_bidirect
pure_direct_bn_arcs<-rm.bidirect(nodup_bn_arcs)$no_bidirect_arcs
result_Approach_1=data.frame(
  
  num_true_arcs=nrow(arcs(ran)),
  num_all_arcs=nrow(half_bidi_bn_arcs),
  num_arcs=nrow(arcs.compare(half_bidi_bn_arcs,arcs(ran))),
  num_dire=nrow(direction.compare(pure_direct_bn_arcs,arcs(ran)))
)
#only_cu_results<-rbind(only_cu_results,only_cu_result)


#current + subsequent
simu_current<-d_simu_data[seq.int(2,n_nodes*3,3)]
simu_subsequent<-d_simu_data[seq.int(3,n_nodes*3,3)]
if (LATENT) {
  simu_current<-simu_current[,s]
  simu_subsequent<-simu_subsequent[,s]
}
simu_cu_su<-cbind(simu_current,simu_subsequent)
if (INCLUDE_S) simu_cu_su<-cbind(simu_cu_su,stages_99=as.factor(simu_data$stage))
bn.pc.cu.su<-pc.stable(simu_cu_su,max.sx = maxsx,test = test_method)
all_bn_arcs<-all.bn.arcs(bn.pc.cu.su)
nodup_bn_arcs<-all_bn_arcs[!duplicated(all_bn_arcs),]
half_bidi_bn_arcs<-rm.bidirect(nodup_bn_arcs)$half_bidirect
pure_direct_bn_arcs<-rm.bidirect(nodup_bn_arcs)$no_bidirect_arcs


A1B2_arcs<-arcs.1.2(bn.pc.cu.su)
nodup_A1B2_arcs<-A1B2_arcs[!duplicated(A1B2_arcs),]
pure_direct_A1B2_arcs<-rm.bidirect(nodup_A1B2_arcs)$no_bidirect_arcs
su_arcs=pure_direct_A1B2_arcs

result_Approach_2=data.frame(
  num_true_arcs=nrow(arcs(ran)),
  num_all_arcs=nrow(half_bidi_bn_arcs),
  num_arcs=nrow(arcs.compare(half_bidi_bn_arcs,arcs(ran))),
  num_dire=nrow(direction.compare(pure_direct_A1B2_arcs,arcs(ran)))
)
#cu_su_results<-rbind(cu_su_results,cu_su_result)


#current + change
simu_current<-d_simu_data[seq.int(2,n_nodes*3,3)]
simu_change=continuous_simu_data[seq.int(3,n_nodes*3,3)]-
  continuous_simu_data[seq.int(2,n_nodes*3,3)]
if(discretized){
  simu_change=as.data.frame(apply(simu_change, 2, discretize_kmeans))
}
if (LATENT) {
  simu_current<-simu_current[,s]
  simu_change<-simu_change[,s]
}
simu_cu_change=cbind(simu_current,simu_change)
if (INCLUDE_S) simu_cu_change<-cbind(simu_cu_change,stages_99=as.factor(simu_data$stage))
bn.pc.cu.ch=pc.stable(simu_cu_change,max.sx = maxsx,test = test_method)
all_bn_arcs<-all.bn.arcs(bn.pc.cu.ch)
nodup_bn_arcs<-all_bn_arcs[!duplicated(all_bn_arcs),]
half_bidi_bn_arcs<-rm.bidirect(nodup_bn_arcs)$half_bidirect
pure_direct_bn_arcs<-rm.bidirect(nodup_bn_arcs)$no_bidirect_arcs


A1B2_arcs<-arcs.1.2(bn.pc.cu.ch)
nodup_A1B2_arcs<-A1B2_arcs[!duplicated(A1B2_arcs),]
pure_direct_A1B2_arcs<-rm.bidirect(nodup_A1B2_arcs)$no_bidirect_arcs


result_Approach_3=data.frame(
  num_true_arcs=nrow(arcs(ran)),
  num_all_arcs=nrow(half_bidi_bn_arcs),
  num_arcs=nrow(arcs.compare(half_bidi_bn_arcs,arcs(ran))),
  num_dire=nrow(direction.compare(pure_direct_bn_arcs,arcs(ran)))
  
)

result_CITL=data.frame(
  num_true_arcs=nrow(arcs(ran)),
  num_all_arcs=nrow(half_bidi_bn_arcs),
  num_arcs=nrow(arcs.compare(half_bidi_bn_arcs,arcs(ran))),
  num_dire=nrow(direction.compare(pure_direct_A1B2_arcs,arcs(ran)))
  
)
