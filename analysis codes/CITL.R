if (!requireNamespace("bnlearn", quietly = TRUE))
  install.packages("bnlearn")

if (!requireNamespace("snow", quietly = TRUE))
  install.packages("snow")
library(bnlearn)
library(snow)



arcs.1.2<-function(learned_graph){
  if(nrow(arcs(learned_graph))==0) return(data.frame())
  xxx_graph_arcs=data.frame()
  x=apply(arcs(learned_graph), 1, function(x){
    x=strsplit(x,"_")
    if(x$from[1]!=x$to[1]){
      if(x$from[2]!=x$to[2]){
        if(x$from[2]==1){
          f=x$from[1]  #direction same to bn
          t=x$to[1]
          return(data.frame(from=f,to=t,stringsAsFactors = F))
        }
        else {
          f=x$to[1]  #direction different to bn
          t=x$from[1]
          return(data.frame(from=f,to=t,stringsAsFactors = F))
        }
      }
    }
  })
  for (a in x) {
    if(!is.null(a)) xxx_graph_arcs=rbind(xxx_graph_arcs,data.frame(from=a[1],to=a[2],stringsAsFactors = F))
  }
  return(xxx_graph_arcs)
}

#given a arcs(not a graph), return a no bidirect arcs 
#and a half bidirect arcs
rm.bidirect<-function(arcs_df){
  if(nrow(arcs_df)==0) return(list(
    no_bidirect_arcs=data.frame(),
    half_bidirect=data.frame()
  ))
  m<-as.data.frame(arcs_df)
  x<-apply(m[,], 1, function(x){
    a=m[m[,2]==x[[1]],]
    return(sum(a[,1]==x[[2]]))
  })
  bidirct_pairs<-sum(x)/2
  onedir_pairs<-sum(x==0)
  p_no_bidirect<-m[x==0,]
  p_with_bidirect<-m[x!=0,]
  half_bidirect<-p_no_bidirect
  if(nrow(p_with_bidirect)==0){half_bidirect<-p_no_bidirect}
  else {
    for (i in 1:nrow(p_with_bidirect)) {
      from=p_with_bidirect[i,1]
      to=p_with_bidirect[i,2]
      t=sum(duplicated(rbind(
        half_bidirect,
        data.frame(from=to,to=from,stringsAsFactors = F))))
      if(t==0){half_bidirect<-rbind(half_bidirect,p_with_bidirect[i,])
      }
    }
  }
  
  return(list(
    no_bidirect_arcs=p_no_bidirect,
    half_bidirect=half_bidirect
  ))
}

n_cluster=10

Spliced<-read.csv('Spliced.csv',header = F)
delta_s<-read.csv('delta_s.csv',header = F)
gene_names<-read.csv('gene_names.csv',header=F,stringsAsFactors = F)
s_gene_names<-paste0(gene_names[1,],"_1")
colnames(Spliced)<-s_gene_names
de_gene_names<-paste0(gene_names[1,],"_2")
colnames(delta_s)<-de_gene_names

maxsx<-floor(sqrt(ncol(Spliced)))

stage = read.csv("cluster_label.csv",header = F)[,1]




cl=makeCluster(n_cluster,type = "SOCK")
spl_del<-cbind(Spliced,delta_s)
bn=pc.stable(spl_del,max.sx=maxsx,undirected = T, cluster = cl)
stopCluster(cl)

source("./functions_s.R")

A1B2_arcs<-arcs.1.2(bn) 
nodup_A1B2_arcs<-A1B2_arcs[!duplicated(A1B2_arcs),]
pure_direct_A1B2_arcs<-rm.bidirect(nodup_A1B2_arcs)$no_bidirect_arcs

sum_results<-data.frame()
for (i in 1:nrow(pure_direct_A1B2_arcs)) {
  a=pure_direct_A1B2_arcs[i,1]
  b=pure_direct_A1B2_arcs[i,2]
  
  #some gene names have problem
  #if (strsplit(a,"")[[1]][1]=="X" &strsplit(a,"")[[1]][1]!="Y") a=substring(a,2,100)
  #if (strsplit(b,"")[[1]][1]=="X" &strsplit(b,"")[[1]][1]!="Y") b=substring(b,2,100)
  
  cur_cur=cor(spliced[[a]],spliced[[b]])
  cur_cha=cor(spliced[[a]],delta[[b]])
  p=ci.test(a,b,"label",spliced,test = "mi-cg")$p.value
  
  x=data.frame(from=a,to=b,type=type,cur_cur=cur_cur,cur_cha=cur_cha,p.value=p)
  
  sum_results=rbind(sum_results,x)
}

write.csv(sum_results,"results.csv")






















