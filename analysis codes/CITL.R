if (!requireNamespace("bnlearn", quietly = TRUE))
  install.packages("bnlearn")

if (!requireNamespace("snow", quietly = TRUE))
  install.packages("snow")
library(bnlearn)
library(snow)

source("./functions_s.R")

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






















