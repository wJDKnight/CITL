if (!requireNamespace("bnlearn", quietly = TRUE))
  install.packages("bnlearn")

if (!requireNamespace("snow", quietly = TRUE))
  install.packages("snow")
library(bnlearn)
library(snow)

source("./functions_s.R")

args<-commandArgs(T) 

n_cluster=as.numeric(args[4])

Spliced<-read.csv(args[1],header = F)
delta_s<-read.csv(args[2],header = F)
gene_names<-read.csv(args[3],header=F,stringsAsFactors = F)
s_gene_names<-paste0(gene_names[1,],"_1")
colnames(Spliced)<-s_gene_names
de_gene_names<-paste0(gene_names[1,],"_2")
colnames(delta_s)<-de_gene_names

maxsx<-floor(sqrt(ncol(Spliced)))

#stage = read.csv("cluster_label.csv",header = F)[,1]


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
  
  cur_cur=cor(Spliced[[paste0(a,"_1")]],Spliced[[paste0(b,"_1")]])
  cur_cha=cor(Spliced[[paste0(a,"_1")]],delta_s[[paste0(b,"_2")]])
  cur_sub=cor(Spliced[[paste0(a,"_1")]],(delta_s[[paste0(b,"_2")]]+Spliced[[paste0(b,"_1")]]))
  x=data.frame(from=a,to=b,cur_cur=cur_cur,cur_cha=cur_cha,cur_sub=cur_sub)
  
  sum_results=rbind(sum_results,x)
}

write.csv(sum_results,"results.csv")






















