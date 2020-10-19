#evaluating results of simulations
#including :
#approach123+etick; latent; instant; approach0
#without Scribe(it's in its own scripts(time-lagged and instant).)


evalu.arcs<-function(only_cu_results){
  tp=only_cu_results$num_arcs
  fp=only_cu_results$num_all_arcs-only_cu_results$num_arcs
  fn=only_cu_results$num_true_arcs-only_cu_results$num_arcs
  precision=tp/(tp+fp)
  recall=tp/(tp+fn)
  f=2*precision*recall/(precision+recall)
  return(data.frame(precision=precision,
                    recall=recall,
                    f=f))
}

evalu.direcions<-function(cu_su_results){
  precision<-cu_su_results$num_dire/cu_su_results$num_all_arcs
  recall<-cu_su_results$num_dire/cu_su_results$num_true_arcs
  f=2*precision*recall/(precision+recall)
  return(data.frame(precision=precision,
                    recall=recall,
                    f=f))
}
evalu.dire.xxx<-function(cu_su_results){
  precision<-cu_su_results$num_xxx_dire/cu_su_results$num_all_arcs
  recall<-cu_su_results$num_xxx_dire/cu_su_results$num_true_arcs
  f=2*precision*recall/(precision+recall)
  return(data.frame(precision=precision,
                    recall=recall,
                    f=f))
}

ls=readRDS("./50/single_nolat_continuous.rds")
only_cu_results<-data.frame()
cu_su_results<-data.frame()
cu_ch_results<-data.frame()
#i=1
for (l in ls) {
  #print(i)
  #i=i+1
  only_cu_results=rbind(only_cu_results,l$o)
  cu_su_results=rbind(cu_su_results,l$s)
  cu_ch_results=rbind(cu_ch_results,l$h)
}


only_cur=evalu.arcs(only_cu_results)
#sapply(only_cur, mean,na.rm=T)
#sapply(only_cur, sd,na.rm=T)
cur_sub=evalu.arcs(cu_su_results)
#sapply(cur_sub, mean,na.rm=T)
#sapply(cur_sub, sd,na.rm=T)
cur_cha=evalu.arcs(cu_ch_results)
#sapply(cur_cha, mean,na.rm=T)
#sapply(cur_cha, sd,na.rm=T)
m_ordered=cbind(sapply(only_cur, mean,na.rm=T),sapply(cur_sub, mean,na.rm=T),sapply(cur_cha, mean,na.rm=T))
s_ordered=cbind(sapply(only_cur, sd,na.rm=T),sapply(cur_sub, sd,na.rm=T),sapply(cur_cha, sd,na.rm=T))
write.csv(m_ordered,"m1_multistages_1_0.1.csv")
write.csv(s_ordered,"s1_multistages_1_0.1.csv")

#not plot for now
plot_data<-cbind(rbind(only_cur,cur_sub,cur_cha),legend=c(
  rep("only current",200),
  rep("current and subsequent",200),
  rep("current and changing",200)))

ggplot(plot_data)


only_cur=evalu.direcions(only_cu_results)
cur_sub_bn=evalu.direcions(cu_su_results)
cur_sub_xxx=evalu.dire.xxx(cu_su_results)
cur_cha_bn=evalu.direcions(cu_ch_results)
cur_cha_xxx=evalu.dire.xxx(cu_ch_results)
m_ordered=cbind(
  sapply(only_cur,mean,na.rm=T),
  sapply(cur_sub_bn,mean,na.rm=T),
  sapply(cur_sub_xxx,mean,na.rm=T),
  sapply(cur_cha_bn,mean,na.rm=T),
  sapply(cur_cha_xxx,mean,na.rm=T)
)
s_ordered=cbind(
  sapply(only_cur,sd,na.rm=T),
  sapply(cur_sub_bn,sd,na.rm=T),
  sapply(cur_sub_xxx,sd,na.rm=T),
  sapply(cur_cha_bn,sd,na.rm=T),
  sapply(cur_cha_xxx,sd,na.rm=T)
)
write.csv(m_ordered,"m2_multistages_1_0.1.csv")
write.csv(s_ordered,"s2_multistages_1_0.1.csv")




#plot lat  arcs
library(ggplot2)
library(Hmisc)

lat0=readRDS("./lat/single_nolat_continuous.rds")
lat0.1=readRDS("./lat/single_lat9_continuous.rds")
lat0.3=readRDS("./lat/single_lat7_continuous.rds")
lat0.5=readRDS("./lat/single_lat5_continuous.rds")

shapedata<-function(ls,lat_p){
  only_cu_results<-data.frame()
  cu_su_results<-data.frame()
  cu_ch_results<-data.frame()
  for (l in ls) {
    only_cu_results=rbind(only_cu_results,l$o)
    cu_su_results=rbind(cu_su_results,l$s)
    cu_ch_results=rbind(cu_ch_results,l$h)
  }
  only_cur=evalu.arcs(only_cu_results)
  #only_cur<-only_cur[!is.nan(only_cur$f),]
  cur_sub=evalu.arcs(cu_su_results)
  #cur_sub<-cur_sub[!is.nan(cur_sub$f),]
  cur_cha=evalu.arcs(cu_ch_results)
  #cur_cha<-cur_cha[!is.nan(cur_cha$f),]
  p_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$precision)
  p_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$precision)
  p_ch=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3/eTICK",500),value=cur_cha$precision)
  p_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha$precision)
  precisions=rbind(p_cu,p_su,p_ch_bn,p_ch)
  
  r_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$recall)
  r_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$recall)
  r_ch=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3/eTICK",500),value=cur_cha$recall)
  r_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha$recall)
  recalls=rbind(r_cu,r_su,r_ch_bn,r_ch)
  f_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$f)
  f_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$f)
  f_ch=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3/eTICK",500),value=cur_cha$f)
  f_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha$f)
  fs=rbind(f_cu,f_su,f_ch_bn,f_ch)
  return(list(precisions=precisions,
              recalls=recalls,
              fs=fs))
}

precision_data=rbind(shapedata(lat0,"0%")$precisions,
                     shapedata(lat0.1,"10%")$precisions,
                     shapedata(lat0.3,"30%")$precisions,
                     shapedata(lat0.5,"50%")$precisions)

recal_data=rbind(shapedata(lat0,"0%")$recalls,
                 shapedata(lat0.1,"10%")$recalls,
                 shapedata(lat0.3,"30%")$recalls,
                 shapedata(lat0.5,"50%")$recalls)

f_data=rbind(shapedata(lat0,"0%")$fs,
             shapedata(lat0.1,"10%")$fs,
             shapedata(lat0.3,"30%")$fs,
             shapedata(lat0.5,"50%")$fs)
geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4,position = position_dodge(width = 0.9))
  )
}
ggplot(precision_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL) +ylab("Precision")
ggplot(recal_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL)+ylab("Recall")
ggplot(f_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL)+ylab("F-measure")

edges_data<-rbind(precision_data,recal_data,f_data)
edges_data$value_type=factor(x=c(rep("Precision",8000),rep("Recall",8000),rep("F-measure",8000)),levels = c("Precision","Recall","F-measure"))
COLOR=c("Model 1"="#F3D5AD","Model 2"="#F5B895","Model 3/eTICK"="#B5C7D3","eTICK"="#0F4C81")

ggplot(edges_data[edges_data$model_type!="Model 3",], aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_manual(values =  COLOR)+
  xlab(NULL)+ylab(NULL)+
  facet_wrap(~value_type, nrow = 1)+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),axis.line = element_line(color = 'black'),axis.ticks.x = element_blank())


#direction



shapedata.dir<-function(ls,lat_p){
  only_cu_results<-data.frame()
  cu_su_results<-data.frame()
  cu_ch_results<-data.frame()
  for (l in ls) {
    only_cu_results=rbind(only_cu_results,l$o)
    cu_su_results=rbind(cu_su_results,l$s)
    cu_ch_results=rbind(cu_ch_results,l$h)
  }
  only_cur=evalu.direcions(only_cu_results)
  #only_cur<-only_cur[!is.nan(only_cur$f),]
  cur_sub=evalu.dire.xxx(cu_su_results)
  #cur_sub<-cur_sub[!is.nan(cur_sub$f),]
  cur_cha_bn=evalu.direcions(cu_ch_results)
  cur_cha=evalu.dire.xxx(cu_ch_results)
  #cur_cha<-cur_cha[!is.nan(cur_cha$f),]
  p_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$precision)
  p_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$precision)
  p_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha_bn$precision)
  p_ch=data.frame(lat=rep(lat_p,500),model_type=rep("eTICK",500),value=cur_cha$precision)
  precisions=rbind(p_cu,p_su,p_ch_bn, p_ch)
  
  r_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$recall)
  r_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$recall)
  r_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("eTICK",500),value=cur_cha$recall)
  r_ch=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha_bn$recall)
  recalls=rbind(r_cu,r_su,r_ch_bn,r_ch)
  f_cu=data.frame(lat=rep(lat_p,500),model_type=rep("Model 1",500),value=only_cur$f)
  f_su=data.frame(lat=rep(lat_p,500),model_type=rep("Model 2",500),value=cur_sub$f)
  f_ch=data.frame(lat=rep(lat_p,500),model_type=rep("eTICK",500),value=cur_cha$f)
  f_ch_bn=data.frame(lat=rep(lat_p,500),model_type=rep("Model 3",500),value=cur_cha_bn$f)
  fs=rbind(f_cu,f_su,f_ch_bn,f_ch)
  return(list(precisions=precisions,
              recalls=recalls,
              fs=fs))
}

precision_data=rbind(shapedata.dir(lat0,"0%")$precisions,
                     shapedata.dir(lat0.1,"10%")$precisions,
                     shapedata.dir(lat0.3,"30%")$precisions,
                     shapedata.dir(lat0.5,"50%")$precisions)

recal_data=rbind(shapedata.dir(lat0,"0%")$recalls,
                 shapedata.dir(lat0.1,"10%")$recalls,
                 shapedata.dir(lat0.3,"30%")$recalls,
                 shapedata.dir(lat0.5,"50%")$recalls)

f_data=rbind(shapedata.dir(lat0,"0%")$fs,
             shapedata.dir(lat0.1,"10%")$fs,
             shapedata.dir(lat0.3,"30%")$fs,
             shapedata.dir(lat0.5,"50%")$fs)
geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4,position = position_dodge(width = 0.9))
  )
}
ggplot(precision_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL) +ylab("Precision")
ggplot(recal_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL)+ylab("Recall")
ggplot(f_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL)+ylab("F-measure")

edges_data<-rbind(precision_data,recal_data,f_data)
edges_data$value_type=factor(x=c(rep("Precision",8000),rep("Recall",8000),rep("F-measure",8000)),levels = c("Precision","Recall","F-measure"))
COLOR=c("Model 1"="#F3D5AD","Model 2"="#F5B895","Model 3"="#B5C7D3","eTICK"="#0F4C81")

ggplot(edges_data, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_manual(values=COLOR)+
  xlab(NULL)+ylab(NULL)+
  facet_wrap(~value_type, nrow = 1)+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),axis.line = element_line(color = 'black'),axis.ticks.x = element_blank())

#how well a method determine the direction given a true causal pair
precision_data_arcs=rbind(shapedata(lat0,"0%")$precisions,
                          shapedata(lat0.1,"10%")$precisions,
                          shapedata(lat0.3,"30%")$precisions,
                          shapedata(lat0.5,"50%")$precisions)
precision_data_directions=rbind(shapedata.dir(lat0,"0%")$precisions,
                     shapedata.dir(lat0.1,"10%")$precisions,
                     shapedata.dir(lat0.3,"30%")$precisions,
                     shapedata.dir(lat0.5,"50%")$precisions)
direction_precision=cbind(precision_data_directions[,1:2],value=precision_data_directions$value/precision_data_arcs$value)

geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4,position = position_dodge(width = 0.9))
  )
}
ggplot(direction_precision, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL) +ylab("Precision of determining direction")
COLOR=c("Model 1"="#F3D5AD","Model 2"="#F5B895","Model 3"="#B5C7D3","eTICK"="#0F4C81")
ggplot(direction_precision, aes(lat,value, fill = model_type))+
  geom_boxplot(aes(),outlier.shape = NA) +
  theme(panel.background = element_blank(),axis.line = element_line(color = 'black'),axis.ticks.x = element_blank())+
  xlab(NULL) +ylab("Ability of Determining Directions (ADD)")+ scale_color_brewer(palette = "Accent")+scale_fill_manual(values = COLOR)

#old
#old
# how well a method determine the direction given a true causal pair including nonxxx
#shapedata.dir.nonxxx<-function(ls,lat_p){
  only_cu_results<-data.frame()
  cu_su_results<-data.frame()
  cu_ch_results<-data.frame()
  for (l in ls) {
    only_cu_results=rbind(only_cu_results,l$o)
    cu_su_results=rbind(cu_su_results,l$s)
    cu_ch_results=rbind(cu_ch_results,l$h)
  }
  only_cur=evalu.direcions(only_cu_results)
  #only_cur<-only_cur[!is.nan(only_cur$f),]
  cur_sub=evalu.direcions(cu_su_results)
  #cur_sub<-cur_sub[!is.nan(cur_sub$f),]
  cur_cha=evalu.direcions(cu_ch_results)
  #cur_cha<-cur_cha[!is.nan(cur_cha$f),]
  p_cu=data.frame(lat=rep(lat_p,500),model_type=rep("only cur",500),value=only_cur$precision)
  p_su=data.frame(lat=rep(lat_p,500),model_type=rep("cur+sub",500),value=cur_sub$precision)
  p_ch=data.frame(lat=rep(lat_p,500),model_type=rep("cur+cha",500),value=cur_cha$precision)
  precisions=rbind(p_cu,p_su,p_ch)
  
  r_cu=data.frame(lat=rep(lat_p,500),model_type=rep("only cur",500),value=only_cur$recall)
  r_su=data.frame(lat=rep(lat_p,500),model_type=rep("cur+sub",500),value=cur_sub$recall)
  r_ch=data.frame(lat=rep(lat_p,500),model_type=rep("cur+cha",500),value=cur_cha$recall)
  recalls=rbind(r_cu,r_su,r_ch)
  f_cu=data.frame(lat=rep(lat_p,500),model_type=rep("only cur",500),value=only_cur$f)
  f_su=data.frame(lat=rep(lat_p,500),model_type=rep("cur+sub",500),value=cur_sub$f)
  f_ch=data.frame(lat=rep(lat_p,500),model_type=rep("cur+cha",500),value=cur_cha$f)
  fs=rbind(f_cu,f_su,f_ch)
  return(list(precisions=precisions,
              recalls=recalls,
              fs=fs))
  

precision_data_directions_nonxxx=rbind(shapedata.dir.nonxxx(lat0,"0%")$precisions,
                                shapedata.dir.nonxxx(lat0.1,"10%")$precisions,
                                shapedata.dir.nonxxx(lat0.3,"30%")$precisions,
                                shapedata.dir.nonxxx(lat0.5,"50%")$precisions)
direction_precision_nonxxx=cbind(precision_data_directions_nonxxx[,1:2],value=precision_data_directions_nonxxx$value/precision_data_arcs$value)
direction_precision=direction_precision[!direction_precision$model_type=="only cur",]
direction_precision_all=rbind(direction_precision,direction_precision_nonxxx)
geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "bar", position = "dodge"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4,position = position_dodge(width = 0.9))
  )
}

ggplot(direction_precision_all, aes(lat,value,fill=model_type)) + geom_mean()+ scale_fill_brewer(palette = "Accent")+
  xlab(NULL) +ylab("Precision on discovered edges")


ggplot(direction_precision_all, aes(lat,value, fill = model_type))+
  geom_boxplot(aes(),outlier.shape = NA) +
  xlab(NULL) +ylab("Precision on discovered edges")+ scale_color_brewer(palette = "Accent")+scale_fill_brewer(palette = "Accent")










#instant
ins<-readRDS("./50/sing_instant_continucous.rds")
only_cu_results<-data.frame()
cu_su_results<-data.frame()
cu_ch_results<-data.frame()
for (l in ins) {
  only_cu_results=rbind(only_cu_results,l$o)
  cu_su_results=rbind(cu_su_results,l$s)
  cu_ch_results=rbind(cu_ch_results,l$h)
}
only_cur=evalu.arcs(only_cu_results)
cur_sub=evalu.arcs(cu_su_results)
cur_cha=evalu.arcs(cu_ch_results)
m_ordered=cbind(sapply(only_cur, mean,na.rm=T),sapply(cur_sub, mean,na.rm=T),sapply(cur_cha, mean,na.rm=T))
m_ordered
s_ordered=cbind(sapply(only_cur, sd,na.rm=T),sapply(cur_sub, sd,na.rm=T),sapply(cur_cha, sd,na.rm=T))

only_cur=evalu.direcions(only_cu_results)
cur_sub_bn=evalu.direcions(cu_su_results)
cur_sub_xxx=evalu.dire.xxx(cu_su_results)
cur_cha_bn=evalu.direcions(cu_ch_results)
cur_cha_xxx=evalu.dire.xxx(cu_ch_results)
m_ordered=cbind(
  sapply(only_cur,mean,na.rm=T),
  sapply(cur_sub_bn,mean,na.rm=T),
  sapply(cur_sub_xxx,mean,na.rm=T),
  sapply(cur_cha_bn,mean,na.rm=T),
  sapply(cur_cha_xxx,mean,na.rm=T)
)
s_ordered=cbind(
  sapply(only_cur,sd,na.rm=T),
  sapply(cur_sub_bn,sd,na.rm=T),
  sapply(cur_sub_xxx,sd,na.rm=T),
  sapply(cur_cha_bn,sd,na.rm=T),
  sapply(cur_cha_xxx,sd,na.rm=T)
)


#approach 0
ls=readRDS("./approach0/approach0_multistage.rds")

cu_ch_results<-data.frame()
#i=1
for (l in ls) {
  #print(i)
  #i=i+1

  cu_ch_results=rbind(cu_ch_results,l$h)
}

approach0_arcs_results=data.frame()
for (cor_cutoff in seq(from = 0.1, to = 0.9,by =0.05)) {
  cur_cha=evalu.arcs(cu_ch_results[cu_ch_results$cor_cutoff==cor_cutoff,])
  d=data.frame(cor_cutoff=cor_cutoff,t(sapply(cur_cha, mean,na.rm=T)))
  approach0_arcs_results=rbind(approach0_arcs_results,d)
  
}

approach0_dire_results=data.frame()
for (cor_cutoff in seq(from = 0.1, to = 0.9,by =0.05)) {
  cur_cha=evalu.dire.xxx(cu_ch_results[cu_ch_results$cor_cutoff==cor_cutoff,])
  d=data.frame(cor_cutoff=cor_cutoff,t(sapply(cur_cha, mean,na.rm=T)))
  approach0_dire_results=rbind(approach0_dire_results,d)
  
}

write.csv(approach0_arcs_results,"./approach0/approach0_arcs_results_multi.csv")
write.csv(approach0_dire_results,"./approach0/approach0_dire_results_multi.csv")
