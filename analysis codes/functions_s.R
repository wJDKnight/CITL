#functions

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

#sum bi directions if there are duplicates
#this is not useful
sum_bi_directions<-function(m){
  m<-as.data.frame(m)
  x<-apply(m[,], 1, function(x){
    a=m[m[,2]==x[[1]],]
    return(sum(a[,1]==x[[2]]))
  })
  return(sum(x)/2)
}
#sum same directions (caused by XXX framework)
#this is not useful
sum_same_directions<-function(m){
  m<-as.data.frame(m)
  x<-apply(m[,], 1, function(x){
    a=m[m[,1]==x[[1]],]
    return(sum(a[,2]==x[[2]]))
  })
  return((sum(x)-nrow(m))/2)
}

#arcs of a graph withou duplicates zhijieyong duplicate() jiuxingle
arcs_nodup<-function(bnet){
  learned<-data.frame()
  c_list<-c()
  for (geneA in names(bnet$nodes)) {
    #print(geneA)
    c_list<-append(c_list,geneA)
    for (geneB in nbr(bnet,geneA)) {
      if(is.element(geneB,c_list)) next
      else {learned<-rbind(learned,data.frame(geneA,geneB,stringsAsFactors = F))
      }    
    }
  }
  return(learned)
}

#all arcs of A-B in bn
all.bn.arcs<-function(learned_graph){
  if(nrow(arcs(learned_graph))==0) return(data.frame())
  bn_graph_arcs=data.frame()
  x=apply(arcs(learned_graph), 1, function(x){
    x=strsplit(x,"_")
    if(x$from[1]!=x$to[1]){
      #it doesn't matter whether 1 or 2
      f=x$from[1]  #direction same to bn
      t=x$to[1]
      return(data.frame(from=f,to=t,stringsAsFactors = F))
      
    }
  })
  for (a in x) {
    if(!is.null(a)) bn_graph_arcs=rbind(bn_graph_arcs,data.frame(from=a[1],to=a[2],stringsAsFactors = F))
  }
  return(bn_graph_arcs)
}

#arcs of A1-B2 in a bn
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
#arcs of A-B_delta in a bn
arcs.na.delta<-function(learned_graph){
  if(nrow(arcs(learned_graph))==0) return(data.frame())
  xxx_graph_arcs=data.frame()
  x=apply(arcs(learned_graph), 1, function(x){
    x=strsplit(x,"_")
    if(x$from[1]!=x$to[1]){
      if(length(x$from)!=length(x$to)){
        if(length(x$from)<length(x$to)){
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

#compare two arcs with no bidirect
arcs.compare<-function(learned,truth){
  if(nrow(learned)==0|nrow(truth)==0) return(data.frame())
  #sum correct arcs
  x=apply(learned[,],1,function(x){
    #print(x)
    a=matrix(truth[truth[,1]==x[[1]],],ncol = 2,byrow = F)
    b=matrix(truth[truth[,2]==x[[1]],],ncol = 2,byrow = F)
    if(sum(a[,2]==x[[2]])==1) return(1)
    if(sum(b[,1]==x[[2]])==1) return(1)
    return(0)
  })
  return(learned[x==1,])
 
}
direction.compare<-function(learned,truth){
  if(nrow(learned)==0|nrow(truth)==0) return(data.frame())
  #sum correct arcs(minus bi direct)
  y=apply(learned,1,function(x){
    #print(x)
    a=matrix(truth[truth[,1]==x[[1]],],ncol = 2,byrow = F)
    b=matrix(truth[truth[,2]==x[[1]],],ncol = 2,byrow = F)
    if(sum(a[,2]==x[[2]])==1) return(1)
    if(sum(b[,1]==x[[2]])==1) return(0)
    return(0)
  })
  return(learned[y==1,])
}




