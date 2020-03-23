convg=function(sub.seq.in, thresh.in){
  init.sub.seq.in=sub.seq.in
  init.thresh.in=thresh.in

  max.seq.val=c()
  max.seq.val[1]=1/3

  for(i in 2:9){
    max.seq.val[i]=max(init.sub.seq.in[[i]])
  }

  index.j=min(which(max.seq.val >= init.thresh.in))

  if(index.j<9 | max.seq.val[9] >= init.thresh.in){
    index.max=which(init.sub.seq.in[[index.j]]==max.seq.val[index.j])
    out.result=list(index.j, max.seq.val[index.j], index.max)
    return(out.result)
  } else return(list(NA, NA, NA))
}
