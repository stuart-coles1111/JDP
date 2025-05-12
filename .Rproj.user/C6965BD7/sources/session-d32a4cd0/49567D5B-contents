newstrengths_l <- function(p1,p2){
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)

  s1_1 <- sarilhos_l(p1[1])
  s2_1 <- formos_l(p1[2])
  s3_1 <- series_l(p1[3])


  s1_2 <- sarilhos_l(p2[1])
  s2_2 <- formos_l(p2[2])
  s3_2 <- series_l(p2[3])


  c_1 <- (s1_1 + s2_1 + s3_1)
  c_2 <- (s1_2 + s2_2 + s3_2)
  list(
    a_1=c(s1_1,s2_1,s3_1),
    a_2=c(s1_2,s2_2,s3_2),
    s=c(c_1,c_2),
    p=c(exp(c_1 - c_2)/(1 + exp(c_1 - c_2)), 1/(1 + exp(c_1 - c_2)))
  )
}


game_sim_l <- function(){

  #    a1 <- runif(3, 0, 10)
  #    a2 <- runif(3, 0, 10)
  a1=11
  a2=11
  while(max(a1) > 10 | max(a2) > 10){
    a1 <- rdirichlet(1,c(1,1,1)) * 15 + runif(3,-.5,.5)
    a2 <- rdirichlet(1,c(1,1,1)) * 15 + runif(3,-.5,.5)
  }
  s <- newstrengths_l(a1,a2)[[4]][1]
  w <-  0
  l <- 0
  while(w<5 & l <5){
    res <- sample(c(1,0),1,F,c(s,1-s))
    if(res==1)w<-w+1
    else l <- l+1
  }
  c(a1,a2,s,1-s,w,l) %>% round(2)
}



mgame_sim_l <- function(n){
  df <- lapply(1:n,function(x)game_sim_l())
  df <- do.call(rbind, df) %>% as.data.frame
  colnames(df) <- c('sarilhos_1','formos_1','series_1','sarilhos_2','formos_2','series_2','p_1','p_2','w_1','w_2')
  df
}



newch_l <- function(p){
  y <- -newstrengths_l(c(p[1],p[2],15-(p[1]+p[2])),c(p[1],p[2],15-(p[1]+p[2])))[[3]][2]
  cat(y,fill=T)
  y
}

newopt_l <- function(q, init = c(5,5)){
  res <- optim(init,newch_l,control=list(maxit=5000),method="Nelder-Mead")
  list(res, c(res$par, 15-sum(res$par)) %>% round(2))
}

# version 1

l <- function(x, k){
  if(any(x<0 | x > 10))return(-100)
  k*x
}


sarilhos_l <- function(x, k = 1.2){
  l(x,k)
}

formos_l <- function(x, k=1.8){
  l(x,k)
}


series_l <- function(x, k = 1.5){
  l(x,k)
}


newopt_l()
set.seed(15)
temp <- mgame_sim_l(100)
temp=temp[,-c(7,8)]
temp[,1:6] = temp[,1:6] * 1000
temp = cbind(match=1:100,temp)
write.csv(temp, 'jogo_v1.csv',row.names=FALSE)

dum = temp
dum$p=dum$w_1/(dum$w_1 + dum$w_2)
gam(p~s(sarilhos_1) + s(sarilhos_2)+ s(formos_1) + s(formos_2)+ s(series_1) + s(series_2), data=dum)%>% plot


# version 2

l <- function(x, k){
  if(any(x<0 | x > 10))return(-100)
  k*x
}


sarilhos_l <- function(x, k = 1.2){
  l(x,k)
}

formos_l <- function(x, k=1.8){
  pmin(l(x,k), l(5,k))
  #    l(x,k)
}


series_l <- function(x, k = 1.5){
  pmin(l(x,k), l(8,k))
  #    l(x,k)
}



newopt_l()
set.seed(15)
temp <- mgame_sim_l(100)
temp=temp[,-c(7,8)]
temp[,1:6] = temp[,1:6] * 1000
temp = cbind(match=1:100,temp)
write.csv(temp, 'jogo_v2.csv',row.names=FALSE)

dum = temp
dum$p=dum$w_1/(dum$w_1 + dum$w_2)
gam(p~s(sarilhos_1) + s(sarilhos_2)+ s(formos_1) + s(formos_2)+ s(series_1) + s(series_2), data=dum)%>% plot


