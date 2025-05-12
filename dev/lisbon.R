formos <- function(x, b=.5){
  if(any(x<0 | x > 10))return(0)
  x/10 * b
}

sarilhos <- function(x, a=0.5, b=1){
  if(any(x<0 | x > 10))return(0)
  (1-exp(-a * x))*b
}

series <- function(x,a=.5, b=2, lim = 0.5){
  if(any(x<0 | x > 10))return(0)
  y <- b * (1/(11-x)^a - 1/11^a)/(1 - 1/11^a)
  pmin(y, b*lim)
}


l <- function(x, k){
  if(any(x<0 | x > 10))return(-100)
  k*x
}


sarilhos_l <- function(x, k = 1.2){
  #    pmin(l(x,k), l(8,k))
  l(x,k)
}

formos_l <- function(x, k=1.8){
  #   pmin(l(x,k), l(5,k))
  l(x,k)
}



series_l <- function(x, k = 1.5){
  #    pmin(l(x,k), l(7,k))
  l(x,k)
}

newstrengths <- function(p1,p2, alpha=3){
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)

  s1_1 <- sarilhos(p1[1])
  s2_1 <- formos(p1[2])
  s3_1 <- series(p1[3])


  s1_2 <- sarilhos(p2[1])
  s2_2 <- formos(p2[2])
  s3_2 <- series(p2[3])


  c_1 <- (s1_1 + s2_1 + s3_1)
  c_2 <- (s1_2 + s2_2 + s3_2)
  list(
    a_1=c(s1_1,s2_1,s3_1),
    a_2=c(s1_2,s2_2,s3_2),
    s=c(c_1,c_2),
    p=c(exp(c_1 - c_2)/(1 + exp(c_1 - c_2)), 1/(1 + exp(c_1 - c_2)))
  )
}

newstrengths_l <- function(p1,p2, alpha=3){
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

newch <- function(p){
  y <- -newstrengths(c(p[1],p[2],15-(p[1]+p[2])),c(p[1],p[2],15-(p[1]+p[2])))[[3]][2]
  cat(y,fill=T)
  y
}

newopt <- function(q, init = c(5,5)){
  res <- optim(init,newch,control=list(maxit=5000),method="Nelder-Mead")
  list(res, c(res$par, 15-sum(res$par)) %>% round(2))
}



game_sim <- function(){

  a1 <- runif(3, 0, 10)
  a2 <- runif(3, 0, 10)


  s <- newstrengths(a1,a2)[[4]][1]
  w <-  0
  l <- 0
  while(w<5 & l <5){
    res <- sample(c(1,0),1,F,c(s,1-s))
    if(res==1)w<-w+1
    else l <- l+1
  }
  c(a1,a2,s,1-s,w,l) %>% round(2)
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

mgame_sim <- function(n){
  df <- lapply(1:n,function(x)game_sim())
  df <- do.call(rbind, df) %>% as.data.frame
  colnames(df) <- c('sarilhos_1','formos_1','series_1','sarilhos_2','formos_2','series_2','p_1','p_2','w_1','w_2')
  df
}

mgame_sim_l <- function(n){
  df <- lapply(1:n,function(x)game_sim_l())
  df <- do.call(rbind, df) %>% as.data.frame
  colnames(df) <- c('sarilhos_1','formos_1','series_1','sarilhos_2','formos_2','series_2','p_1','p_2','w_1','w_2')
  df
}

set.seed(99)
temp <- mgame_sim(100)
temp=temp[,-c(7,8)]
temp[,1:6] = temp[,1:6] * 1000
temp = cbind(match=1:100,temp)
write.csv(temp, 'modified_jogo.csv',row.names=FALSE)
print(xtable(temp, digits=0), include.rownames=FALSE)

dum <- temp
dum <- dum %>%
  mutate(
    winner=ifelse(w_1 > w_2, 1,2),
    margin = factor(abs(w_1-w_2), levels=1:5, order=TRUE),
    totals = w_1 + w_2,
    sarilhos_w = ifelse(winner == 1, sarilhos_1, sarilhos_2),
    formos_w = ifelse(winner == 1, formos_1, formos_2),
    series_w = ifelse(winner == 1, series_1, series_2),
    sarilhos_l = ifelse(winner == 2, sarilhos_1, sarilhos_2),
    formos_l = ifelse(winner == 2, formos_1, formos_2),
    series_l = ifelse(winner == 2, series_1, series_2),
    sarilhos_s = sarilhos_w - sarilhos_l,
    formos_s = formos_w - formos_l,
    series_s = series_w - series_l
  )

dum$p=dum$w_1/(dum$w_1 + dum$w_2)
dum$q = log(dum$p/(1-dum$p))
mod <- gam(data = dum, as.numeric(margin) ~ 0 + sarilhos_s + formos_s + series_s, family=ocat(R=5), method="REML")
gam(p ~ s(sarilhos_s), data=dum)%>% plot
==================

  library(dplyr)
library(mgcv)

dum <- read.csv("newjogo.csv")
dum <- dum %>%
  mutate(
    winner=ifelse(w_1 > w_2, 1,2),
    margin = factor(abs(w_1-w_2), levels=1:5, order=TRUE),
    totals = w_1 + w_2,
    sarilhos_w = ifelse(winner == 1, sarilhos_1, sarilhos_2),
    formas_w = ifelse(winner == 1, formas_1, formas_2),
    series_w = ifelse(winner == 1, series_1, series_2),
    sarilhos_l = ifelse(winner == 2, sarilhos_1, sarilhos_2),
    formas_l = ifelse(winner == 2, formas_1, formas_2),
    series_l = ifelse(winner == 2, series_1, series_2),
    sarilhos_s = sarilhos_w - sarilhos_l,
    formas_s = formas_w - formas_l,
    series_s = series_w - series_l
  )


mod <- gam(data = dum, as.numeric(margin) ~ 0 + sarilhos_s + formas_s + series_s, family=ocat(R=5), method="REML")
dum=mgame_sim(10000)
dum$p=dum$w_1/(dum$w_1 + dum$w_2)
gam( p ~ s(formas_1), data=dum) %>% plot


newch_l <- function(p){
  y <- -newstrengths_l(c(p[1],p[2],15-(p[1]+p[2])),c(p[1],p[2],15-(p[1]+p[2])))[[3]][2]
  cat(y,fill=T)
  y
}

newopt_l <- function(q, init = c(5,5)){
  res <- optim(init,newch_l,control=list(maxit=5000),method="Nelder-Mead")
  list(res, c(res$par, 15-sum(res$par)) %>% round(2))
}



# linear version



l <- function(x, k){
  if(any(x<0 | x > 10))return(-100)
  k*x
}


sarilhos_l <- function(x, k = 1.2){
  pmin(l(x,k), l(8,k))
  #    l(x,k)
}

formos_l <- function(x, k=1.8){
  pmin(l(x,k), l(5,k))
  #    l(x,k)
}


series_l <- function(x, k = 1.5){
  l(x,k)
}

set.seed(12)
temp <- mgame_sim_l(100)
temp=temp[,-c(7,8)]
temp[,1:6] = temp[,1:6] * 1000
temp = cbind(match=1:100,temp)
write.csv(temp, 'linear_jogo.csv',row.names=FALSE)


dum <- temp
dum <- dum %>%
  mutate(
    winner=ifelse(w_1 > w_2, 1,2),
    margin = factor(abs(w_1-w_2), levels=1:5, order=TRUE),
    totals = w_1 + w_2,
    sarilhos_w = ifelse(winner == 1, sarilhos_1, sarilhos_2),
    formos_w = ifelse(winner == 1, formos_1, formos_2),
    series_w = ifelse(winner == 1, series_1, series_2),
    sarilhos_l = ifelse(winner == 2, sarilhos_1, sarilhos_2),
    formos_l = ifelse(winner == 2, formos_1, formos_2),
    series_l = ifelse(winner == 2, series_1, series_2),
    sarilhos_s = sarilhos_w - sarilhos_l,
    formos_s = formos_w - formos_l,
    series_s = series_w - series_l
  )
dum$p=dum$w_1/(dum$w_1 + dum$w_2)
gam(p~(sarilhos_1) + (sarilhos_2)+ (formos_1) + (formos_2)+ (series_1) + (series_2), data=dum)%>% summary
