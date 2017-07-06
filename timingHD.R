library(VIM)
library(microbenchmark)
library(ggplot2)

nRowsAll <- 10^c(2:8)
times <- c(rep(50,2),rep(50,3),rep(50,3))
mb <- list()
#for(i in 1:3){
for(i in seq_along(nRowsAll)){
  nRows <- nRowsAll[i]
  cat(nRows, "start...")
  rm(x)
  gc()
  x<-data.frame(x=rnorm(nRows),
                d1=sample(LETTERS[1:3],nRows,rep=T),d2=sample(LETTERS[1:2],nRows,rep=T))
  x[sample(1:nRows,nRows/10),1] <- NA
  if(i<=3){
    mb[[i]] <- microbenchmark(matchImpute(x,match_var=c("d1","d2")),
                              hotdeck(x,domain_var = c("d1","d2")),
                              kNN(x,variable="x",dist_var=c("d1","d2")),
                              irmi(x),
                              times=times[i]) 
  }else{
    mb[[i]] <- microbenchmark(matchImpute(x,match_var=c("d1","d2")),
                              hotdeck(x,domain_var = c("d1","d2")),
                              times=times[i])
  }
  cat("done\n")
}
save(nRowsAll,mb,file="timingHD2.RData")
load("timingHD2.RData")
fillup <- function(x,l){
  x <- c(x,rep(NA,l-length(x)))
  x
}
tmed <- do.call("rbind",lapply(mb,function(x)fillup(print(x,unit="s")$median,4)))
tlq <- do.call("rbind",lapply(mb,function(x)fillup(print(x,unit="s")$lq,4)))
tuq <- do.call("rbind",lapply(mb,function(x)fillup(print(x,unit="s")$uq,4)))

out <- data.frame(method=c(rep("matchImpute",7),rep("hotdeck",7),rep("kNN",7),rep("irmi",7)),
                  medianTime=c(tmed[,1],tmed[,2],tmed[,3],tmed[,4]),
                  lq=c(tlq[,1],tlq[,2],tlq[,3],tlq[,4]),
                  uq=c(tuq[,1],tuq[,2],tuq[,3],tlq[,4]),
                  nrow=rep(nRowsAll,4),
                  nmissing=rep(nRowsAll,4)/10)

ggplot(out[out$nrow<=1e+04,],aes(y=medianTime,x=nrow,color=method))+
  geom_ribbon(alpha=.1,color="transparent",aes(fill=method,ymax=uq,ymin=lq))+
  geom_line()
ggplot(out[out$method%in%c("matchImpute","hotdeck"),],aes(y=medianTime,x=nrow,color=method))+
  geom_ribbon(alpha=.1,color="transparent",aes(fill=method,ymax=uq,ymin=lq))+
  geom_line()+
  geom_vline(xintercept = 1e+04,color="grey70")

