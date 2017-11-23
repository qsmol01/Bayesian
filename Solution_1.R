DataClean = function(csvfile1, csvfile2, csvfile3, csvfile4, csvfile5){
  file1 = read.csv(csvfile1)
  file2 = read.csv(csvfile2)
  file3 = read.csv(csvfile3)
  file4 = read.csv(csvfile4)
  file5 = read.csv(csvfile5)
  
  Shenyang = na.contiguous(file1$PM_US.Post)
  Chengdu = na.contiguous(file2$PM_US.Post)
  Beijing = na.contiguous(file3$PM_US.Post)
  Guangzhou = na.contiguous(file4$PM_US.Post)
  Shanghai = na.contiguous(file5$PM_US.Post)
  
  write.csv(Shenyang, file = "Shenyang.csv")
  write.csv(Chengdu, file = "Chengdu.csv")
  write.csv(Beijing, file = "Beijing.csv")
  write.csv(Guangzhou, file = "Guangzhou.csv")
  write.csv(Shanghai, file = "Shanghai.csv")
}

category = function(ts){
  ts = ceiling(ts / 30)
  unlist(lapply(ts,FUN = function(X)min(X,10)))
}

GeneratePMatrix = function(ts){
  P = matrix(0,nrow = 10,ncol = 10)
  ts = category(ts)
  for(i in 2:length(ts)){
    P[ts[i],ts[i-1]] = P[ts[i],ts[i-1]] + 1
  }
  #P1 current moment is condition
  P100 = apply(P,2,sum)
  P10 = matrix(rep(P100,10),byrow = T,ncol = 10)
  P1 = rbind(P / P10,P100/sum(P))
  
  #P2 next moment is condition
  P200 = apply(P,1,sum)
  P20 = matrix(rep(P200,10),ncol = 10,byrow = F)
  P2 = cbind(P / P20,P200/sum(P))
  P1[is.na(P1)] = 0
  P2[is.na(P2)] = 0
  return(list(P1,P2))
}

predic_1 = function(pm25,P){
  
  predict_2 = function(i,j,P){
    
    (P[[1]][i,j] * P[[1]][11,j]  + 1)/ (P[[2]][i,11] + 10)
  }
  
  pm25 = category(pm25)
  labels = paste(seq(1,271,30),c(seq(30,270,30),"more"),sep="~")
  
  res = matrix(numeric(),nrow = 10,ncol = 1,dimnames = list(labels,c("Pr")))
  for(i in 1:10){
    res[i,1] = predict_2(pm25,i,P)
  }
  res[is.na(res)] = 0
  row.names(res) = labels
  
  expect = labels[order(res[,1],decreasing = T)[1]]
  
  cat("Expected value:",expect,"\n")
  print(res)
  return(invisible(list(res,expect)))
}

predictpm25 = function(pm25,city){
  city = paste(city,".csv", sep = "")
  ts = read.csv(city)$x
  P = GeneratePMatrix(ts)
  predic_1(pm25,P)
}
