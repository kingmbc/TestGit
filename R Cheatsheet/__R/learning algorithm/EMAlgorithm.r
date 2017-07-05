#EM 알고리즘에 대한 설명은 이전 포스트에 정리되어 있습니다.

# 다변량 정규분포의 확률 값을 계산하기 위한 함수
dmnorm <- function(p, x, means, covM) {
  temp1 <- 1/(((2*pi)^(p/2))*sqrt(det(covM)))
  temp2 <- -0.5*((x-means)%*%solve(covM)%*%as.matrix(x-means))
  return(temp1*exp(temp2))
}



# data.size : 데이터 크기
# var.dim : 변수개수
# prs : 군집들의 확률
# mE : 오차한계
# logE : 로그추정량
# prC_x : 각각의 데이터가 각각의 군집에 속할 확률
# result : 각각의 데이터가 속한 그룹번호



EM_Clustering <- function(dataSet, k) {
  dataSet <- as.matrix(dataSet)
  data.size <- nrow(dataSet)
  var.dim <- ncol(dataSet)
  
  # Step 1 : 모수 초기화
  means <- matrix(0, k, var.dim)
  covM <- matrix(0, k, var.dim*var.dim)
  
  means[1, ] <- c(5.006, 3.428, 1.462, 0.246)
  means[2, ] <- c(5.936, 2.770, 4.260, 1.326)
  means[3, ] <- c(6.588, 2.974, 5.552, 2.026)
  
  cov1 <- cov(dataSet[1:50, ])
  cov2 <- cov(dataSet[51:100, ])
  cov3 <- cov(dataSet[101:150, ])
  
  covM[1, ] <- as.vector(cov1)
  covM[2, ] <- as.vector(cov2)
  covM[3, ] <- as.vector(cov3)
  
  prs <- c(1/k, 1/k, 1/k)
  mE <- 10^-3
  
  # 로그 추정량 계산
  logE <- 0
  for(i in 1:data.size) {
    temp <- 0
    for(j in 1:k) {
      temp <- temp+(prs[j]*dmnorm(var.dim, dataSet[i, ], means[j, ], matrix(covM[j, ], var.dim, var.dim)))
    }
    logE[1] <- logE[1]+log(temp)
  }
  
  
  prC_x <- c()
  result <- c()
  
  
  
  # Step 2 : n번째 반복에서의 확률계산
  # Step 3 : 모수에 대한 추정량 갱신
  n <- 0
  result <- 0
  while(n<10) {
    # Pr(Cluster|x) 값 계산
    for(i in 1:data.size) {
      temp_1 <- 0  # Pr(x)를 계산 할 임시변수
      temp_2 <- 0  # Pr(x|Cluster)*Pr(x) 값을 비교 할 임시변수
      temp_3 <- 0  # x가 속하는 군집번호를 저장 할 임시변수
      
      temp_4 <- c() # Pr(x|Cluster)*Pr(x) 값을 저장 할 임시변수
      
      for(j in 1:k) {
        temp_4[j] <- prs[j]*(prs[j]*dmnorm(var.dim, dataSet[i, ], means[j, ], matrix(covM[j, ], var.dim, var.dim)))
        temp_1 <- temp_1+temp_4[j]
        if(temp_2 < temp_4[j]) {
          temp_2 <- temp_4[j]
          temp_3 <- j
        }
      }
      result[i] <- temp_3
      prC_x[i] <- temp_2/temp_1
    }
    
    
    
    # 군집들의 확률과 평균 계산
    for(i in 1:k) {
      temp_1 <- 0  # Pr(Cluster|x) 값의 합을 저장 할 임시변수
      
      temp_2 <- 0  # x*Pr(Cluster|x) 값의 합을 저장 할 임시변수
      for(j in 1:data.size) {
        if(result[j]==i) {
          temp_1 <- temp_1+prC_x[j]
          temp_2 <- temp_2+(dataSet[j, ]*prC_x[j])
        }
      }
      prs[i] <- temp_1/data.size
      means[i, ] <- temp_2/temp_1
    }
    
    
    
    # 군집들의 분산 계산
    for(i in 1:k) {
      temp_1 <- 0  # Pr(Cluster|x) 값을 저장 할 임시변수
      
      temp_2 <- 0  # Pr(Cluster|x)*(x-mean of Cluster)^2 값을 저장 할 임시변수
      
      for(j in 1:data.size) {
        if(result[j]==i) {
          temp_1 <- temp_1+prC_x[j]
          temp_2 <- temp_2+(prC_x[j]*(as.matrix(dataSet[j, ]-means[i, ])%*%(dataSet[j, ]-means[i, ])))
        }
      }
      covM[i, ] <- as.vector(temp_2/temp_1)
    }
    
    # Step 4 : 로그 추정량 계산 및 변화량 비교
    newLogE <- 0
    for(i in 1:data.size) {
      temp <- 0  # Temporary variable to save value of log estimate
      for(j in 1:k) {
        temp <- temp+(prs[j]*dmnorm(var.dim, dataSet[i, ], means[j, ], matrix(covM[j, ], var.dim, var.dim)))
      }
      newLogE <- newLogE+log(temp)
    }
    
    if(abs(logE[n+1]-newLogE)<=mE) {
      cat("The number of looping : ", n+1, "\n")
      cat("Result : \n")
      return(result)
    } else {
      n <- n+1
      logE[n+1] <- newLogE
    }
  }
  
  # Step 5 : Output
  cat("The number of looping : ", n+1, "\n")
  cat("Result : \n")
  return(result)
}


dataSet = as.matrix(iris[,-5]);
rst = EM_Clustering(as.matrix(iris[,-5]), 3) 

library(EMCluster)
demo(allinit, 'EMCluster', ask = F, echo = TRUE)
demo(allinit_ss, 'EMCluster', ask = F, echo = TRUE)
demo ( allinit , 'EMCluster', ask = F)


################### DEMO
library(EMCluster, quiet = TRUE)
set.seed(1234)
x <- da1$da
TC <- da1$class
n <- nrow(x)
p <- ncol(x)
k <- 10
ret.em <- init.EM(x, nclass = k, method = "em.EM")
ret.Rnd <- init.EM(x, nclass = k, method = "Rnd.EM", EMC = .EMC.Rnd)
ret.Rndp <- init.EM(x, nclass = k, method = "Rnd.EM", EMC = .EMC.Rndp)
ret.svd <- emgroup(x, nclass = k)
par(mfrow = c(2, 2))
plotem(ret.em, x, main = "em")
plotem(ret.Rnd, x, main = "Rnd")
plotem(ret.Rndp, x, main = "Rnd+")
plotem(ret.svd, x, main = "svd")
ret.all <-
  cbind(
    c(ret.em$llhdval, ret.Rnd$llhdval, ret.Rndp$llhdval,
    ret.svd$llhdval),
    c(RRand(ret.em$class, TC)$adjRand,
    RRand(ret.Rnd$class, TC)$adjRand,
    RRand(ret.Rndp$class, TC)$adjRand,
    RRand(ret.svd$class, TC)$adjRand)
  )

rownames(ret.all) <- c("em", "Rnd", "Rnd+", "svd")
colnames(ret.all) <- c("logL", "adjR")
ret.all

## emgroup
emobj <- emgroup(x, nclass = 10)
summary(emobj)

ret.0 <- starts.via.svd(x, nclass = 10, method = "kmeans")
summary(ret.0)

## End(Not run)