#K-평균 군집화 기법은 비계층적(non-hierarchical) 방법인 최적 분리(partitioning) 군집 방법의 하나이며, 대용량의 자료에서 군집을 발견하는데 효과적인 방법으로 알려져 있다.

#Algorithm : K-means
#    step1. 군집 수 k를 결정하고, 각 군집에 초기치(initial value) 또는 군집 중심(cluster center)을 1개씩 할당한다.
#    step2. 모든 데이터를 가장 가까운 군집 중심에 배속시킨다.
#    step3. 각 군집에 배속된 데이터로 새로운 군집 중심을 계산한다.
#    step4. 군집 중심이 변화가 거의 없을 때까지 step2.와 step3.을 반복한다.
#    step5. Output

#Example) K-평균 군집화 기법을 이용한 붓꽃 데이터 군집화(Using R)

# DataSet : 군집화할 데이터
# row.size : 개체 수
# col.size : 한 개체의 변수 개수
# ClusterNum : 군집 수
# ClusterCenter : 군집 중심
# Deviation : 편차
# LastClusterCenter : 이전 군집 중심
# Result : 결과(속해있는 집단)를 저장할 배열

KmeansClustering <- function(DataSet, ClusterNum) {
    row.size <- nrow(DataSet)
    col.size <- ncol(DataSet)

    # Step 1 : sample을 뽑아 첫 seed 결정
    select.no <- sample(row.size, ClusterNum, replace=FALSE)
    #select.no=c(1,2,3)
    ClusterCenter <- matrix(0, ClusterNum, col.size+1)
    for(i in 1:ClusterNum) {
        ClusterCenter[i, 1:col.size] <- DataSet[select.no[i], 1:col.size]
    }
    # print(ClusterCenter)

    Result <- matrix(0, row.size, 1)
    LastClusterCenter <- 0
    Deviation <- matrix(0, 1, ClusterNum)
    n <- 0

    while(n<20) {
        LastClusterCenter <- ClusterCenter

        # Step 2 : 가장 가까운 군집 중심의 그룹에 배속
        #각 점별로 각 Cluster와의 Deviation을 구한다.
        #가장 작은 Deviation을 가진 Cluster에 그 점이 속해진다.
        for(i in 1:row.size) {
            for(j in 1:ClusterNum) {
                # 편차계산
                Deviation[, j] <- sqrt(sum((DataSet[i, 1:col.size]-ClusterCenter[j, 1:col.size])^2))
            }
            Minimum <- min(Deviation)
            for(j in 1:ClusterNum) {
                # 가장 가까운 집단에 배속
                if(Deviation[j]==Minimum) {
                    Result[i, 1] <- j
                    break
                }
            }
        }

        # Step 3 : 새로운 군집 중심 계산
        ClusterCenter <- matrix(0, ClusterNum, col.size+1)

        # 군집 별 평균 계산
        for(i in 1:ClusterNum) {
            k <- 0
            for(j in 1:row.size) {
                if(Result[j, 1]==i) {
                    k <- k+1
                    ClusterCenter[i, 1:col.size] <- ClusterCenter[i, 1:col.size]+DataSet[j, 1:col.size]
                }
            }
            ClusterCenter[i, col.size+1] <- k
            ClusterCenter[i, 1:col.size] <- ClusterCenter[i, 1:col.size]/k
        }
        #print(ClusterCenter)



        # Step 4 : 군집 중심의 변화 확인
        if(sum((LastClusterCenter-ClusterCenter)^2)==0) {
            print(ClusterCenter)
            return(Result)
        }
        n <- n+1
    }
    return(Result)
}

# 군집화할 iris 데이터 만들기
DataSet <- as.matrix(iris[, -5])
ClusterNum <- 3
x = KmeansClustering(DataSet, ClusterNum)
kmeans(DataSet, 3)  # K-means clustering 결과


# 결과확인

x <- KmeansClustering(DataSet, ClusterNum)

#위의 결과는 군집화된 각 집단에 대한 변수별 평균값과 그 집단에 속한 개수를 나타내고 있다.
# 1집단은 38, 2집단은 50, 3집단은 62개가 속해 있음을 알 수 있다.
# 다음으로 분류가 잘 되었는지 알아보기 위해 품종별로 군집화된 결과를 확인하였다.



setosa <- x[1:50,]
versicolor <- x[51:100,]
virginica <- x[101:150,]
table(setosa)
table(versicolor)
table(virginica)


#실행 결과 setosa의 경우 확실하게 군집화가 되었으며 versicolor는 3집단에 48개가 속하였으나,
#virginica의 경우 1집단과 3집단으로 나눠져 확실하게 어느 한 집단으로 군집화 되지 않았다.
#위의 프로그램을 실행했을때 나오는 결과가 가끔 많은 차이를 보일때가 있다.
#알고리즘에 이상은 없어보이며 아마도 step 1에서 결정된 군집중심이
#매번 달라짐에 따라 결과값이 달라지는 듯 하다.
#R에 내장된 kmeans함수를 이용했을때 또한 결과가 매번 달라짐을 확인할 수 있었다.


A = matrix(c(22,21, 
             19,20, 
             18,22, 
             1,3, 
             3,2, 
             55,3, 
             51,44, 
             3,56, 
             30,32, 
             3,12, 
             75,12), ncol=2, byrow=T); A
kmeans(A, 3)
KmeansClustering(A,3)


encounterLogs = read.csv(file='D:/Desktop/Dropbox/R Code/EncounterLog.csv', sep=',', header=T)
DataSet = as.matrix(encounterLogs[,-c(1:4)])
summary(encounterLogs)
kmeans(DataSet, 6)
KmeansClustering(DataSet, 6)

###### Cluster analysis in R: determine the optimal number of clusters
###### http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
n = 100
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
## 1-approach
plot(d)
mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(mydata,
                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## 2-approach
install.packages('cluster');require(cluster);
install.packages('fpc');require(fpc);
pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(d, pamk.best$nc))

## 2-1-approach
library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(d, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
# still 4

## 3-approach
install.packages('vegan'); require(vegan);
fit <- cascadeKM(scale(d, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 5 clusters!

## 4-approach
# See http://www.jstatsoft.org/v18/i06/paper
# http://www.stat.washington.edu/fraley/mclust/tr504.pdf
#
library(mclust)
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
d_clust <- Mclust(as.matrix(d), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
plot(d_clust)

## 5-approach
install.packages('apcluster'); library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), d)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
heatmap(d.apclus)
plot(d.apclus, d)
################################################################
