## User, Movie, and Rating Information
u.data = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u.data", header=F)
summary(u.data)
colnames(u.data)= c("user id", "item id", "rating", "timestamp")
head(u.data)
tail(u.data)

## Training set and Test set
D:\Data Storage\CF 측정을 위한 추천 데이터\movie lens data(100k)

u1.base = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u1.base", header=F)
u1.test = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u1.test", header=F)
u2.base = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u2.base", header=F)
u2.test = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u2.test", header=F)
u3.base = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u3.base", header=F)
u3.test = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u3.test", header=F)
u4.base = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u4.base", header=F)
u4.test = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u4.test", header=F)
u5.base = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u5.base", header=F)
u5.test = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u5.test", header=F)



## Movie Information
u.item = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u.item",  quote="\"", sep="|", header=F)
summary(u.item)
colnames(u.item)= c("movie id", "movie title", "release date",
                   "video release date", "IMDb URL",
                   "unknown", "Action", "Adventure",
                   "Animation", "Children's", "Comedy",
                   "Crime", "Documentary", "Drama", "Fantasy",
                   "Film-Noir", "Horror", "Musical", "Mystery",
                   "Romance", "Sci-Fi", "Thriller", "War", "Western")
head(u.item)
tail(u.item)

## Demographic information
u.user = read.table("D:/Data Storage/CF 측정을 위한 추천 데이터/movie lens data(100k)/u.user",  quote="\"", sep="|", header=F)
summary(u.user)
colnames(u.user)= c("user id", "age", "gender", "occupation", "zip code")
head(u.user)
tail(u.user)

######### K-means test
DataSet <- as.matrix(u.data[, -4])
x <- KmeansClustering(DataSet, 3)
kmeans(DataSet, 3)

DataSet <- as.matrix(u1.base[, -4])
x <- KmeansClustering(DataSet, 3)
kmeans(DataSet, 3)


