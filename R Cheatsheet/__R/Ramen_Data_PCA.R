data <- read.table("D:/Desktop/Dropbox/R Code/__R/Ramen_Data_PCA.txt", header=T, fileEncoding="UTF-8")
data;
?cor
cor(data)
round(cor(data),2)
p1 = prcomp(data, scale=TRUE);
print(p1)
summary(p1)
predict(p1)
round(predict(p1), 2)
pdf("plot2.pdf", family="Korea1deb");
biplot(p1)
dev.off()
