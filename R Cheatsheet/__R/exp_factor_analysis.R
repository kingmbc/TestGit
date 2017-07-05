# read in the first worksheet from the workbook myexcel.xlsx
# first row contains variable names
library(xlsx)
mydata <- read.xlsx("c:/myexcel.xlsx", 1)

# read in the worksheet named mysheet
mydata <- read.xlsx("c:/myexcel.xlsx", sheetName = "mysheet")



install.packages("psych")
require(psych)

data(bfi)
describe(bfi)
fa.parallel(bfi)

ml5.out <- factanal(covmat = cor(bfi, use = "complete.obs"), factors = 5, rotation = "none")
ml5.out
print(ml5.out, digits=2, cutoff=.3, sort=TRUE)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation
fit <- factanal(mydata, 3, rotation="varimax")

print(fit, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2 

load <- fit$loadings[,1:2] 

plot(load,type="n")                                 # set up plot 

text(load,labels=names(mydata),cex=.7)           # add variable names 