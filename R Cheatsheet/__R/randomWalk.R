#http://www.r-bloggers.com/a-plot-of-250-random-walks/

# Generate k random walks across time {0, 1, ... , T}
T <- 100
k <- 250
initial.value <- 10
GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(rnorm(T)))
}
# Matrix of random walks
values <- replicate(k, GetRandomWalk())
# Create an empty plot
dev.new(height=8, width=12)
plot(0:T, rep(NA, T + 1), main=sprintf("%s Random Walks", k),
     xlab="time", ylab="value",
     ylim=10 + 4.5 * c(-1, 1) * sqrt(T))
mtext(sprintf("%s%s} with initial value of %s",
              "Across time {0, 1, ... , ", T, initial.value))
for (i in 1:k) {
  lines(0:T, values[ , i], lwd=0.25)
}
for (sign in c(-1, 1)) {
  curve(initial.value + sign * 1.96 * sqrt(x), from=0, to=T,
        n=2*T, col="darkred", lty=2, lwd=1.5, add=TRUE)
}
legend("topright", "1.96 * sqrt(t)",
       bty="n", lwd=1.5, lty=2, col="darkred")
savePlot("random_walks.png")

a = GetRandomWalk();
ma = mean(a);ma;
mad = ma-a;mad;
mean(mad)



library(RCurl)
library(XML)
options(RCurlOptions = list(useragent = "R"))
HEURE=0:23
extracttemp=function(Y,M,D){
  url=paste(
    "http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=QC&StationID=5415&Year=",Y,"&Month=",
    M,"&Day=",D,sep="")
  wp <- getURLContent(url)
  doc <- htmlParse(wp, asText = TRUE) 
  docName(doc) <- url
  tmp <- readHTMLTable(doc)
  basejour=data.frame(Year=Y,Month=M,Day=D,
                      Hour=HEURE,Temp=as.numeric(as.character(data.frame(tmp[2])[,2]))[2:25])
  return(basejour)}
B=NULL
for(y in 1955:2013){
  for(d in 1:31){
    B=rbind(B,extracttemp(y,1,d))}}
