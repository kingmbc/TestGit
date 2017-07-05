# 3D Non-centrality Probability Distribution
tProb3D <- function()
{ tt <- seq(0,10,len=21)
  ncp <- seq(0,6,len=31)
  ptn <- outer(tt,ncp, function(t,d) pt(t, df = 3, ncp=d))
  t.tit <- "Non-Central t - Probabilities"
  persp(tt,ncp,ptn, zlim=0:1, r=2, phi=20, theta=200, main=t.tit,
      xlab = "t", ylab = "Non-centrality Parameter",
      zlab = "Pr(T <= t)",expand = 0.5, col = "cyan",
      ltheta = 120, shade = 0.5, ticktype = "detailed")
}
