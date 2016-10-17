panel.splot <- function(y)
{
  function(x,yv=y)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 2, usr[3:4]), new = TRUE)
    plot(yv,x)
  }
}
pairs(iris[,1:4],diag.panel=panel.splot(iris[,5]))