# functions
base = function(){
  plot(0,0, xlim=c(-3,3), ylim=c(-3,3), type="n")
  grid(col = "black")
  sapply(p[1:3], function(i) points(x=i[1], y=i[2], pch=15))
  sapply(p[4:6], function(i) points(x=i[1], y=i[2], pch=19))
}
toABline = function(w){
  abline(a = -w[3]/w[2], b = -w[1]/w[2])
}
whichWrong = function(w){
  z = sapply(p, function(i) {w[1]*i[1] + w[2]*i[2] + w[3]})
  g = sapply(1:length(z), function(i) {ifelse(i<=3, z[i]>=0, z[i]<0)})
  ww = which(g==FALSE)
  if(length(ww)==0)
    return(NULL)
  return(ww[1])
}
newLine = function(w, learning_rate){
  ww = whichWrong(w)
  if(is.null(ww))
    return(NULL)
  correct_label = ifelse(ww<=3, 1, -1)
  return(w + learning_rate*correct_label*c(p[[ww]], 1))
}


# settings
learning_rate = 0.1
p = list(c(-1,2), c(1,1.5), c(2,-0.5),
         c(1,-2), c(-1,-2), c(-3,0))

# run
par(mfrow=c(3,3))
base()
w = c(1,-1,0)
toABline(w)
while(TRUE){
  w = newLine(w, learning_rate)
  if(is.null(w)){
    print("every point is on the correct side")
    break;
  }
  base()
  toABline(w)
}




