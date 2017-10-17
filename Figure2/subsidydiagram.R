library(ggplot2)
p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))



#Two members and 4 years
fun.1 <- function(x) {
  
  
  asignacion=0
  bonobase=0
  suf=0
  #First, bono de asignacion social
  
  asignacion=7500*2*(x<2.515)+ 
            6000*2*(x<3.207)*(x>2.515)+
            4500*2*(x<4.213)*(x>3.207)
  
  bonobase=7170*2*(x<1.515)*0
  
  suf=7170*2*(x<11.734)
  #total subsidies
  total=suf+bonobase+asignacion
  return(total)
}



#four members and 1 month
fun.2 <- function(x) {
  
  
  asignacion=0
  bonobase=0
  suf=0
  #First, bono de asignacion social
  
  asignacion=7500*4*(x<2.515)+ 
    6000*4*(x<3.207)*(x>2.515)+
    4500*4*(x<4.213)*(x>3.207)
  
  bonobase=13591*4*(x<1.515)*0
  
  suf=7170*4*(x<11.734)
  #total subsidies
  total=suf+bonobase+asignacion
  return(total)
}


p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p<-p + stat_function(fun = fun.1,color="blue",size=1) +  stat_function(fun = fun.2,color="red",size=1,linetype = 2)+xlim(0,20)
p<-p+labs(title="",x="Social Protection Card Score", y="Monthly Transfer (CLP)")
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.title=element_text(size=rel(0)),
  panel.background = element_rect(fill = "white"),
  panel.grid.minor = element_line(linetype = "dotted",color="black"),
  panel.border = element_rect(fill = NA, colour = "black", size = 2)
  )

setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/subsidyschedule")
p
dev.set()
png(file="Subsidies.png")
p
dev.off()



#Using only the blue line



p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p<-p + stat_function(fun = fun.1,color="blue",size=1) +xlim(0,20)
p<-p+labs(title="",x="Social Protection Card Score", y="Monthly Transfer (CLP)")
p<-p + theme(
  plot.title = element_text(size = rel(2)),
  axis.title=element_text(size = rel(2)),
  axis.text.x=element_text(size = rel(2)),
  axis.text.y=element_text(size = rel(2)),
  legend.text=element_text(size = rel(2)),
  legend.title=element_text(size=rel(0)),
  panel.background = element_rect(fill = "white"),
  panel.grid.minor = element_line(linetype = "dotted",color="black"),
  panel.border = element_rect(fill = NA, colour = "black", size = 2)
)

setwd("/Users/rodrigoazuero/Dropbox/BACKUPRODRIGO/Research/Chile/Figures/subsidyschedule")
p
dev.set()
png(file="Subsidies2.png")
p
dev.off()



