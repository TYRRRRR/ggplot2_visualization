library(tidyverse)

coin <- c(3,2)
res <- NULL
set.seed(sample(rnorm(1000,0,40),1))
for(i in 1:6){
gua <- sample(coin,3,replace = TRUE,prob = c(0.5,0.5))
res <- rbind(res,gua)
}
rownames(res) <- paste('GUA',1:6,sep='')
df <- as.data.frame(res)
df$rowSum <- rowSums(df)
df$yingyang <- ifelse(df$rowSum == 6 | df$rowSum == 8,"ying","yang")
df$yvalue <- seq(1,5,length=6)
df$yingyang

P <- ggplot(data = df,aes(x=seq(2,4,length=6),y=yvalue)) +
   geom_point(size = 0)

for(i in 1:6){
  col<- "black"
  if(i > 3){
    col <- "red"
  }
  if (df$yingyang[i]=="ying"){
    P <- P + geom_segment(x = 2, y = df$yvalue[i], xend = 2.8,yend = df$yvalue[i],size=7,colour = col)+
      geom_segment(x = 3.2, y = df$yvalue[i], xend = 4,yend = df$yvalue[i],size=7,colour = col)
  } else {
    P <- P + geom_segment(x = 2, y = df$yvalue[i], xend = 4,yend = df$yvalue[i],size=7,colour = col)
  }
}

P <- P + xlim(0, 6) + ylim(-1, 7) +
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

P
