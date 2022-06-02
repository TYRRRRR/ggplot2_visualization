library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

df <- read_csv("which cities have the most CCTV cameras.csv")
str(df)

cctvChina <- df %>% filter(Country=="China")%>% 
  rename(Numofcam = `# of CCTV Cameras`,Numofppl = `# of People`, numofCam1000 = `# of CCTV Cameras per 1,000 People`,
         camperSM = `Cameras per Square Mile`, crimdex = `Crime Index`)


top10 <- cctvChina%>%
  arrange(desc(numofCam1000))%>%
  top_n(8,numofCam1000)

top10


cctvChina <- cctvChina %>%
  filter(City %in% top10$City)

cctvChina


p <- ggplot() +
  geom_bar(data = cctvChina,aes(x= factor(City),y= numofCam1000),stat = 'identity', fill = 'red') +
  geom_point(data = cctvChina, aes(x= factor(City),y= crimdex))

p

