df <- df %>% mutate(amount=as.numeric(amount))%>%
  mutate(priceUSD = case_when(currency == "CHF"~ amount * 1.03,
                              currency == "EUR"~ amount * 1.07,
                              currency == "DKK"~ amount * 0.14,
                              currency == "Â£" ~ amount * 1.26))%>%
  select(-Price,-currency,amount)%>%
  filter(!is.na(Type) & !is.na(Length) & !is.na(Width) & !is.na(Location) & `Year Built` !=0)



df %>% arrange(desc(priceUSD))%>%
  vis_miss()
summary(df$`Year Built`)

df <- df %>% mutate(Year_built_Type = case_when(
  `Year Built` <= 1915 ~ "Ancient",
  `Year Built` <= 1945 ~ "Old",
  `Year Built` <= 1975 ~ "Classics",
  `Year Built` <= 2005 ~ "Current",
  `Year Built` <= 2035 ~ "New"))

df %>% group_by(`Year Built`)%>% 
  summarize(total_views = sum(`Number of views last 7 days`))%>% 
  ggplot(aes(x=`Year Built`,y=`total_views`,size = total_views))+
  geom_point(aes(color=total_views))+
  scale_colour_gradient(low = "#6699ff", high = "black")+
  scale_y_log10()+
  geom_smooth(method='lm',color='black')+
  scale_x_continuous(breaks= round(seq(min(df$`Year Built`), max(df$`Year Built`), by = 15)))+
  ylab(" Total Views in 7 Days by Year Built ")+
  ggtitle("Total Views in 7 Days by Year Built \n VS. \n Year Built")+
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))

total_view <- sum(df$`Number of views last 7 days`)

df %>% group_by(Year_built_Type)%>% 
  summarize(view_proportion = sum(`Number of views last 7 days`)/total_view)%>% 
  ggplot(aes(x=factor(Year_built_Type,levels = c("Ancient","Old","Classics","Current","New")),y=view_proportion))+
  geom_bar(stat = 'identity', width=0.7,fill= "#6E8DAB")+
  geom_text(aes(label = paste(round(view_proportion*100,2),"%")),hjust = -0.3)+
  ylim(0,0.6)+
  xlab("")+
  ylab("Proportion")+
  geom_vline(xintercept = 3.5,linetype="dotted",size = 2, color = "grey")+
  ggtitle("Proportion of Views \n for Each Built-Time Category")+
  annotate("text", x = 4, y = 0.3, label = "Boats Built \n 1975 - 2005", color = "white",size = 4)+
  annotate("text", x = 5, y = 0.3, label = "Boats Built \n 2005 - 2022", color = "white",size = 4)+
  annotate("text", x = 4, y = 0.55, label = "Make up 94% of \n the total views", color = "#666699",size = 6)+
  coord_flip()+
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))


summary(df$priceUSD)

unique(df$`Boat Type`)
unique(df$`Year Built`)
str(df)

df <- df %>% mutate(size = Length * Width) %>% arrange(desc(size))
summary(df$size)

ggplot(df,aes(x=size))+
  geom_boxplot()
  
ggplot(df,aes(x=size))+
  geom_histogram(bins=50)+
  theme_classic()

df <- df %>% filter(size > 1 & size <= 400)

summary(df$size)

df <- df %>% mutate(Size_Type = case_when(
                                          size <= 20 ~ "Small",
                                          size <= 35 ~ "Median",
                                          size <= 60 ~ "Large",
                                          size <= 100 ~ "Huge",
                                          size <= 400 ~ "Luxury"))




df %>% group_by(Size_Type)%>% 
  summarize(view_proportion = sum(`Number of views last 7 days`)/total_view)%>% 
  ggplot(aes(x=factor(Size_Type,levels = c("Luxury","Huge","Large","Median","Small")),y=view_proportion))+
  geom_bar(stat = 'identity',width=0.7,fill= "#6E8DAB")+
  geom_text(aes(label = paste(round(view_proportion*100,2),"%")),hjust = -0.3)+
  ylim(0,0.5)+
  xlab("")+
  ylab("Proportion")+
  geom_vline(xintercept = 3.5,linetype="dotted",size = 2, color = "grey")+
  annotate("text", x = 4, y = 0.2, label = "Boats size \n smaller than 20 m^2", color = "white",size = 4)+
  annotate("text", x = 5, y = 0.28, label = "Boats Built \n smaller than 35 m^2", color = "white",size = 4)+
  annotate("text", x = 4, y = 0.4, label = "Make up 60% of \n the total views", color = "#666699",size = 6)+
  ggtitle("Proportion of Views \n for Each Boat Size Category")+
  coord_flip()+ 
  theme_classic()+
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))
