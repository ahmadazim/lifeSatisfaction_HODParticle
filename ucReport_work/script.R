d = read.csv('/Users/ahmadazim/Documents/Harvard/Clubs/HODP/lifeSat/spring2021.csv')

q98 = d$How.satisfied.have.you.been.with.your.social.life.as.a.Harvard.College.student.during.the.2020.21.academic.year.compared.to.past.academic.years.
q98_df = data.frame(table(q98))
q98_df = q98_df[q98_df$Freq>1 & q98_df$q98 != '',]
data = q98_df
colnames(data) = c('category', 'count')
primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB')

data$category = c('1-Extremely dissatisfied',            
                  '5-Extremely satisfied',               
                  '3-Neither satisfied nor dissatisfied',
                  '2-Somewhat dissatisfied',             
                  '4-Somewhat satisfied')
pltSocial = ggplot(data=data, aes(x="", y=count, fill = category)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')) +
  labs(title = 'How satisfied have you been with your social life during the 2020-21 \nacademic year compared to past academic years?') + 
  theme_void() +
  theme(plot.title = element_text(size=13, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=0, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica")
  )
ggsave('./satisfiedSocialLife.png', plot = pltSocial) 


num7 = read.csv('./num7.csv')
satisfied = ggplot(data = num7, aes(x = Category, y = value, fill = Season, width = 0.7))+
  geom_bar(position="dodge", stat="identity")+
  ylab('Rating (0-10)')+
  xlab('')+
  ggtitle('On a scale from 0 (extremely dissatisfied) to 10 \n(satisfied), how satisfied are you with the following: ')+
  coord_flip()+
  theme_hodp()+
  theme(plot.title = element_text(size=13, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=0, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica")
  )
ggsave('./satisfied10_fallSpring.png', plot = satisfied) 


