#UC Survey Data Analysis

fall = read.csv("Fall copy.csv", header = TRUE)
spring = read.csv("Spring.csv", header = TRUE)


#### Most Recent Debate ####
# Step 0: HODP Theme
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('hrbrthemes')) install.packages('hrbrthemes'); library(hrbrthemes)
if (!require('magick')) install.packages('magick'); library(magick)
if (!require('plotly')) install.packages('plotly'); library(plotly)
logo <- image_read("logo.png")
# Legend: https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot

monochrome <- c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')
primary <- c('#EE3838', '#FA9E1C', '#78C4D4', '#4B5973', '#E2DDDB')
sidebysidebarplot <- c("#ef3e3e", "#2c3e50")
theme_hodp <- function () { 
  theme_classic(base_size=12, base_family="Helvetica") %+replace%
    theme(
      panel.background  = element_rect(fill="#F2F2F2", colour=NA),
      plot.background = element_rect(fill="#F2F2F2", colour="#d3d3d3"),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA),
      plot.title = element_text(size=24,  family="Helvetica", face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.subtitle = element_text(size=18,  family="Helvetica", color="#717171", face = "italic", margin = margin(t = 0, r = 0, b = 10, l = 0)),
      plot.caption = element_text(size=8,  family="Helvetica", hjust = 1),
      axis.text.x =element_text(size=10,  family="Helvetica"),
      axis.title.x =element_text(size=14, family="Helvetica", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, family="Helvetica", angle=90, face ='bold'),
      legend.title=element_text(size=10, family="Helvetica"), 
      legend.text=element_text(size=10, family="Helvetica"),
      legend.position = "bottom",
      axis.ticks = element_blank()
    )
}
img = image_read("logo.png")

table(fall$How.satisfied.are.you.with.Harvard.s.Spring.2021.reopening.so.far.)
plot(table(fall$How.satisfied.are.you.with.Harvard.s.Spring.2021.reopening.so.far.))

q1 <- table(fall$How.satisfied.are.you.with.Harvard.s.Spring.2021.reopening.so.far.)
q1 <- data.frame(level = c("Very satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Very dissatisfied"),value = c(52, 287, 92, 135, 72))

ggplot(data = q1, aes(x=level, y=value)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "Satisfaction with Fall Reopening") + 
  ylab("Number of Students") +
  xlab("Level of Satisfaction") +
  theme_hodp() 

fall_q1 <- ggplot(data = q1, aes(x=level, y=value)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "Satisfaction with Fall Reopening") + 
  ylab("Number of Students") +
  xlab("Level of Satisfaction") +
  theme_hodp() 
fall_q1

# Add logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(2, 'cm'))

# Interactive Graphic
ggplotly(fall_q1)

table(spring$How.satisfied.are.you.with.Harvard.s.Spring.2021.semester.so.far.)
plot(table(spring$How.satisfied.are.you.with.Harvard.s.Spring.2021.semester.so.far.))

q1.2.t <-subset(spring, spring$Where.are.you.currently.living. == "On-campus")

q1.s <- table(spring$How.satisfied.are.you.with.Harvard.s.Spring.2021.semester.so.far.[spring$Where.are.you.currently.living.=='On-campus'])
q1.s <- data.frame(level = c("Very satisfied", "Somewhat satisfied", "Neither satisfied nor dissatisfied", "Somewhat dissatisfied", "Very dissatisfied"),value = c(12, 88, 24, 17, 10))

spring_q1 <- ggplot(data = q1.s, aes(x=level, y=value)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#FA9E1C') +
  coord_flip() +
  labs(title= "Satisfaction with Spring Semester") + 
  ylab("Number of Students") +
  xlab("Level of Satisfaction") +
  theme_hodp() +
  theme(legend.title = element_blank())
spring_q1

# Add logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(2, 'cm'))

# Interactive Graphic
ggplotly(spring_q1)

ultimate_q1 <- data.frame(Level = c("5- Very satisfied", "4- Somewhat satisfied", "3- Neither satisfied nor dissatisfied", "2- Somewhat dissatisfied", "1- Very dissatisfied"), c = c(0,0,0,0,0),Fall = c(52/638, 287/638, 92/638, 135/638, 72/638), Spring = c(30/498, 199/498, 93/498, 111/498, 65/498))
library(reshape2)
ultimate_q1 <- melt(ultimate_q1)
names(ultimate_q1)[3] <- "percent"

ult_q1_plot <- ggplot(data = ultimate_q1, aes(y=percent, x=variable, fill = Level)) +
  geom_bar(stat = "identity", width = .85, position = 'stack') +
  coord_flip() +
  labs(title= "Satisfaction with Spring Semester" ) + 
  ylab("Percentage of student respondents") +
  xlab("") +
  theme_void() +
  guides(fill= guide_legend(ncol=2))+
  coord_polar(theta = 'y')+
  scale_fill_manual(values = rev(c("#4B5973", "#78C4D4", "#E2DDDB", "#FF9586", "#BE1E26")))+
  theme(legend.title = element_blank(),legend.position = "bottom")+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))
ult_q1_plot

ggplotly(ult_q1_plot)

# doing the same thing split by on or off campus
oncampus <- data.frame(Level = c("5- Very satisfied", "4- Somewhat satisfied", "3- Neither satisfied nor dissatisfied", "2- Somewhat dissatisfied", "1- Very dissatisfied"), Fall = c(26/198, 117/198, 12/198, 31/198, 12/198), Spring = c(12/151, 88/151, 24/151, 17/151, 10/151))
library(reshape2)
oncampus <- melt(oncampus)
names(oncampus)[3] <- "percent"

oncam <- ggplot(data = oncampus, aes(x=Level, y=percent*100, fill = variable)) +
  geom_bar(stat = "identity", width = .5, position = 'dodge') +
  coord_flip() +
  labs(title= "On-campus") + 
  ylab("Percentage of student respondents") +
  xlab("") +
  theme_hodp() +
  theme(legend.title = element_blank())
oncam

offcampus <- data.frame(Level = c("5- Very satisfied", "4- Somewhat satisfied", "3- Neither satisfied nor dissatisfied", "2- Somewhat dissatisfied", "1- Very dissatisfied"), Fall = c(26/440, 170/440, 80/440, 104/440, 60/440), Spring = c(18/347, 111/347, 69/347, 94/347, 55/347))
library(reshape2)
offcampus <- melt(offcampus)
names(offcampus)[3] <- "percent"

offcam <- ggplot(data = offcampus, aes(x=Level, y=percent*100, fill = variable)) +
  geom_bar(stat = "identity", width = .5, position = 'dodge') +
  coord_flip() +
  labs(title= "Off-campus") + 
  ylab("Percentage of student respondents") +
  xlab("") +
  theme_hodp() +
  theme(legend.title = element_blank())
offcam

grid.arrange(oncam,offcam, ncol=2)



#continue the 5000 allowance for COVID(essentially a 5000 discount)
table(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Give.a..5.000.COVID.19.allowance.grant.to.all.students..including.those.living.on.campus..not.just.remote.students.)
allowance_mean <- mean(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Give.a..5.000.COVID.19.allowance.grant.to.all.students..including.those.living.on.campus..not.just.remote.students., na.rm = T)
median(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Give.a..5.000.COVID.19.allowance.grant.to.all.students..including.those.living.on.campus..not.just.remote.students., na.rm = T)
allowance <- data.frame(level = 1:10, proportion = c(10/455, 6/455, 23/455, 6/455, 47/455, 27/455, 33/455, 33/455, 18/455, 252/455))
allowance_plot <- ggplot(data = allowance, aes(x=level, y=proportion)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#4B5973') +
  labs(title= "Continue $5,000 Allowance") + 
  ylab("Proportion of Respondents") +
  xlab("Level of Approval") +
  theme_hodp() 
allowance_plot

#invite all undergrads back to housing in fall 2021
table(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Invite.all.undergraduates.back.to.on.campus.housing)
oncampus_mean <- mean(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Invite.all.undergraduates.back.to.on.campus.housing, na.rm = T)
median(spring$To.what.extent.do.you.approve.or.disapprove.of.the.following.potential.policies.for.the.Fall.2021.semester....Invite.all.undergraduates.back.to.on.campus.housing, na.rm = T)
oncampus <- data.frame(level = 1:10, proportion = c(1/485, 2/485, 5/485, 5/485, 18/485, 6/485, 28/485, 48/485, 33/485, 339/485))
oncampus_plot <- ggplot(data = oncampus, aes(x=level, y=proportion)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#4B5973') +
  labs(title= "Invite All Undergrads to Campus") + 
  ylab("Proportion of Respondents") +
  xlab("Level of Approval") +
  theme_hodp() 
oncampus_plot

#CONTINUE THIS FOR ALL THE 1-10 QUESTIONS REGARDING FALL 2021

#######

table(spring$How.satisfied.are.you.with.the.education.you.have.received.at.Harvard.during.the.current.2020.21.academic.year.compared.to.prior.academic.years.)
edu_qual <- data.frame(Satisfaction = c("5- Extremely satisfied", "4- Somewhat satisfied", "3- Neither satisfied nor dissatisfied", "2- Somewhat dissatisfied", "1- Extremely dissatisfied"),value = c(24, 104, 41, 107, 54))

ggplot(data = edu_qual, aes(x=Satisfaction, y=value)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "Satisfaction with Education Overall") + 
  ylab("Number of Students") +
  xlab("Level of Satisfaction") +
  theme_hodp() 

edu_qual_pie <- ggplot(data=edu_qual, aes(x="", y=value, fill = Satisfaction)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar("y") +
  theme(plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=10, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica"),
        legend.position = "bottom",
  ) + 
  scale_fill_manual(values = c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')) +
  labs(title= "Satisfaction with 20-21 Education") + 
  theme_void() +
  theme(plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
             legend.title=element_text(size=10, family="Helvetica"), 
             legend.text=element_text(size=10, family="Helvetica")
  )

edu_qual_pie


##student wellbeing

table(spring$During.the.past.two.weeks..how.often..if.ever..have.you....Felt.so.depressed.it.was.difficult.to.function)
plot(table(spring$During.the.past.two.weeks..how.often..if.ever..have.you....Felt.so.depressed.it.was.difficult.to.function))

question <- c(rep("sad",5), rep("depressed", 5))
level <- rep(c("Never", "Rarely", "Sometimes", "Often", "Very often"), 5)
value<- abs(rnorm(10, 25, 12))
data <- data.frame(question, level, value)

ggplot(data, aes(fill = level, y = value, x = question)) +
  geom_bar(position="dodge", stat="identity")

twoweeks <- read.csv("twoweeks.csv", header = T)
twoweeks_stacked <- ggplot(data = twoweeks, aes(x=Experience, y = (Number.of.students/474)*100, fill = forcats::fct_rev(Severity))) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c('#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586'),labels = c("Very often", "Often", "Sometimes", "Rarely", "Never")) +
  ylab("Percentage of student respondents")+
  xlab("")+
  labs(title= " ", fill = "") + 
  guides(fill = guide_legend(reverse = TRUE))+
  coord_polar()+
  theme_hodp()
twoweeks_stacked
RHODP::add_HODP_logo()
png("stacked.png", width = 3000, height = 1890, res = 300)
twoweeks_stacked
dev.off()

####
#ON A SCALE FROM 0 (Extremely dissatisfied) to 10 (extremely satisfied): during the Spring 2021 semester

semester_opinions <- read.csv("Spring2021opinions.csv", header = T)
#for (i in nrow(semester_opinions)){
  #semester_opinions$Question[i] <- stringr::str_replace(semester_opinions$Question[i], pattern = "\\\\n", '\n')
#}

library(stringr)
semester_opinions$mark <- ifelse(semester_opinions$Mean.Rating < 0, "below", "above") #above/below 5
semester_opinions <- semester_opinions[order(semester_opinions$Mean.Rating),] #sort
semester_opinions$Question <- str_wrap(semester_opinions$Question, width = 80)
semester_opinions$Question <- factor(semester_opinions$Question, levels = semester_opinions$Question)  # convert to factor to retain sorted order in plot.


opi <- ggplot(semester_opinions, aes(x=Question, y = Mean.Rating, label = Mean.Rating)) +
  geom_bar(stat = 'identity', aes(fill=mark), width=0.5) +
  scale_fill_manual(name = "Mean rating",
                    labels = c("Satisfied", "Dissatisfied"),
                    values = c("above" = "#00ba38", "below" = "#f8766d")) +
  labs(subtitle = "Mean ratings of the Spring 2021 semester",
       title="Overall Sentiments") +
  coord_flip()+
  theme_hodp()+
  ylab("Mean Rating (centered)")
opi

semester_opinions

##HOURS ON ACTIVITES BTWN FALL AND SPRING

#create new data without ppl who enter zeros

actives <- read.csv("activity_csv.csv", header = T)
actives.spring <- read.csv("spring_active.csv", header = T)

median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.coursework.and.required.academic.activities, na.rm = T)
median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.extracurriculars, na.rm = T)
median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Exercise, na.rm = T)
median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.socializing, na.rm = T)
median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....In.person.socializing, na.rm = T)
median(actives$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Social.media, na.rm = T)

median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.coursework.and.required.academic.activities, na.rm = T)
median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.extracurriculars, na.rm = T)
median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Exercise, na.rm = T)
median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Remote.socializing, na.rm = T)
median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....In.person.socializing, na.rm = T)
median(actives.spring$Roughly.how.many.hours.per.week.have.you.spent.on.the.following.activities.during.this.semester....Social.media, na.rm = T)

active_hrs_data <- read.csv("act_hours.csv", header = T)

specie <- active_hrs_data$Activity
Semester <- active_hrs_data$Semester
value <- active_hrs_data$Median.Hours.per.Week
df <- data.frame(specie, Semester, value)






acthours <- ggplot(data = df, aes(fill=Semester, y=value, x=reorder(specie, -value))) +
  geom_bar(stat = "identity", width = .8, position = 'dodge') +
  labs(title= "Hours Spent on Activities Per Week") + 
  ylab("Median Hours Spent Per Week") +
  xlab("Activity") +
  theme_hodp() +
  theme(axis.text.x = element_text(angle = -60, vjust = 0.2, hjust=0.2)) 
acthours


##WILL YOU ENROLL FOR FALL

table(spring$Do.you.plan.to.enroll.at.Harvard.College.in.the.Fall.2021.semester.)
returning <- data.frame(Plan = c("5- Definitely yes", "4- Probably yes", "3- Might or might not", "2- Probably not", "1- Definitely not"),value = c(256, 125, 61, 4, 14))

return_or_no <- ggplot(data = returning, aes(x=Plan, y=value/460)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "Do you plan to enroll in Fall 2021?") + 
  ylab("Percentage of student respondents") +
  xlab("") +
  theme_hodp() 
return_or_no

##LIVING PREFERENCES FOR FALL 2021
table(spring$If.you.plan.to.enroll.at.Harvard.College.in.the.Fall.2021.semester..where.do.you.prefer.to.live.)


vector <- c('On-campus' = 95, 'Off-campus' = 3, 'No preference' = 2)
waffle <- waffle(vector, rows = 10, size = 0.9,
       colors = c('#FF9586', '#D84742', '#760000'),
       title = "Living Preferences for Fall 2021",
       reverse = T)
waffle


table(spring$When.is.the.latest.date.that.you.believe.Harvard.should.release.its.decision.about.the.Fall.2021.term.by.to.give.students.adequate.time.to.make.plans....Selected.Choice)
plot(table(spring$When.is.the.latest.date.that.you.believe.Harvard.should.release.its.decision.about.the.Fall.2021.term.by.to.give.students.adequate.time.to.make.plans....Selected.Choice))

##WHEN SHOULD HARVARD RELEASE A DECISION BY
death_data <- data.frame(Date = c("April 30", "May 15", "May 31", "June 15", "June 30", "July 15"),value = c(74, 71, 128, 107, 64, 42))

fallplan_release <- ggplot(data = death_data, aes(x=reorder(Date, value), y=(value/486)*100)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "When Should Fall Plans Be Released By?") + 
  ylab("Perecentage of student respondents") +
  xlab("Date") +
  theme_hodp() 
fallplan_release

deathday <- ggplot(data=death_data, aes(x="", y=value, fill = Date)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar("y") +
  theme(plot.title = element_text(size=20,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title=element_text(size=10, family="Helvetica"), 
        legend.text=element_text(size=10, family="Helvetica"),
        legend.position = "bottom",
  ) + 
  scale_fill_manual(values = c('red','#760000', '#BE1E26', '#D84742', '#FF6B61', '#FF9586')) +
  labs(title= "When Should Fall Plans Be Released?") + 
  theme_void() +
  theme_hodp() 
deathday



##STUDENT RECOMMENDATIONS

recommend <- read.csv("recommend.csv", header = T)

mean(recommend$Give.a..5.000.COVID.19.allowance.grant.to.all.students..including.those.living.on.campus..not.just.remote.students., na.rm = T)

values1 <- as.numeric(colMeans(recommend, na.rm = T))
name <- colnames(recommend)
rec_df <- data.frame(name, values1)

recs <- ggplot(data = rec_df, aes(x=reorder(name, values1), y=values1)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = '#EE3838') +
  coord_flip() +
  labs(title= "What Policies Should Be Enacted for Fall 2021?") + 
  ylab("Mean Rating By Students (0-10)") +
  xlab("") +
  theme_hodp() 
recs



##IF YOU LIVED OFF CAMPUS DO U LIKE IT BETTER OR NO

table(spring$If.you.lived.off.campus.last.semester..do.you.prefer.living.off.campus.or.on.campus.)
table(spring$If.you.lived.on.campus.last.semester..do.you.prefer.living.off.campus.or.on.campus.)

life <- read.csv("onOroff.csv", header = T)

vector1 <- c('On-campus' = 72, 'Off-campus' = 16, 'No preference' = 12)
waffle2 <- waffle(vector1, rows = 10, size = 0.9,
                 colors = c('#FF9586', '#D84742', '#760000'),
                 title = "Preferences of students that were both on and off-campus in the 20-21 school year",
                 reverse = T)
waffle2

##how satisfied are you with your social life
#living preferences for all students

table(spring$How.satisfied.have.you.been.with.your.social.life.as.a.Harvard.College.student.during.the.2020.21.academic.year.compared.to.past.academic.years.)

vector5 <- c('Dissatisfied' = 78,'Neutral' = 9, 'Satisfied' = 13)
waffle12 <- waffle(vector5, rows = 10, size = 0.9,
                   colors = c('#760000', "#D84742",'#FF9586'),
                   title = "Social Life Satisfaction",
                   reverse = F)
waffle12


vector1 <- c('Off-campus' = 3, 'No preference' = 2,'On-campus' = 95)
waffle2 <- waffle(vector1, rows = 10, size = 0.9,
                 colors = c('#760000', "#D84742",'#FF9586'),
                 title = "Living Preferences for Fall",
                 reverse = F)
waffle2
