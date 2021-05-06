library(RHODP)
library(ggplot2)
library(tidyverse)
library(alluvial)
library(ggalluvial)
library(reshape2)
library(gridExtra)
library(waffle)


# Load in data
fall = read.csv("fall2020.csv")
spring = read.csv("spring2021.csv")

# alluvial spring satisfaction to prob of enrolling...
colnames(spring)[colnames(spring) == 'Do.you.plan.to.enroll.at.Harvard.College.in.the.Fall.2021.semester.'] = 'enroll'
colnames(spring)[colnames(spring) == 'How.satisfied.are.you.with.Harvard.s.Spring.2021.semester.so.far.'] = 'satisfaction'
colnames(spring)[colnames(spring) == 'What.year.of.college.are.you.in.'] = 'yearClass'


alluvial_satEnroll_df = data.frame(yearClass = spring$yearClass, enroll = spring$enroll, sat = spring$satisfaction)
alluvial_satEnroll_df$yearClass[alluvial_satEnroll_df$yearClass == 'Senior, but graduating later than May 2021'] = "Senior (Graduating later)"
alluvial_satEnroll_df$yearClass[alluvial_satEnroll_df$yearClass == 'Senior, and graduating in May 2021'] = "Senior (Graduating May 2021)"
alluvial_satEnroll_df = alluvial_satEnroll_df[alluvial_satEnroll_df$enroll != '',]
alluvial_satEnroll_df = alluvial_satEnroll_df[alluvial_satEnroll_df$sat != '',]

alluvial_satEnroll_df$tmp = paste0(alluvial_satEnroll_df$yearClass, '_', alluvial_satEnroll_df$enroll,'_', alluvial_satEnroll_df$sat)
alluvial_satEnroll_df = count(alluvial_satEnroll_df, tmp)
alluvial_satEnroll_df = separate(alluvial_satEnroll_df, tmp, c("classYear", "enroll", "sat"), sep="_")

alluvial_satEnroll_df$sat[alluvial_satEnroll_df$sat == 'Neither satisfied nor dissatisfied'] = "Neither satisfied \nnor dissatisfied"
alluvial_satEnroll_df$sat = factor(alluvial_satEnroll_df$sat, levels = c("Very dissatisfied", 
                                                                         "Somewhat dissatisfied",
                                                                         "Neither satisfied \nnor dissatisfied",
                                                                         "Somewhat satisfied",
                                                                         "Very satisfied"))

alluvial_satEnroll_df$enroll = factor(alluvial_satEnroll_df$enroll, levels = c("Definitely not",
                                                                               "Might or might not",
                                                                               "Probably not",
                                                                               "Graduating May 2021",
                                                                               "Probably yes",
                                                                               "Definitely yes"))


alluvialSatPlan = ggplot(alluvial_satEnroll_df[alluvial_satEnroll_df$enroll != 'Graduating May 2021',], 
                         aes(y = n, axis1 = sat, axis2 = enroll)) +
  geom_alluvium(aes(fill = sat), width = 1/20, show.legend = FALSE, alpha = 0.55) +
  geom_stratum(width = 1/12, fill = "gray", color = "black") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_manual(values= c(as.character(RHODP::HODPcols('primary'))[1:4], "#b5c4b5"))+
  ggtitle("How has Spring Satisfaction Affected Students' Fall Plans? (n = 457)")+
  theme_void()+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))+
  theme(plot.margin=unit(c(0,0,0.5,1),"cm"))

png('alluvial_satisfactionPlan.png', width = 11, height = 8, res = 500, units = 'in')
alluvialSatPlan
RHODP::add_HODP_logo(width = 1.5)
dev.off()






# How often have you engaged with the following since the beginning of this semester?
engageRes_spring = spring[,grep(pattern = "How.often.have.you", colnames(spring))]
colnames(engageRes_spring) = unlist(stringr::str_split(colnames(engageRes_spring), pattern = 'semester....'))[seq(2,38,by=2)]
colnames(engageRes_spring) = stringr::str_replace(colnames(engageRes_spring), pattern = "\\.", replacement = ' ')
  #subset...
engageRes_spring = engageRes_spring[,c(1,2,3,4,9,10,12,13,19)]
colnames(engageRes_spring) = c("Remote Classes", "OH (with professors)", "OH (with TFs/CAs)","Academic Resource \nCenter",                    
                               "Extracurriculars", "Club Sports","Research","House Social Events", "Harvard University \nHealth Services")
engageRes_spring[engageRes_spring == "At least once a month" | engageRes_spring == "At least every other week"] = '< 1 day/week'
engageRes_df = data.frame(matrix(0, nrow= 7, ncol= 9))
colnames(engageRes_df) = colnames(engageRes_spring)
rownames(engageRes_df) = unique(engageRes_spring$`Remote Classes`)
for(i in 1:ncol(engageRes_df)){
  x = data.frame(table(engageRes_spring[,i]))
  rownames(x) = x$Var1
  ordered = x[rownames(engageRes_df),]
  ordered$Freq[is.na(ordered$Freq)] = 0
  ordered$prop = ordered$Freq/sum(ordered$Freq)
  engageRes_df[,i] = ordered$prop
}


engageRes_df$Freq = rownames(engageRes_df)
engageRes_df = engageRes_df[engageRes_df$Freq !='',]

engageRes_df$Freq = c("5-6 days/week","Every day","Never","3-4 days/week","< 1 day/week","1-2 days/week")
rownames(engageRes_df) = NULL

engageRes_df = reshape2::melt(engageRes_df, variable.id = 'Freq')
engageRes_df$Freq = factor(engageRes_df$Freq, levels = rev(c("Never",
                                                         "< 1 day/week",
                                                         "1-2 days/week",
                                                         "3-4 days/week",
                                                         "5-6 days/week",
                                                         "Every day")))
cols = c('#aba7a6','#82b582', HODP_mono()[5], as.character(HODPcols('primary'))[2:4])
resEng = ggplot(engageRes_df, aes(fill=Freq, y=value, x=variable)) + 
  geom_bar(position="fill", stat="identity")+
  ggtitle("How often have you engaged with the following \nsince the beginning of this semester?") +
  theme_HODP()+
  coord_flip()+
  scale_fill_manual(values = cols)+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title = element_blank())+
  xlab('Resource')+
  ylab('Proportion of Respondents')

png('resEng.png', width = 10, height = 7, res = 500, units = 'in')
resEng
RHODP::add_HODP_logo(width = 2)
dev.off()

  

# Word cloud...
library(ggwordcloud)
library(tm)
seniorYear = spring$What.would.you.like.to.see.as.part.of.your.senior.year.
length(seniorYear[seniorYear != ''])   # 93, make note of this!
seniorYear = seniorYear[seniorYear != '']
seniorYear_words = tm::removePunctuation(unlist(stringr::str_split(tolower(seniorYear),' ')))
for(i in 2:length(seniorYear_words)){
  if(seniorYear_words[i] == 'person' & seniorYear_words[i-1] == 'in'){
    seniorYear_words[i] = 'in-person'
  }
  if(seniorYear_words[i] == 'campus' & seniorYear_words[i-1] == 'on'){
    seniorYear_words[i] = 'on-campus'
  }
}
seniorYear_words[seniorYear_words == 'inperson'] = 'in-person'
seniorYear_words[seniorYear_words == 'oncampus'] = 'on-campus'
words_remove = c('i', 'a', 'the', 'and', 'to', 'in', 'be', 'as', 'for', 'on', 'of','will',
                 '', 'like', 'would', 'have', 'that','with', 'all', 'if', 'not', 'it', 'is',
                 'back', 'year', 'no', 'possible', 'harvard', 'more', 'some', 'see', 'we', 'by',
                 'my', 'there', 'this', 'able', 'can', 'everyone', 'much','or', 'at', 'but',
                 'are', 'being', 'fall', 'get', 'semester', 'again', 'fully', 'hope', 'id',
                 'make', 'senior', 'should', 'so', 'ability', 'also', 'an', 'house', 'least', 
                 'life', 'real', 'think', 'us', 'what', 'etc', 'full', 'seniors', 'spring',
                 'time', 'been', 'better', 'break', 'covid', 'even', 'halls', 'into', 'its',
                 'just','many','out','really','things','very','why','allow','college','doesnt',
                 'flip','good', 'harvards', 'hopefully','important','learning','new','next',
                 'one', 'online','reason','school','seems', 'semblance','thats', 'they', 'then',
                 'up','upset','want', 'was', 'well', 'were','when','which','who','years')

seniorYear_words = seniorYear_words[!seniorYear_words %in% names(table(seniorYear_words)[table(seniorYear_words) < 3])]
seniorYear_words = seniorYear_words[!seniorYear_words %in% words_remove]

seniorYear_words_df = data.frame(table(seniorYear_words))
seniorYear_words_df$seniorYear_words = str_to_title(seniorYear_words_df$seniorYear_words)
#write.csv(seniorYear_words_df,'words.csv')
seniorYear_words_df$angle = 90*sample(c(0, 1), nrow(seniorYear_words_df), replace = TRUE, prob = c(75, 25))

# might just use online...
ggplot(seniorYear_words_df, aes(label = seniorYear_words, size = Freq, angle = angle)) +
  geom_text_wordcloud_area(eccentricity = 5) +
  scale_size_area(max_size = 24) +
  theme_minimal()





## Comparison fall v Spring Satisfaction projection vs actual 
ultimate_q1 <- data.frame(Level = c("5- Very satisfied", "4- Somewhat satisfied", "3- Neither satisfied nor dissatisfied", "2- Somewhat dissatisfied", "1- Very dissatisfied"), c = c(0,0,0,0,0),Fall = c(52/638, 287/638, 92/638, 135/638, 72/638), Spring = c(30/498, 199/498, 93/498, 111/498, 65/498))
library(reshape2)
ultimate_q1 <- melt(ultimate_q1)
names(ultimate_q1)[3] <- "percent"

ult_q1_plot <- ggplot(data = ultimate_q1, aes(y=percent, x=variable, fill = Level)) +
  geom_bar(stat = "identity", width = .85, position = 'stack') +
  coord_flip() +
  labs(title= "Satisfaction with Spring Semester") + 
  ylab("Percentage of student respondents") +
  xlab("") +
  theme_HODP()+
  theme_void() +
  coord_polar(theta = 'y')+
  guides(fill=guide_legend(ncol=2)) +
  scale_fill_manual(values = rev(c("#4B5973", "#78C4D4", "#E2DDDB", "#FF9586", "#BE1E26")))+
  theme(legend.title = element_blank(), legend.position = 'bottom')+
  theme(plot.title = element_text(size=18, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))
png('ExpectationVsReality.png', width = 9, height = 7, res = 500, units = 'in')
ult_q1_plot
RHODP::add_HODP_logo(width = 2)
dev.off()




## What does time allocation look like this semester --> stratify by on or off campus
weeklyHours = spring[,grep('Roughly', colnames(spring))]
colnames(weeklyHours) = c('Coursework', 'Extracurriculars', 'Exercise', 'In-Person Socializing', 'Remote Socializing', 'Social Media')
# remove zeros...
weeklyHours = weeklyHours[weeklyHours$Coursework != 0,]
weeklyHours = weeklyHours[!is.na(weeklyHours$Coursework),]
# note that 168 total hours in the week... normalize...
weeklyHours = weeklyHours/168

avgWeek = data.frame(hours = colMeans(weeklyHours))
avgWeek = avgWeek/168
avgWeek['Other',] = 1-sum(avgWeek)-(1/3)
avgWeek['Sleep (Assumed)',] = 1/3
avgWeek$activity = rownames(avgWeek)

avgWeek$ymax = cumsum(avgWeek$hours)
avgWeek$ymin = c(0, head(avgWeek$ymax, n=-1))
avgWeek$activity = factor(avgWeek$activity, levels = c(c('Coursework', 'Extracurriculars', 'Exercise', 'In-Person Socializing', 'Remote Socializing', 'Social Media', 'Other', 'Sleep (Assumed)')))

cols = c("#4B5973", "#82b582", "#78C4D4", '#B88BE0', "#FA9E1C", "#EE3838", "#FF9586", "#E2DDDB")
all_avgWeek = ggplot(avgWeek, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=activity)) +
  geom_rect() +
  coord_polar(theta="y")+
  xlim(c(1.5, 4))+
  scale_fill_manual(values = cols)+
  theme_void()+
  ggtitle('How many hours per week have you \nspent on the following activities?')+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=18, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))

png('all_avgWeek.png', width = 10, height = 7, res = 500, units = 'in')
all_avgWeek
RHODP::add_HODP_logo(width = 2)
dev.off()


#compare on and off campus
weeklyHours = spring[,grep('Roughly', colnames(spring))]; colnames(weeklyHours) = c('Coursework', 'Extracurriculars', 'Exercise', 'In-Person Socializing', 'Remote Socializing', 'Social Media')
weeklyHours = cbind(weeklyHours, spring$Where.are.you.currently.living.); colnames(weeklyHours)[7] = 'livingSit'
weeklyHours = weeklyHours[weeklyHours$Coursework != 0,]
weeklyHours = weeklyHours[!is.na(weeklyHours$Coursework),]

weeklyHours_onOff = melt(weeklyHours,id.var="livingSit")
onOffCampus_activity = ggplot(weeklyHours_onOff, aes(variable, value,fill=livingSit))+ 
  geom_boxplot() + 
  labs(title = "Off-Campus (n = 264) vs. On-Campus students (n = 128)")+
  xlab('Activity')+
  ylab('Time Spent (Hours/week)')+
  scale_fill_manual(values = as.character(HODP_primary('red', 'light blue')))+
  theme_HODP()+
  theme(legend.title = element_blank(), legend.position = 'bottom', 
        plot.title = element_text(size=18, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        plot.background = element_rect('white'), panel.background = element_rect('white'))
  

png('onOffCampus_activity.png', width = 10, height = 5, res = 500, units = 'in')
onOffCampus_activity
RHODP::add_HODP_logo(width = 2)
dev.off()



# During the past 2 weeks, how often have you...
past2weeks = spring[,grep('During', colnames(spring))]
colnames(past2weeks) = c('Felt overwhelmed', 'Felt excessive pressure \nto succeed', 'Felt very sad', 'Felt so depressed it \nwas difficult to function', 'Stayed up all night to finish an \nassignment or prepare for an exam', 'Exercised')
past2weeks_compress = sapply(past2weeks,table)
past2weeks_compress = data.frame(past2weeks_compress[rownames(past2weeks_compress) != '',])
colnames(past2weeks_compress) = c('Felt overwhelmed', 'Felt excessive pressure \nto succeed', 'Felt very sad', 'Felt so depressed it \nwas difficult to function', 'Stayed up all night to \nfinish an assignment or \nprepare for an exam', 'Exercised')
past2weeks_compress$often = rownames(past2weeks_compress)
past2weeks_gg = melt(past2weeks_compress, id.vars = 'often')

past2weeks_gg$often = factor(past2weeks_gg$often, levels = rev(c("Very often","Often","Sometimes","Rarely","Never")))
past2weeks_gg$variable = factor(past2weeks_gg$variable, levels = rev(c('Felt overwhelmed', 
                                                                       'Felt excessive pressure \nto succeed', 
                                                                       'Felt very sad', 
                                                                       'Felt so depressed it \nwas difficult to function', 
                                                                       'Stayed up all night to \nfinish an assignment or \nprepare for an exam', 
                                                                       'Exercised')))
past2wks = ggplot(past2weeks_gg, aes(fill=often, y=value, x=variable)) + 
  geom_bar(position="fill", stat="identity")+
  ggtitle("During the past two weeks, how often have you...") +
  theme_HODP()+
  coord_flip()+
  scale_fill_manual(values = rev(as.character(HODP_primary())))+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.title = element_blank())+
  xlab('')+
  ylab('Proportion of Respondents')

png('past2weeks.png', width = 11, height = 7, res = 500, units = 'in')
past2wks
RHODP::add_HODP_logo(width = 2)
dev.off()



## stratify satisfaction...
satStrat = spring[,c('satisfaction', 'Are.you.an.international.student.', 'Are.you.on.Harvard.financial.aid.', 
       'Are.you.a.varsity.athlete.', 'Where.are.you.currently.living.', 'yearClass', 
       'Do.you.identify.as.a.first.generation.and.or.low.income.college.student..If.so..check.all.boxes.that.apply.')]
colnames(satStrat) = c('sat', 'international', 'finAid', 'varsityAthlete', 'onOffcampus', 'class', 'firstGen_lowIncome')

satStrat = satStrat[satStrat$sat != '',]
satStrat$satNum = satStrat$sat
satStrat$satNum = factor(satStrat$satNum, levels = c("Very dissatisfied",
                                                     "Somewhat dissatisfied",
                                                     "Neither satisfied nor dissatisfied",
                                                     "Somewhat satisfied",
                                                     "Very satisfied"))
satStrat$satNum = as.numeric(satStrat$satNum)
mean(satStrat$satNum)  # 3.036145
mean(satStrat$satNum[satStrat$international == 'Yes'])  # 2.565217
mean(satStrat$satNum[satStrat$varsityAthlete == 'Yes'])  # 2.807692
mean(satStrat$satNum[satStrat$onOffcampus == 'On-campus'])  # 3.496689
mean(satStrat$satNum[satStrat$onOffcampus == 'Off-campus'])  # 2.835735
mean(satStrat$satNum[satStrat$firstGen_lowIncome != 'None' & satStrat$firstGen_lowIncome != ''])  # 3.052632 (FGLI)

satStrat$class[satStrat$class == 'Senior, but graduating later than May 2021' | satStrat$class == 'Senior, and graduating in May 2021'] = 'Senior'
tapply(satStrat$satNum, satStrat$class, mean)
# First-year     Junior     Senior  Sophomore 
#   2.987578   3.263158   3.355263   2.748299 

# calculate standard error bars... (dvide by sqrt of n)
sd(satStrat$satNum[satStrat$international == 'Yes'])/sqrt(length(satStrat$satNum[satStrat$international == 'Yes'])) # 0.1511699
sd(satStrat$satNum[satStrat$varsityAthlete == 'Yes'])/sqrt(length(satStrat$satNum[satStrat$varsityAthlete == 'Yes'])) # 0.1737669
sd(satStrat$satNum[satStrat$onOffcampus == 'On-campus'])/sqrt(length(satStrat$satNum[satStrat$onOffcampus == 'On-campus'])) # 0.0829235
sd(satStrat$satNum[satStrat$onOffcampus == 'Off-campus'])/sqrt(length(satStrat$satNum[satStrat$onOffcampus == 'Off-campus'])) # 0.06373023
sd(satStrat$satNum[satStrat$firstGen_lowIncome != 'None' & satStrat$firstGen_lowIncome != ''])/sqrt(length(satStrat$satNum[satStrat$firstGen_lowIncome != 'None' & satStrat$firstGen_lowIncome != ''])) # 0.08905078

tapply(satStrat$satNum, satStrat$class, function(x) sd(x)/sqrt(length(x)))
# First-year     Junior     Senior  Sophomore 
# 0.09198787 0.09815297 0.12383343 0.10434755 

# manually create df...
satStrat_means = data.frame(category = c('All \nRespondents', 'International \nStudents', 'Varsity Athletes', 'Living \nOn-Campus', 'Living \nOff-Campus','FGLI','First-Years', 'Sophomores','Juniors', 'Seniors'),
                            satisfaction = c(3.036145, 2.565217, 2.807692, 3.496689, 2.835735, 3.052632, 2.987578, 2.748299, 3.263158, 3.355263),
                            SE = c(0, 0.1511699, 0.1737669, 0.0829235, 0.06373023, 0.08905078, 0.09198787, 0.10434755, 0.09815297, 0.12383343))
satStrat_means$relativeSat = satStrat_means$satisfaction-3.036145
satStrat_means = satStrat_means[order(satStrat_means$relativeSat),]
satStrat_means$posNeg = 'positive'; satStrat_means$posNeg[satStrat_means$relativeSat < 0] = 'negative'

satStrat_means$minSEbar = satStrat_means$relativeSat-satStrat_means$SE
satStrat_means$maxSEbar = satStrat_means$relativeSat+satStrat_means$SE
#satStrat_means$minSEbar[7] = 0
#satStrat_means$maxSEbar[5] = 0

satStrat_means$category = factor(satStrat_means$category, levels = c(satStrat_means$category))
satStrat_plot = ggplot(satStrat_means, aes(x=category, y=relativeSat, fill=posNeg)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=minSEbar, ymax=maxSEbar), width=.2,
                position=position_dodge(.9))+
  coord_flip()+
  theme_HODP()+
  scale_fill_manual(values = rev(c("#82b582", as.character(HODP_primary('red')))))+
  xlab('')+
  ylab('Mean Satisfaction Relative to All Respondents')+
  ggtitle('Satisfaction with Spring 2021 semester across Student Groups')+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.position = "none")+
  geom_text(aes(y=c(rep(0.035,6), rep(-0.035,4)), label=round(satisfaction,2), fontface=2, vjust = c(rep(0.5, 4), -1, 0.5, -1, rep(0.5, 3))), 
            color=c(rep("#BE1E26",5), 'black', rep("darkgreen",4)), size=3.5)

png('satStrat.png', width = 10, height = 7, res = 500, units = 'in')
satStrat_plot
RHODP::add_HODP_logo(width = 1.8)
dev.off()




### Satisfaction in general
scaleSat = spring[,grep('On.', colnames(spring))]
colnames(scaleSat) = unlist(stringr::str_split(colnames(scaleSat), pattern = '\\.\\.\\.\\.'))[seq(0,98,2)]
for(i in 1:100) colnames(scaleSat) = stringr::str_replace(colnames(scaleSat), pattern = '\\.', replacement = ' ')
colnames(scaleSat) = stringr::str_replace(colnames(scaleSat), pattern = ' s ', replacement = '\'s ')
# only keep a few to look at...
ggSat = scaleSat[,c(1,2,3,7,13,16,18,20,23,25,26,31,34,49)]
colnames(ggSat) = c('Harvard\'s Spring 2021 \nreopening plan overall',
                    'Virtual commencement',
                    'Remote classes',
                    'Communication from \nadministration',
                    'Virtual shopping week',
                    'Harvard\'s ability to prevent \nCOVID-19 infections on campus',
                    'Wellness days',
                    'Harvard\'s financial aid \npolicies during the pandemic',
                    'Support for students facing \nmental health challenges',
                    'Professors',
                    'Social interactions with peers',
                    'Spring 2021 tuition fees',
                    '$5,000 remote grant',
                    'On campus life overall')


# trying violin plots...
ggSat = melt(ggSat)
ggSat = ggSat[!(is.na(ggSat$value)),]

ggplot(ggSat, aes(x=variable, y=value)) + 
  geom_violin()

ggSat$variable = factor(ggSat$variable, levels = names(sort(tapply(ggSat$value, ggSat$variable, mean))))  #sort by median

ridgesSat = ggplot(ggSat, aes(y=variable, x=value, fill = variable)) + 
  stat_density_ridges(rel_min_height = 0, quantile_lines = TRUE, quantiles = 2, alpha = 0.75, size = .7)+ # median
  ylab('')+
  xlab('Rating')+
  ggtitle('On a scale from 0 (dissatisfied) to 10 (satisfied), \nhow satisfied are you with the following?\n')+
  theme_HODP()+
  scale_fill_manual(values = c(rep(as.character(HODP_primary()[1:4]),4)[1:14]))+
  theme(plot.title = element_text(size=18,  family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5),
        legend.position = "none")+
  scale_x_continuous(breaks = 0:10)
  #xlim(c(0,10))


png('ridgesStat.png', width = 10, height = 7, res = 500, units = 'in')
ridgesSat
RHODP::add_HODP_logo(width = 1.8)
dev.off()
  
  
# ggplot(ggSat, aes(x = value, y = variable, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
#     stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
#     scale_fill_viridis_c(name = "Tail probability", direction = -1)


# satisfaction pie chart....
satisfaction_df = data.frame(table(spring$satisfaction))
satisfaction_df = satisfaction_df[2:6,]
satisfaction_df$Var1 = factor(satisfaction_df$Var1, levels = c("Very dissatisfied", 
                                                               "Somewhat dissatisfied",
                                                               "Neither satisfied nor dissatisfied",
                                                               "Somewhat satisfied",
                                                               "Very satisfied"))
satisfaction_df$ymax = cumsum(satisfaction_df$Freq)
satisfaction_df$ymin = c(0, head(satisfaction_df$ymax, n=-1))
satisfaction_df = satisfaction_df[c(4,2,1,3,5),]
satisfaction_df$Freq = factor(satisfaction_df$Freq, levels= satisfaction_df$Freq)

satisfaction = ggplot(satisfaction_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  coord_polar(theta="y")+
  xlim(c(1.5, 4))+
  scale_fill_manual(values = as.character(HODP_primary())[c(1,2,5,3,4)])+
  theme_void()+
  ggtitle('How satisfied are you with Harvard\'s \nSpring 2021 semester?')+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=18, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))+
  theme(panel.background = element_rect(fill = "#F2F2F2", colour = NA), plot.background = element_rect(fill = "#F2F2F2", colour = "#d3d3d3"), legend.background = element_rect(fill = "transparent", 
                                                                                                                                           colour = NA), legend.key = element_rect(fill = "transparent", 
                                                                                                                                                                                   colour = NA))

png('satisfaction.png', width = 8.5, height = 7, res = 500, units = 'in')
satisfaction
RHODP::add_HODP_logo(width = 1.8)
dev.off()



## factors word cloud...
library(tm)
factorsSpring = spring$Please.elaborate.on.your.response.what.specific.factors.have.influenced.your.opinion.of.Harvard.s.Spring.2021.semester.so.far.
length(factorsSpring[factorsSpring != ''])
factorsSpring = factorsSpring[factorsSpring != '']
factorsSpring_words = tm::removePunctuation(unlist(stringr::str_split(tolower(factorsSpring),' ')))
for(i in 1:100) factorsSpring_words = stringr::str_remove(factorsSpring_words, '\n')

for(i in 2:length(factorsSpring_words)){
  if(factorsSpring_words[i] == 'person' & factorsSpring_words[i-1] == 'in'){
    factorsSpring_words[i] = 'in-person'
  }
  if(factorsSpring_words[i] == 'campus' & factorsSpring_words[i-1] == 'on'){
    factorsSpring_words[i] = 'on-campus'
  }
  if(factorsSpring_words[i] == 'days' | factorsSpring_words[i] == 'day' & factorsSpring_words[i-1] == 'wellness'){
    factorsSpring_words[i-1] = 'wellness days'
    factorsSpring_words[i] = ''
  }
}
factorsSpring_words[factorsSpring_words == 'inperson'] = 'in-person'
factorsSpring_words[factorsSpring_words == 'oncampus'] = 'on-campus'
factorsSpring_words[factorsSpring_words == 'offcampus'] = 'off-campus'
words_remove = c('i', 'a', 'the', 'and', 'to', 'in', 'be', 'as', 'for', 'on', 'of','will',
                 '', 'like', 'would', 'have', 'that','with', 'all', 'if', 'not', 'it', 'is',
                 'back', 'year', 'no', 'possible', 'harvard', 'more', 'some', 'see', 'we', 'by',
                 'my', 'there', 'this', 'able', 'can', 'everyone', 'much','or', 'at', 'but',
                 'are', 'being', 'get', 'semester', 'again', 'fully', 'id',
                 'make', 'should', 'so', 'ability', 'also', 'an', 'house', 'least', 
                 'life', 'real', 'think', 'us', 'what', 'etc', 'full', 'seniors',
                 'time', 'been', 'better', 'break', 'even', 'halls', 'into', 'its',
                 'just','many','out','really','things','very','why','allow','college','doesnt',
                 'flip','good', 'harvards', 'hopefully','important','learning','new','next',
                 'one', 'reason','school','seems', 'semblance','thats', 'they', 'then',
                 'up','upset','want', 'was', 'well', 'were','when','which','who','years', 'has',
                 'classes', 'students', 'am', 'from', 'im', 'feel', 'spring', 'campus', 'do','still', 
                 'me', 'their', 'because', 'people', 'experience', 'how', 'other', 
                 'day', 'dont', 'ive' , 'lack', 'going', 'having', 'off', 'could', 'given',
                 'about', 'great', 'had', 'class','doing','fall','made', 'however', 'them', 'lot','only',
                 'our', 'over', 'take', 'though','understand','any', 'did', 'most', 'same', 'most', 'offcampus', 'since',
                 'situation', 'while', 'actually', 'every', 'extremely', 'covid', 'definitely', 'felt', 'know', 'less', 'too',
                 'university','way','without','best','nice','stay','taking', 'done','need','now','bit',
                 'didnt', 'during', 'enough', 'especially', 'go', 'few', 'keep','little','part','through','week', 'although',
                 'arent', 'despite', 'first', 'pretty', 'these', 'those', 'completely', 'entire', 'fact', 'far', 'nothing')

factorsSpring_words = factorsSpring_words[!factorsSpring_words %in% names(table(factorsSpring_words)[table(factorsSpring_words) < 3])]
factorsSpring_words = factorsSpring_words[!factorsSpring_words %in% words_remove]

factorsSpring_words_df = data.frame(sort(table(factorsSpring_words), decreasing = T)[1:60])
factorsSpring_words_df$factorsSpring_words = str_to_title(factorsSpring_words_df$factorsSpring_words)
write.csv(factorsSpring_words_df,'wordBubble.csv')






# Waffle plots
vector5 <- c('Dissatisfied' = 78,'Neutral' = 9, 'Satisfied' = 13)
waffle12 <- waffle(vector5, rows = 10, size = 0.9,
                   colors = c('#760000', "#D84742",'#FF9586'),
                   title = "Social Life Satisfaction",
                   reverse = F)+
  theme(legend.title = element_blank(), legend.position = 'left')+
  theme(plot.title = element_text(size=26, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))

vector1 <- c('Dissatisfied' = 15, 'Neutral' = 13,'Satisfied' = 72)
waffle2 <- waffle(vector1, rows = 10, size = 0.9,
                  colors = c('#760000', "#D84742",'#FF9586'),
                  title = "Living Situation Satisfaction",
                  reverse = F)+
  theme(legend.title = element_blank(), legend.position = 'right')+
  theme(plot.title = element_text(size=24, family="Helvetica", face = "bold", margin = margin(t = 10, r = 0, b = 1, l = 0), hjust = 0.5))
  

png('waffle.png', width = 12, height = 7, res = 500, units = 'in')
grid.arrange(waffle12, waffle2, nrow = 1)
RHODP::add_HODP_logo(width = 1.8)
dev.off()



