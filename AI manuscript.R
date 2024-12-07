require(Hmisc)
require(gmodels)
require(ggplot2)
require(leaflet)
require(dplyr)
require(finalfit)
require(corrplot)#for plotting the correaltion matrix
require(cluster)#for clustering
require(lavaan)#for confirmatory factor analysis
require(haven)
mydata<-final.sheet.after.data.cleaning.4493
dim(mydata)
head(mydata)
sum(is.na(mydata))


#the demographics section=[1:6]
#the knowledge section=[7:16]
#the deep learning section[13:16]
#the attitude section[17:35 except for 30]
#Q30=unique
#Ai and medical curriculum[31:35] attitude
#perception[36:39]
#AI in rediology[37:39] perception
#AI training [40:42] unique
###################################
#country of residence
CrossTable(mydata$X2.Country.of.residence)
mydata$country<-mydata$X2.Country.of.residence
barplot(table(mydata$country))
CrossTable(mydata$country)
#question30
mydata$q30<-mydata$X30.Which.of.these.specialties.do.you.think.will.be.impacted.the.earliest.and.most.
mydata$q30[mydata$q30=="Oncology "]<-"Oncology"
mydata$q30[mydata$q30=="Surgery "]<-"Surgery"
mydata$q30[mydata$q30=="Diagnostic radiology"]<-"D.radiology"
mydata$q30[mydata$q30=="Family practice"]<-"F.practice"
mydata$q30[mydata$q30=="Internal medicine"]<-"internal.M"

table(mydata$q30)
mydata[mydata==""]<-NA
mydata<- mydata[complete.cases(mydata$q30),]

q30_table<-mydata %>% 
  summary_factorlist(dependent = "country", explanatory ="q30", total_col = TRUE, p=TRUE )
# write.csv(q30_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\q30 table.csv")
barplot(table(mydata$q30))
###################################
#the demographics section
#gender
CrossTable(mydata$X1.Gender)
mydata$X1.Gender[mydata$X1.Gender==""]<-NA
mydata$gender<-mydata$X1.Gender
barplot(table(mydata$gender))

#university
CrossTable(mydata$X3.University)
mydata$university<-mydata$X3.University
barplot(table(mydata$university))
#living zone
CrossTable(mydata$X4.Where.do.you.live.)
mydata$living_zone<-mydata$X4.Where.do.you.live.
barplot(table(mydata$living_zone))
#grade
CrossTable(mydata$X5.Grade)
mydata$grade<-mydata$X5.Grade
mydata$grade_merged<-ifelse(mydata$grade == 1| mydata$grade==2|mydata$grade==3,"Academic years", "Clinical Years")
barplot(table(mydata$grade))
#tech experience
table(mydata$X6.I.consider.myself.a.tech.savvy..well.informed.about.or.proficient.in.the.use.of.modern.technology..especially.computers.)
mydata$tech_experience<-mydata$X6.I.consider.myself.a.tech.savvy..well.informed.about.or.proficient.in.the.use.of.modern.technology..especially.computers.
mydata$tech_experience<-as.factor(mydata$tech_experience)
barplot(table(mydata$tech_experience))
table(mydata$tech_experience)
#demographics_table
explanatory_demog<-c("gender", "university", "living_zone", "grade", "tech_experience")
dependent_demog<-"country"
demographics<-mydata %>% 
  summary_factorlist(dependent_demog, explanatory_demog, total_col = TRUE, p= TRUE)
demographic<- as.data.frame(demographics)
View(demographic)
## write.csv(demographic,"D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\demographics.csv", row.names = FALSE)
##################################################
############################################3####
#the knowledge section
Q1k<-CrossTable(mydata$X7.I.have.an.understanding.of.the.basic.computational.principles.of.artificial.intelligence..AI.)
Q2K<-CrossTable(mydata$X8.I.am.comfortable.with.the.nomenclature.related.to.artificial.intelligence)
Q3K<-CrossTable(mydata$X9.I.have.an.understanding.of.the.limitations.of.artificial.intelligence)
Q4k<-CrossTable(mydata$X10.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Were.you.already.aware.of.these.topics.in.radiology..)
Q5k<-CrossTable(mydata$X11.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Do.you.personally.have.a.basic.understanding.of.the.technologies.used.in.these.topics..)
Q6k<-table(mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.)
#
#converting the result of Q6k into numbers
mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.[mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.=="Computer science projects involving artificial intelligence, Clinical research involving artificial intelligence"]<-2
mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.[mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.=="Courses on artificial intelligence / machine learning, Clinical research involving artificial intelligence"]<-2
mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.[mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.=="Courses on artificial intelligence / machine learning, Computer science projects involving artificial intelligence"]<-2
mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.[mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.=="Courses on artificial intelligence / machine learning, Computer science projects involving artificial intelligence, Clinical research involving artificial intelligence"]<-3
#converting the non of the above and the individual values
mydata$q6k<-mydata$X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.
mydata$q6k<-ifelse(mydata$q6k=="None of the above",0,
            ifelse(mydata$q6k==2,2,
                   ifelse(mydata$q6k==3,3,1)))
table(mydata$q6k)
CrossTable(mydata$q6k)
#
Q7k<-CrossTable(mydata$X13.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.is.a.class.of.machine.learning.algorithms.that.use.multiple.layers.of.neural.networks..)
Q8k<-CrossTable(mydata$X14.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.methods.learn.directly.from.data..without.the.need.for.manual.feature.extraction..)
Q9k<-CrossTable(mydata$X15.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Application.of.deep.learning.in.radiology.requires.large.databases.of.labeled.medical.images..)
Q10k<-CrossTable(mydata$X16.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.systems.are.often.opaque..it.can.be.difficult.to.delineate.the.underlying..thought.process...)
#renaming
mydata$q1k<-mydata$X7.I.have.an.understanding.of.the.basic.computational.principles.of.artificial.intelligence..AI.
mydata$q2k<-mydata$X8.I.am.comfortable.with.the.nomenclature.related.to.artificial.intelligence
mydata$q3k<-mydata$X9.I.have.an.understanding.of.the.limitations.of.artificial.intelligence
mydata$q4k<-mydata$X10.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Were.you.already.aware.of.these.topics.in.radiology..
mydata$q5k<-mydata$X11.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Do.you.personally.have.a.basic.understanding.of.the.technologies.used.in.these.topics..
mydata$q6k<-mydata$q6k
mydata$q7k<-mydata$X13.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.is.a.class.of.machine.learning.algorithms.that.use.multiple.layers.of.neural.networks..
mydata$q8k<-mydata$X14.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.methods.learn.directly.from.data..without.the.need.for.manual.feature.extraction..
mydata$q9k<-mydata$X15.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Application.of.deep.learning.in.radiology.requires.large.databases.of.labeled.medical.images..
mydata$q10k<-mydata$X16.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.systems.are.often.opaque..it.can.be.difficult.to.delineate.the.underlying..thought.process...
#rescoring the knowledge section
table(mydata$q1k)#5
table(mydata$q2k)#5
table(mydata$q3k)#5
table(mydata$q4k)
mydata$q4k<-ifelse(mydata$q4k=="Yes",2,0)
table(mydata$q5k)
mydata$q5k<-ifelse(mydata$q5k=="Yes",2,0)
table(mydata$q6k)#3
table(mydata$q7k)
mydata$q7k<-ifelse(mydata$q7k=="TRUE",2,0)
table(mydata$q8k)
mydata$q8k<-ifelse(mydata$q8k=="TRUE",2,0)
table(mydata$q9k)
mydata$q9k<-ifelse(mydata$q9k=="TRUE",2,0)
table(mydata$q10k)
mydata$q10k<-ifelse(mydata$q10k=="TRUE",2,0)
#classifying the knowledge scores into groups
#the maximum total score of knowledge =5+5+5+3+(2*6)=30
knowledge_section<-data.frame(mydata$q1k,mydata$q2k,mydata$q3k,mydata$q4k,mydata$q5k,mydata$q6k,mydata$q7k,mydata$q8k,mydata$q9k,mydata$q10k)
#creating the total knowledge variable
mydata$total_knowledge<-rowSums(knowledge_section)
table(mydata$total_knowledge)
hist(mydata$total_knowledge)
#leveling the total knowledge score
#the ratios
mydata$total_knowledge_ratio<-(mydata$total_knowledge/30)*100
table(mydata$total_knowledge_ratio)
#leveling
mydata$knowledge_levels<-ifelse(mydata$total_knowledge_ratio<60,"low",
                                ifelse(mydata$total_knowledge_ratio>59.99999 & mydata$total_knowledge_ratio<80,"moderate","high"))
CrossTable(mydata$knowledge_levels)
barplot(table(mydata$knowledge_levels))
mydata$knowledge_levels<-as.factor(mydata$knowledge_levels)

mydata$knowledge_levels<-factor(mydata$knowledge_levels,
                                levels = c("low","moderate","high"))
#creating the knowledge level per country table
explanatory_k<-"knowledge_levels"
dependent_k<-"country"
knowledge_table<-mydata %>% 
  summary_factorlist(dependent_k, explanatory_k, total_col = TRUE, p=TRUE)

# write.csv(knowledge_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\knowledge per country.csv")
###############################
#plotting knowledge per country
ggplot(data = mydata, mapping = aes(x=country, fill=knowledge_levels))+
  geom_bar()+
  scale_fill_manual(values = c("violetred4", "blue", "black"))+
  theme_bw()
##############################################3
#the deep learning questions
deep_learning_section
################
mydata$X7.I.have.an.understanding.of.the.basic.computational.principles.of.artificial.intelligence..AI.<-as.factor(mydata$X7.I.have.an.understanding.of.the.basic.computational.principles.of.artificial.intelligence..AI.)
mydata$X8.I.am.comfortable.with.the.nomenclature.related.to.artificial.intelligence<-as.factor(mydata$X8.I.am.comfortable.with.the.nomenclature.related.to.artificial.intelligence)
mydata$X9.I.have.an.understanding.of.the.limitations.of.artificial.intelligence<-as.factor(mydata$X9.I.have.an.understanding.of.the.limitations.of.artificial.intelligence)
#knowledge by country q by q
explanatory_k_q<-c("X7.I.have.an.understanding.of.the.basic.computational.principles.of.artificial.intelligence..AI.",
                   "X8.I.am.comfortable.with.the.nomenclature.related.to.artificial.intelligence",
                   "X9.I.have.an.understanding.of.the.limitations.of.artificial.intelligence",
                   "X10.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Were.you.already.aware.of.these.topics.in.radiology..",
                   "X11.Deep.Learning..and..Artificial.Intelligence..are.currently.being.broadly.discussed.in.the.radiological.community...Do.you.personally.have.a.basic.understanding.of.the.technologies.used.in.these.topics..",
                   "X12.Your.exposure.to.artificial.intelligence.includes...Please.select.all.that.apply.",
                   "X13.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.is.a.class.of.machine.learning.algorithms.that.use.multiple.layers.of.neural.networks..",
                   "X14.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.methods.learn.directly.from.data..without.the.need.for.manual.feature.extraction..",
                   "X15.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Application.of.deep.learning.in.radiology.requires.large.databases.of.labeled.medical.images..",
                   "X16.The.following.questions.test.your.understanding.of.deep.learning..a.subtype.of.artificial.intelligence....Deep.learning.systems.are.often.opaque..it.can.be.difficult.to.delineate.the.underlying..thought.process...")

k_by_q<-mydata %>% 
  summary_factorlist(dependent_k, explanatory_k_q, total_col = TRUE, p=TRUE)
# write.csv(k_by_q, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\knowledge by questions.csv")
#############################
#the deep learning section, total = 2*4=8
deep_learning_section<-data.frame(mydata$q10k,mydata$q9k,mydata$q8k,mydata$q7k)
mydata$total_deep_learning<-rowSums(deep_learning_section)
hist(mydata$total_deep_learning)
#ratios
mydata$total_deep_learning_ratio<-(mydata$total_deep_learning/8)*100
#creating the levels
mydata$deep_learning_levels<-ifelse(mydata$total_deep_learning_ratio<60,"low",
                                ifelse(mydata$total_deep_learning_ratio>59.9999 & mydata$total_deep_learning_ratio<80,"moderate","high"))
CrossTable(mydata$deep_learning_levels)
barplot(table(mydata$deep_learning_levels))
deep_learning_levels_table<- mydata %>% 
  summary_factorlist(dependent = "country",explanatory = "deep_learning_levels", total_col = TRUE, p=TRUE )
# write.csv(deep_learning_levels_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\deep learning table.csv")
#################################################
###############################################
#############################################
#the attitude section
Q1A<-CrossTable(mydata$X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.)
Q2A<-CrossTable(mydata$X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general.)
Q3A<-CrossTable(mydata$X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.)
Q4A<-CrossTable(mydata$X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future.)
Q5A<-CrossTable(mydata$X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.)
Q6A<-CrossTable(mydata$X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me.)
Q7A<-CrossTable(mydata$X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.)
Q8A<-CrossTable(mydata$X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me.)
Q9A<-CrossTable(mydata$X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.)
Q10A<-CrossTable(mydata$X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..)
Q11A<-CrossTable(mydata$X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.)
Q12A<-CrossTable(mydata$X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.)
Q13A<-CrossTable(mydata$X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.)
Q14A<-CrossTable(mydata$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence)
Q15A<-CrossTable(mydata$X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career)
Q16A<-CrossTable(mydata$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required)
Q17A<-CrossTable(mydata$X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance)
Q18A<-CrossTable(mydata$X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice)
##########
#renaming
q1a<-mydata$X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.
q2a<-mydata$X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general.
q3a<-mydata$X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.
q4a<-mydata$X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future.
q5a<-mydata$X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.
q6a<-mydata$X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me.
q7a<-mydata$X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.
q8a<-mydata$X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me.
q9a<-mydata$X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.
q10a<-mydata$X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..
q11a<-mydata$X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.
q12a<-mydata$X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.
q13a<-mydata$X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.
########
q14a<-mydata$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence
q15a<-mydata$X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career
q16a<-mydata$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required
q17a<-mydata$X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance
q18a<-mydata$X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice
#creating a dataframe for the attitude section
attitude_section<-data.frame(q1a,q2a,q3a,q4a,q5a,q6a,q7a,q8a,q9a,q10a,q11a,q12a,q13a,q14a,q15a,q16a,q17a,q18a)
#convering all the results into numbers
attitude_section<-ifelse(attitude_section=="Strongly agree"|attitude_section=="5",5,
                         ifelse(attitude_section=="Agree"|attitude_section=="4",4,
                                ifelse(attitude_section=="Neutral"|attitude_section=="3",3,
                                       ifelse(attitude_section=="Disagree"|attitude_section=="2",2,1))))
table(attitude_section)
View(attitude_section)
#creting the total attitude variable
mydata$total_attitude<-rowSums(attitude_section)#the maximum= 5*18=90
table(mydata$total_attitude)
hist(mydata$total_attitude)
#the ratio
mydata$total_attitude_ratio<-(mydata$total_attitude/90)*100
#creting the attitude levels variable
mydata$attitude_levels<-ifelse(mydata$total_attitude_ratio<60,"low",
                                     ifelse(mydata$total_attitude_ratio>59.9999 & mydata$total_attitude_ratio<80,"moderate","high"))
CrossTable(mydata$attitude_levels)
barplot(table(mydata$attitude_levels))
#attitude level by country
explanatory_at<-"attitude_levels"
attitude_levels_table<-mydata %>% 
  summary_factorlist(dependent = "country", explanatory_at, total_col = TRUE, p=TRUE)
# write.csv(attitude_levels_table,"D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\attitude levels.csv")
#atitude table by question
explanatory_at_q<-c("X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.",
                   "X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general.",
                  "X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.",
                  "X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future.",
                  "X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.",
                  "X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.",
                 "X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me.",
                "X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.",
               "X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me.",
              "X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.",
              "X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..",
              "X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.",
              "X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.",
              "X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.",
              "X31.All.medical.students.should.receive.teaching.in.artificial.intelligence",
              "X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career",
              "X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required",
              "X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance",
              "X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice")
mydata$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence<-as.factor(mydata$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence)
mydata$X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career<- as.factor(mydata$X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career)
mydata$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required<-as.factor(mydata$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required)
mydata$X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance<-as.factor(mydata$X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance)
mydata$X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice<-as.factor(mydata$X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice)
at_by_q<-mydata %>% 
  summary_factorlist(dependent_k, explanatory_at_q, p= TRUE, total_col = TRUE)
# write.csv(at_by_q, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\attitude by q.csv")
##########################
#the AI in medical curriculum $ attitude
AI_in_MC_section<-data.frame(q18a,q17a,q16a,q15a,q14a)
#the total
mydata$AI_in_MC_total<-rowSums(AI_in_MC_section)
table(mydata$AI_in_MC_total)
hist(mydata$AI_in_MC_total)
#raatio
mydata$AI_in_MC_ratio<-(mydata$AI_in_MC_total/25)*100
#levels
mydata$AI_in_MC_levels<-ifelse(mydata$AI_in_MC_ratio<60,"low",
                               ifelse(mydata$AI_in_MC_ratio>59.9999 & mydata$AI_in_MC_ratio<80,"moderate","high"))
table(mydata$AI_in_MC_levels)
barplot(table(mydata$AI_in_MC_levels))
#table AI in mc
explanatory_AI_MC="AI_in_MC_levels"
AI_MC_table<- mydata %>% 
  summary_factorlist(dependent_k, explanatory_AI_MC, total_col = TRUE, p=TRUE)
# write.csv(AI_MC_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\AI in MC.csv")
#table AI in MC by question
explanatory_AI_MC_q<-c("X31.All.medical.students.should.receive.teaching.in.artificial.intelligence",
                       "X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career",
                       "X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required",
                       "X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance",
                       "X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice")

AI_MC_q<-mydata %>% 
  summary_factorlist(dependent_demog, explanatory_AI_MC_q, total_col = TRUE, p=TRUE)
# write.csv(AI_MC_q, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\AI in MC by question.csv")
##################################################
################################################
#perception section
Q1p<-CrossTable(mydata$X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.)
#########
Q2p<-CrossTable(mydata$X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.)
Q3p<-CrossTable(mydata$X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.)
Q4p<-CrossTable(mydata$X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams.)
#perception table
explanatory_percep<-c("X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.",
                      "X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.",
                      "X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.",
                      "X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams.")

perception_table<-mydata %>% 
  summary_factorlist(dependent_demog, explanatory_percep, total_col = TRUE, p=TRUE)
# write.csv(perception_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\perception table.csv")
##########################
#Ai in radiology $ perception
Ai_in_rad<-data.frame(mydata$X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.,
                      mydata$X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.,
                      mydata$X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams.)
#numerating and leveling dosen't make sense
#####################3
#########################
#############################
#AI training
Q1t<-CrossTable(mydata$X40.I.have.received.teaching.training.in.artificial.intelligence)
Q2t<-CrossTable(mydata$X41.Was.this.teaching.training.a.compulsory.part.of.your.medical.degree.)
Q3t<-CrossTable(mydata$X42.Please.rate.the.usefulness.of.the.teaching.training.you.have.received)
#scoring
AI_training<-data.frame(mydata$X40.I.have.received.teaching.training.in.artificial.intelligence,
                        mydata$X41.Was.this.teaching.training.a.compulsory.part.of.your.medical.degree.,
                        mydata$X42.Please.rate.the.usefulness.of.the.teaching.training.you.have.received)
#numerating
AI_training<-ifelse(AI_training=="Yes",2,0)
#the total score = 2+2+5=9
mydata$AI_training_total<-(rowSums(AI_training))
table(mydata$AI_training_total)
hist(mydata$AI_training_total)
#ratio
mydata$AI_training_ratio<-(mydata$AI_training_total/9)*100
#levels
mydata$AI_training_levels<-ifelse(mydata$AI_training_ratio<60,"low",
                                  ifelse(mydata$AI_training_ratio>59.9999 & mydata$AI_training_ratio<80,"moderate","high"))
table(mydata$AI_training_levels)
barplot(table(mydata$AI_training_levels))
###########################################################
#tables AI training
explanatory_AI_training<-"AI_training_levels"
AI_training_levels_table<-mydata %>% 
  summary_factorlist(dependent_demog, explanatory_AI_training)#didn't work because it has only one level
#####
#table AI_training by q
mydata$X42.Please.rate.the.usefulness.of.the.teaching.training.you.have.received<-as.factor(mydata$X42.Please.rate.the.usefulness.of.the.teaching.training.you.have.received)
explanatory_AI_training_by_q<-c("X40.I.have.received.teaching.training.in.artificial.intelligence",
                                "X41.Was.this.teaching.training.a.compulsory.part.of.your.medical.degree.",
                                "X42.Please.rate.the.usefulness.of.the.teaching.training.you.have.received")
training_by_q_table<-mydata %>% 
  summary_factorlist(dependent_demog, explanatory_AI_training_by_q, total_col = TRUE, p=TRUE)
# write.csv(training_by_q_table, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\AI training by question.csv")
#======================
mydata$AI_training_q<-mydata$X40.I.have.received.teaching.training.in.artificial.intelligence
table(mydata$AI_training_q)
###########################################################
#table1----demographics by country
CrossTable(mydata$country,mydata$X1.Gender)
CrossTable(mydata$country,mydata$X3.University)
CrossTable(mydata$country,mydata$living_zone)
CrossTable(mydata$country,mydata$grade)
CrossTable(mydata$country,mydata$tech_experience)
################################
#table2----knowledge levels by country
CrossTable(mydata$country,mydata$knowledge_levels)
#########################
#table3----deep learning by country
CrossTable(mydata$country,mydata$deep_learning_levels)
############################
#table4----attitude level by country
CrossTable(mydata$country,mydata$attitude_levels)
#############################
# table5----AI in mc by country
CrossTable(mydata$country,mydata$AI_in_MC_levels)
####################33####3
#table6----AI training by country
CrossTable(mydata$country,mydata$AI_training_levels)
###############################################################
##############################################################
#############################################################
#the regression models
# regression model 1
knowledge_model<-lm(total_knowledge~mydata$gender+mydata$X3.University+mydata$living_zone+as.factor(mydata$grade)+as.factor(mydata$tech_experience)+AI_training_q, data=mydata)
summary(knowledge_model)
#plotting
require(ggResidpanel)
install.packages("ggResidpanel")
knowledge_model_plots<-resid_panel(knowledge_model, plots = "all", qqbands = TRUE, theme = "gray", smoother = TRUE)
plot(knowledge_model)
hist(knowledge_model$residuals)
#the knowledge logistic model
#low_high_model
knowledge_low_high<-mydata %>% filter(knowledge_levels=="low"|knowledge_levels=="high")
knowledge_low_high$knowledge_levels<-ifelse(knowledge_low_high$knowledge_levels=="low", 0,1)
knowledge_low_high$knowledge_levels<-as.factor(knowledge_low_high$knowledge_levels)
#explanatory
explanatory_knowledge_reg<-c("gender", "X3.University", "living_zone","grade", "tech_experience","AI_training_q" )
knowledge_low_high_model<-knowledge_low_high %>% 
  finalfit(dependent = "knowledge_levels", explanatory_knowledge_reg)
# write.csv(knowledge_low_high_model, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\knowledge low high model2.csv")
#low moderate model
knowledge_low_moderate<-mydata %>% filter(knowledge_levels=="low"|knowledge_levels=="moderate")
knowledge_low_moderate$knowledge_levels<-ifelse(knowledge_low_moderate$knowledge_levels=="low",0,1)
knowledge_low_moderate$knowledge_levels<-as.factor(knowledge_low_moderate$knowledge_levels)
knowledge_low_moderate_model<-knowledge_low_moderate %>% 
  finalfit(dependent = "knowledge_levels", explanatory_knowledge_reg)
# write.csv(knowledge_low_moderate_model, "D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\knowledge low moderate model2.csv")
##########################################
#####################
#hosemr lemeshow test
#ow_high
require(ResourceSelection)
high_low_model_glm<-glm(knowledge_levels~gender+X3.University+living_zone+grade+tech_experience+AI_training_q, data = knowledge_low_high, family=binomial())
#converting the knowledge level into numeric
knowledge_low_high$knowledge_levels_numeric<-as.numeric(knowledge_low_high$knowledge_levels)
hoslem_k_l_h_model<-hoslem.test(high_low_model_glm$fitted.values, knowledge_low_high$knowledge_levels_numeric)
#
#deviance test to confirm the hoslem lemeshow results
# Fit the logistic regression model
high_low_model_glm <- glm(knowledge_levels ~ gender + X3.University + living_zone + grade + tech_experience + AI_training_q, data = knowledge_low_high, family = binomial())

# Calculate the deviance residuals
deviance_resid <- residuals(high_low_model_glm, type = "deviance")

# Perform the deviance goodness-of-fit test
deviance_df <- sum(!is.na(deviance_resid)) - length(coef(high_low_model_glm))
deviance_chi_sq <- sum(deviance_resid^2)
deviance_p_value <- 1 - pchisq(deviance_chi_sq, deviance_df)

# Print the results
cat("Deviance goodness-of-fit test:\n")
cat("Chi-square statistic:", deviance_chi_sq, "\n")
cat("Degrees of freedom:", deviance_df, "\n")
cat("p-value:", deviance_p_value, "\n")
#it gave the same pvlaue





#low moderate
low_moderate_model_glm<-glm(knowledge_levels~gender+X3.University+living_zone+grade+tech_experience+AI_training_q, data = knowledge_low_moderate, family=binomial())
#converting the knowledge level into numeric
knowledge_low_moderate$knowledge_levels_numeric<-as.numeric(knowledge_low_moderate$knowledge_levels)
hoslem_k_l_m<-hoslem.test(low_moderate_model_glm$fitted.values,knowledge_low_moderate$knowledge_levels_numeric)

############################################
# attitude linear model
attitude_model<-lm(total_attitude~mydata$gender+mydata$X3.University+mydata$living_zone+as.factor(mydata$grade)+as.factor(mydata$tech_experience)+AI_training_q, data=mydata)
summary(attitude_model)#Rsquared is very low
#plotting the model
attitude_model_plots<-resid_panel(attitude_model, plots = "all", theme = "gray", qqbands = TRUE, smoother = TRUE)
plot(attitude_model)
hist(attitude_model$residuals)
###########################################
#logistic model
#data separation
mydata$grade<-as.factor(mydata$grade)
mydata$tech_experience<-as.factor(mydata$tech_experience)
mydata$attitude_reg<-ifelse(mydata$attitude_levels=="low", 0,1)
mydata$attitude_reg<-as.factor(mydata$attitude_reg)
explanatory_attitude_reg<- c("gender", "X3.University", "living_zone","grade", "tech_experience","AI_training_q" )
attitude_logistic_model<-mydata %>% finalfit(dependent = "attitude_reg", explanatory_attitude_reg)
# write.csv(attitude_logistic_model,"D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\attitude logistic model2.csv")
#########################################3
#33,34,35,29 by q40
explanatory_AI_training_effect<-c("X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.",
                                  "X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required",
                                  "X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance",
                                  "X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice")

dependent_AI_training_effect<-"X40.I.have.received.teaching.training.in.artificial.intelligence"
AI_training_effect<-mydata %>% 
  summary_factorlist(dependent =dependent_AI_training_effect, explanatory=explanatory_AI_training_effect, p=TRUE, total_col = TRUE )
# write.csv(AI_training_effect,"D:\\A-scientific\\cross sec\\study-5 AI\\AI study\\tables\\AI training effect.csv")
plot(AI_training_effect)

#####################################################
###########################################
################################
#the reviewer comments:
##################
df<-mydata
df$tech_merged<-apply(df[c(6)],2,likert_merjing_function4)
df$ai_training<-df$AI_training_q
#releveling q36
df$X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.<-
  factor(df$X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.,
         levels = c("Yes", "No", "Unsure"))
df_knowledge_model<-data.frame(df$X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.,
                                            df$X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general., 
                                            df$X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future., 
                                            df$X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future., df$X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.,
                                            df$X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.,
                                            df$X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me., 
                                            df$X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.,
                                            df$X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me., 
                                            df$X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.,
                                            df$X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..,
                                            df$X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.,
                                            df$X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.,
                                            df$X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.,
                                            df$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence,
                                            df$X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career,
                                            df$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required,
                                            df$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required,
                                            df$X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance,
                                            df$X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice,
                                            df$X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.,
                                            df$X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.,
                                            df$X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.,
                                            df$X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams.)




# likert_releveling_function<-function(x){factor(x, levels=c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree"))}
# df_knowledge_releveled<-data.frame(apply(explanatory_knowledge_merged_df[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)],2,likert_releveling_function))

#creating the merjing function
likert_merjing_function<-function(x){ifelse(x=="Agree", "A/SA",
                                            ifelse(x=="Strongly agree", "A/SA",
                                                   ifelse(x=="Disagree","D/SD",
                                                          ifelse(x=="Strongly disagree", "D/SD",
                                                                 ifelse(x=="Neutral","N",
                                                                        ifelse(x==1|x==2,"<3",
                                                                               ifelse(x==3|x==4|x==5,">3",
                                                                                      ifelse(x=="No","No",
                                                                                             ifelse(x=="Yes","Yes","Unsure")))))))))}

df_knowledge_likert_merjed<-data.frame(apply(df_knowledge_model[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)],2,likert_merjing_function))

#merjing the knowledge level and adding the new variable to the knowledge df
df$knowledge_merged<-ifelse(df$knowledge_levels=="high", "high/moderate",
                            ifelse(df$knowledge_levels=="moderate", "high/moderate", "low"))

df$knowledge_merged2<-factor(df$knowledge_merged, levels = c("low", "high/moderate"))

df_knowledge_likert_merjed$knowledge_merged<-df$knowledge_merged

# releveling q36
df_knowledge_likert_merjed$df.X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.<-
  factor(df_knowledge_likert_merjed$df.X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.,
         levels = c("Yes", "No", "Unsure"))





explanatory_knowledge_merged_reg<-c("df.X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.",
                                    "df.X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general.",
                                    "df.X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.",
                                    "df.X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future.",
                                    "df.X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.",
                                    "df.X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.",
                                    "df.X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me.",
                                    "df.X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.",
                                    "df.X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me.",
                                    "df.X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.",
                                    "df.X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..",
                                    "df.X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.",
                                    "df.X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.",
                                    "df.X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.",
                                    "df.X31.All.medical.students.should.receive.teaching.in.artificial.intelligence",
                                    "df.X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career",
                                    "df.X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required",
                                    "df.X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance",
                                    "df.X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice",
                                    "df.X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.",
                                    "df.X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.",
                                    "df.X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.",
                                    "df.X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams.")

knowledge_merged_table<-df_knowledge_likert_merjed %>% summary_factorlist(explanatory_knowledge_merged_reg, dependent = "knowledge_merged", total_col = TRUE, p=TRUE)

#the regression model
df_knowledge_likert_merjed$knowledge_merged<-ifelse(df_knowledge_likert_merjed$knowledge_merged=="low", 0,1)
df_knowledge_likert_merjed$knowledge_merged<-as.factor(df_knowledge_likert_merjed$knowledge_merged)
knowledge_merged_reg_model<-df_knowledge_likert_merjed %>% finalfit(explanatory_knowledge_merged_reg, dependent = "knowledge_merged")
write.csv(knowledge_merged_reg_model, "tables/reviewer comments/knowledge merged regression model by attitude and perception v2.csv")
#hoslem test for the knowledge model
knowledge_merged_glm_model<-glm(knowledge_merged~df.X17.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.radiology.+
                                df.X18.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.revolutionize.medicine.in.general.+
                                df.X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.+
                                df.X20.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human..non.interventional..physician.will.be.replaced.in.the.foreseeable.future.+
                                df.X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.+
                                df.X21.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...In.the.foreseeable.future..all.physicians.will.be.replaced.+
                                df.X22.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.frighten.me.+
                                df.X23.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.radiology.more.exciting.to.me.+
                                df.X24.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...These.developments.make.medicine.in.general.more.exciting.to.me.+
                                df.X25.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.never.make.the.human.physician.expendable.+
                                df.X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..+
                                df.X27.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.radiology.+
                                df.X28.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...Artificial.intelligence.will.improve.medicine.in.general.+
                                df.X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.+
                                df.X31.All.medical.students.should.receive.teaching.in.artificial.intelligence+
                                df.X32.Teaching.in.artificial.intelligence.will.be.beneficial.for.my.career+
                                df.X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required+
                                df.X34.At.the.end.of.my.medical.degree..I.will.have.a.better.understanding.of.the.methods.used.to.assess.healthcare.AI.algorithm.performance+
                                df.X35.Overall..at.the.end.of.my.medical.degree..I.feel.I.will.possess.the.knowledge.needed.to.work.with.AI.in.routine.clinical.practice+
                                df.X36.Would.you.consider.using.the.following.clinical.workflow..Patients..clinical.images.undergo.artificial.intelligence.analysis..A.specialist.subsequently.reviews.both.the.image.and.the.artificial.intelligence.findings.+
                                df.X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.+
                                df.X38.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.diagnosis.in.imaging.exams.+
                                df.X39.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.indication.of.appropriate.imaging.exams., data=df_knowledge_likert_merjed, family = binomial())


df_knowledge_likert_merjed$knowledge_level_numeric<-as.numeric(df_knowledge_likert_merjed$knowledge_merged)
hoslem.test(knowledge_merged_glm_model$fitted.values, df_knowledge_likert_merjed$knowledge_level_numeric)

#################################################
# #clustering
# knowledge_data<-knowledge_section
# knowledge_std<-scale(knowledge_data)
# View(knowledge_std)
# knowledge_clusters<-hclust(dist(knowledge_std), method = "ward.D2")
# plot(knowledge_clusters)
# #kmeans
# wss <- (nrow(knowledge_std)-1)*sum(apply(knowledge_std, 2, var))
# for (i in 2:10) wss[i] <- sum(kmeans(knowledge_std, centers = i)$withinss)
# plot(1:10, wss[1:10], type = "b", xlab = "Number of Clusters of knowledge section", ylab = "Within groups sum of squares")


######################################

#the single Qs regression models
likert_function2<-function(x){ifelse(x=="Agree"|x=="Strongly agree",1,
                           ifelse(x=="Disagree"|x=="Strongly disagree",0,9))}


likert_function3<-function(x){ifelse(x==1|x==2,0,
                                     ifelse(x==4|x==5,1,9))}


df$knowledge_merged3<-as.numeric(df$knowledge_merged2)
#renaming the questions
df$A3<-df$X19.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.human.radiologist.will.be.replaced.in.the.foreseeable.future.
df$A10<-df$X26.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...The.impact.of.artificial.intelligence.alone.will.reduce.the.number.of.radiologists.that.are.needed..
df$A13<-df$X29.Feelings.and.attitudes.towards.artificial.intelligence.and.deep.learning.in.medicine.and.radiology...I.am.less.likely.to.consider.a.career.in.radiology..given.the.advancement.of.AI.
df$ai_mc_1<-df$X31.All.medical.students.should.receive.teaching.in.artificial.intelligence
df$ai_mc_3<-df$X33.At.the.end.of.my.medical.degree..I.will.be.confident.in.using.basic.healthcare.AI.tools.if.required
df$precep<-df$X37.What.potential.applications.for.AI.in.radiology.do.you.see...Automated.detection.of.pathologies.in.imaging.exams.

#applying the function
df$A3<-apply(df["A3"],1 ,FUN=likert_function2)
df$A10<-apply(df["A10"],1, likert_function2)
df$A13<-apply(df["A13"],1,likert_function2)
df$ai_mc_1<-apply(df["ai_mc_1"],1,likert_function3)
df$ai_mc_3<-apply(df["ai_mc_3"],1,likert_function3)
df$precep<-apply(df["precep"],1,likert_function2)

#factoring
df$A3<-as.factor(df$A3)
df$A13<-as.factor(df$A13)
df$A10<-as.factor(df$A10)
df$ai_mc_1<-as.factor(df$ai_mc_1)
df$ai_mc_3<-as.factor(df$ai_mc_3)
df$precep<-as.factor(df$precep)
#filtering questions to remove the N level
df_A3<-df %>% filter(A3==0|A3==1)
df_A10<-df %>% filter(A10==0|A10==1)
df_A13<-df %>% filter(A13==0|A13==1)
df_ai_mc_1<-df %>% filter(ai_mc_1==0|ai_mc_1==1)
df_ai_mc_3<-df %>% filter(ai_mc_3==0|ai_mc_3==1)
df_precep<-df %>% filter(precep==0|precep==1)

#dropping the levels
df_A3<-droplevels(df_A3)
df_A10<-droplevels(df_A10)
df_A13<-droplevels(df_A13)
df_ai_mc_1<-droplevels(df_ai_mc_1)
df_ai_mc_3<-droplevels(df_ai_mc_3)
df_precep<-droplevels(df_precep)

# defining the explanatory variables
explanatory_single_q_reg<-c("gender","X3.University","living_zone","grade","tech_experience","AI_training_q", "knowledge_merged")
#knowledge merged refactoring for reference = "low"
explanatory_single_q_reg2<-c("grade_merged", "AI_training_q","knowledge_merged2")

#the models
A3_model<-df_A3 %>% 
  finalfit(dependent = "A3", explanatory_single_q_reg2)
write.csv(A3_model, "tables/reviewer comments/A3 model2 the human radiologists will be replaced.csv")

A10_model<-df_A10 %>% finalfit(dependent = "A10", explanatory_single_q_reg2)
write.csv(A10_model, "tables/reviewer comments/A10 model2 the impact of AI alone will reduce the number of rads.csv")

A13_model<-df_A13 %>% finalfit(dependent = "A13", explanatory_single_q_reg2)
write.csv(A13_model, "tables/reviewer comments/A13 model2 iam less likely to consider.csv")

ai_mc_1_model<-df_ai_mc_1 %>% finalfit(dependent = "ai_mc_1", explanatory_single_q_reg2)
write.csv(ai_mc_1_model,"tables/reviewer comments/ai mc 1 model2 all medical students should receive teaching.csv")

ai_mc_3_model<-df_ai_mc_3 %>% finalfit(dependent = "ai_mc_3", explanatory_single_q_reg2)
write.csv(ai_mc_3_model, "tables/reviewer comments/ai_mc_3 model2 at the end of medical digree i will be confident.csv")

precep_model<-df_precep %>% finalfit(dependent = "precep", explanatory_single_q_reg2)
write.csv(precep_model, "tables/reviewer comments/precep model2 what potential applications of ai in radiology do you see.csv")
#hosmer lemeshow test
A3_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_A3, family = binomial())
A10_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_A10, family = binomial())
A13_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_A13, family = binomial())
ai_mc_1_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_ai_mc_1, family = binomial())
ai_mc_3_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_ai_mc_3, family = binomial())
precep_model_glm<-glm(A3~grade_merged+AI_training_q+knowledge_merged2, data=df_precep, family = binomial())
#the test
df_precep$knowledge_merged2<-as.numeric(df_precep$knowledge_merged2)
require(ResourceSelection)
hoslem_A3<-hoslem.test(A3_model_glm$fitted.values, df_A3$knowledge_merged3)
hoslem_A10<-hoslem.test(A10_model_glm$fitted.values, df_A10$knowledge_merged3)
hoslem_A13<-hoslem.test(A13_model_glm$fitted.values, df_A13$knowledge_merged3)
hoslem_ai_mc_1<-hoslem.test(ai_mc_1_model_glm$fitted.values, df_ai_mc_1$knowledge_merged3)
hoslem_ai_mc_3<-hoslem.test(ai_mc_3_model_glm$fitted.values, df_ai_mc_3$knowledge_merged3)
hoslem_precep<-hoslem.test(precep_model_glm$fitted.values, df_precep$knowledge_merged2)
#the correlation matrix
likert_function4<-function(x){ifelse(x=="Strongly disagree",1,
                                     ifelse(x=="Disagree",2,
                                            ifelse(x=="Neutral",3,
                                                   ifelse(x=="Agree",4,5))))}


Q3.Attitude<-as.numeric(apply(df["A3"],1,likert_function4))
Q10.Attitude<-as.numeric(apply(df["A10"],1,likert_function4))
Q13.Attitude<-as.numeric(apply(df["A13"],1,likert_function4))
Q1.AI.in.MC<-as.numeric(df$ai_mc_1)
Q3.AI.in.MC<-as.numeric(df$ai_mc_3)
Q2.preception<-as.numeric(apply(df["precep"],1,likert_function))

df_cor_mat<-data.frame(Q3.Attitude,
                       Q10.Attitude,
                       Q13.Attitude,
                       Q1.AI.in.MC,
                       Q3.AI.in.MC,
                       Q2.preception)

cor_mat<-cor(df_cor_mat)
write.csv(cor_mat, "tables/reviewer comments/cor mat for the 6 regression questions.csv")
require(corrplot)
corrplot(cor_mat, method = "circle", type="upper", order="hclust", tl.col="black", tl.srt = 45)
########################################################
#merjing
likert_merjing_function4<-function(x){ifelse(x=="Agree"|x=="Strongly agree", "A/SA",
                                             ifelse(x=="Disagree"|x=="Strongly disagree","D/SD",
                                                    ifelse(x=="Neutral"|x==3,"N",
                                                           ifelse(x==1|x==2,"D/SD",
                                                                  ifelse(x==4|x==5,"A/SA",
                                                                         ifelse(x=="No","No",
                                                                                ifelse(x=="Yes","Yes",
                                                                                       ifelse(x=="Unsure","Unsure",
                                                                                              ifelse(x=="TRUE","TRUE",
                                                                                                     ifelse(x=="FALSE","FALSE","I don't know"))))))))))}


df2=data.frame(apply(df[c(6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)],2,likert_merjing_function4))
write.csv(df2,"tables/reviewer comments/df2 merged strings.csv")

likert_merjing_function5<-function(x){ifelse(x=="Agree"|x=="Strongly agree", "1",
                                             ifelse(x=="Disagree"|x=="Strongly disagree","0",
                                                    ifelse(x=="Neutral"|x==3,"9",
                                                           ifelse(x==1|x==2,"0",
                                                                  ifelse(x==4|x==5,"1",
                                                                         ifelse(x=="No","0",
                                                                                ifelse(x=="Yes","1",
                                                                                       ifelse(x=="Unsure","9",
                                                                                              ifelse(x=="TRUE","1",
                                                                                                     ifelse(x=="FALSE","0","9"))))))))))}


df3=data.frame(apply(df[c(6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)],2,likert_merjing_function5))
df3<-data.frame(demog_df,df3,df$tech_merged,df$ai_training,df$knowledge_merged)
write.csv(df3,"tables/reviewer comments/df3 merged integers2.csv")

###########################################
explanatory_demog_py<-c("gender", "university", "living_zone", "grade_merged", "knowledge_merged", "tech_merged", "ai_training")
df_demog_py<-df[explanatory_demog_py]
df_demog_py$ai_training<-factor(df_demog_py$ai_training,levels = c("Yes","No"))
table(df_demog_py$ai_training)
View(df_demog_py)
write.csv(df_demog_py, "tables/reviewer comments/df_demog_py.csv")
###########################
#confirmatory factor analysis
require(lavaan)
require(haven)


df_cfa=read.csv(file.choose())
View(df_cfa)
df_cfa2=df_cfa[, -c(32,33,34)]
View(df_cfa2)
model<-"knowledge=~q1+q2+q3+q4+q5+q6+q7+q8+q9
        attitude=~q10+q11+q12+q13+q14+q15+q15+q16+q17+q18+q19+q20+q21+q22+q23+q24+q25+q26+q27
        perception=~q28+q29+q30+q31"

summary(fit_cfa, fit.measures=TRUE,standardized=TRUE)
summary(fit_cfa, standardized=TRUE)
inspect(fit_cfa, what="std")

require(semPlot)
semPaths(fit, std)
require(lavaanPlot)
lavaanPlot(fit_cfa,
           node_labels=TRUE,
           edge_labels=TRUE,
           title="CFA plot")
