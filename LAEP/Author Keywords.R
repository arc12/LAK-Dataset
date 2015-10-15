## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2015, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************

## ************* Background ***************
#This was created for the LAEP Project - https://laepanalytics.wordpress.com/ -
# to suit LAK dataset RDF dumps made available September 2015 -http://www.solaresearch.org/resources/lak-dataset/
# This code is also available from Github: https://github.com/arc12/LAK-Dataset

## ************* Purpose ******************
#Process the author-specified keywords, as previously extracted from the LAK Dataset RDF
# Processing to: find keyword frequency and distribution between SoLAR and IEDM, including grouping according to manually-applied labels; keyword co-occurrence analysis (export for Gephi visualisation)
#Pre-processing involves merging some terms.
#Also a simple extract of DBPedia URIs which have been determined by the LAK Dataset creators/process on the basis of author keywords

#The fields expected in the input data are:
# origin [journal or proceedings], year, paper [URI], title [of paper], subject [literal or dbpedia URI]
dataDir<-"./LAEP/Data/"
outDir<-"./LAEP/Out/"
plotDir<-"./LAEP/Plots/"

# ---------------
#Literal keywords
# ---------------
literals<- read.csv(paste(dataDir,"All Literals.csv", sep=""), stringsAsFactors=F)
#the data contains leading and trailing spaces!
literals$subject<- trimws(literals$subject, which="both")

##synthesise a "community" label based on the publication
solar<-c("<http://data.linkededucation.org/resource/lak/conference/lak2011/proceedings>",
         "<http://data.linkededucation.org/resource/lak/conference/lak2012/proceedings>",
         "<http://data.linkededucation.org/resource/lak/conference/lak2013/proceedings>",
         "<http://data.linkededucation.org/resource/lak/conference/lak2014/proceedings>",
         "<http://data.linkededucation.org/resource/lak/journal/jla/2014-01-01>",
         "<http://data.linkededucation.org/resource/lak/journal/jla/2014-01-02>")
iedm<-c("<http://data.linkededucation.org/resource/lak/conference/edm2010/proceedings>",
        "<http://data.linkededucation.org/resource/lak/conference/edm2011/proceedings>",
        "<http://data.linkededucation.org/resource/lak/conference/edm2012/proceedings>",
        "<http://data.linkededucation.org/resource/lak/conference/edm2013/proceedings>",
        "<http://data.linkededucation.org/resource/lak/conference/edm2014/proceedings>",
        "<http://data.linkededucation.org/resource/lak/journal/jedm/2009-01-01>",
        "<http://data.linkededucation.org/resource/lak/journal/jedm/2011-03-01>",
        "<http://data.linkededucation.org/resource/lak/journal/jedm/2012-04-01>")
literals<-cbind(literals, data.frame(community=rep(NA, length(literals[,1]))))
literals$community[which(literals$origin %in% solar)]<-"SoLAR"
literals$community[which(literals$origin %in% iedm)]<-"IEDM"

#just get the unique papers and their venue
p<-literals[,c("paper","community")]
p.unique<-p[!duplicated(p),]
p.comm.table<-table(p.unique$community)#number of key-terms (subjects) used by each community

#get distinct subject in alpha order
l.unique<-unique(literals$subject)
l.unique<-l.unique[order(l.unique)]
write.csv(l.unique,file=paste(outDir,"Distinct Subject Literals.csv", sep=""))

#some basic stats on keywords used *per collection* (not papers used)
l.origin.table<-table(literals$origin)
l.origin.table

#merge keywords that appear to be simply different choices of word for the same idea (generally avoiding merging if it is only likely that the same concept was implied, and without any overlay of generalisation)
#for each item in the list, the first entry in the vector is the preferred term and the others will be replaced by it
replacements<-list(c('big data', 'big-data mining'),                   
                   c('blended learning', 'blended learning courses'),
                   c('CSCL', 'cscl', 'computer-supported collaborative learning', 'computer-supported collaborative learning', 'computer supported collaborative learning (cscl)', 'computer-supported collaborative learning'),
                   c('data analysis', 'data analytics', 'data-driven analysis', 'data extraction and analysis'),
                   c('data fusion', 'data integration'),
                   c('design research', 'design-research'),
                   c('discourse analytics', 'discourse analysis'),
                   c('distance education', 'distance learning'),
                   c('elearning', 'e-learning'),
                   c('graphs', 'graph'),
                   c('high stakes tests', 'high?stakes tests'),
                   c('learning management system', 'learning management systems', 'virtual learning environment', 'course management systems'),
                   c('log files', 'log data'),
                   c('machine learning', 'machine learning analytics'),
                   c('massive open online courses', 'massive open online courses (moocs)', 'mooc', 'moocs'),                   
                   c('mathematics education', 'math'),
                   c('mobile application', 'mobile applications'),
                   c('networked learning', 'network learning'),
                   c('open learner model', 'open learner models'),
                   c('personalized learning', 'personalization'),                
                   c('prediction', 'predictive analytics'),                   
                   c('predictive modeling', 'predictive modelling'),
                   c('recommender system', 'recommendation engines'),
                   c('regression', 'regression analysis'),                
                   c('social learning analytics', 'social learning analysis'),                   
                   c('social recommender system', 'social recommendation'),
                   c('SoLAR', 'society for learning analytics research', 'solar'),
                   c('technology enhanced learning', 'technology-enhanced learning'),             
                   c('text mining', 'text analysis', 'text-mining'),
                   c('visual analytics', 'visual analytics. visualization'),
                   c('visualization', 'data visualization', 'information visualization', 'visualisation', 'visualization'),
                   c('algorithm', 'algorithms'),
                   c('association rule', 'association rules', 'association rule mining'),
                   c('attitude', 'attitudes'),
                   c('automated essay scoring', 'automatic essay scoring'),
                   c('automated feedback', 'automatic feedback'),
                   c('bayesian knowledge tracing', 'bayesian knowledge-tracing'),
                   c('bayesian network', 'bayesian networks'),
                   c('behavior', 'behaviors'),
                   c('bloom\'s taxonomy', 'bloom’s taxonomy'),
                     c('centrality', 'centrality measures'),
                     c('cluster analysis', 'clustering analysis'),
                     c('cognitive diagnostic model', 'cognitive diagnostic models (cdms)'),
                     c('cognitive tutor', 'cognitive tutors'),
                     c('computer-mediated tutoring', 'computermediated tutoring'),                     
                     c('data pre-processing', 'data preprocessing'),
                     c('detector', 'detectors'),
                     c('dynamical analysis', 'dynamical analyses'),
                     c('dynamic bayesian networks', 'dynamic bayes net', 'dynamic bayes nets'),
                     c('educational assessment', 'educational assessments'),
                     c('educational game', 'educational games'),
                     c('eye tracking', 'eye-tracking'),
                     c('human-computer interaction (hci)', 'humancomputer interaction'),
                     c('intelligent tutor', 'intelligent tutors'),
                     c('intelligent tutoring system', 'intelligent tutoring', 'intelligent tutoring systems', 'its'),
                     c('interactive tabletop', 'interactive tabletops'),
                     c('item response theory', 'irt'),
                     c('k-means', 'k-means clustering'),
                     c('learner modeling', 'learners modeling', 'learner models'),
                     c('learning management system', 'learning content management systems', ''),
                     c('logistic regression', 'logistic regression models'),
                     c('massive open online course', 'massive online open courses', 'massive open online courses'),
                     c('mastery learning', ' mastery learning'),
                     c('maximum likelihood estimation', 'maximum likelihood'),
                     c('multiple representations', 'multiple-representations'),
                     c('off-task behavior', 'off-task behaviors'),
                     c('online learning environments', 'digital learning environments'),
                     c('pattern', 'patterns'),                     
                     c('self-evaluation', 'selfevaluation'),
                     c('sensor-free detectors', 'sensor-free detection'),
                     c('sequence mining', 'sequence pattern mining', 'sequential pattern analysis', 'sequential pattern mining'),
                     c('serious game', 'serious games'),
                     c('simulation', 'simulations'),
                     c('social network analysis', 'social networks analysis'),
                     c('standardized test', 'standardized tests'),
                     c('student drop-out', 'student dropout'),
                     c('student model', 'student modeling'),
                     c('student performance', 'student\'s performance'))
for(r in replacements){
   indeces<-which(literals$subject %in% r)
   literals$subject[indeces]<-r[1]
}

#distinct terms after stanardising with replacements as above
l.unique<-unique(literals$subject)
l.unique<-l.unique[order(l.unique)]
write.csv(l.unique,file=paste(outDir,"Distinct Standardised Subject Literals.csv", sep=""))

# >>>>>>>>>>> count occurrences and order by frequency <<<<<<<<<<<<<<<<<<
l.table<-table(literals$subject)
l.table<-l.table[order(l.table, decreasing=T)]
write.csv(l.table,file=paste(outDir,"Subject Literal Frequencies.csv",sep=""))

# >>>>>>>>>>>>>>>Compute relative frequency, split according to the SoLAR/IEDM communities <<<<<<<<<<<<<<<<<
#Relative freq is defined as the proportion of papers with the keyword *in that community*
#The actual difference in keyword freq (total) observed in the dataset (at present!) is only about 20%, so the proportional weighting is not that different from just showing counts broken down by community but....
#append empty cols for both communities, ready for injection of counts
l.df<-data.frame(row.names="term", term=names(l.table), count=l.table, SoLAR=rep(NA, length(l.table)), IEDM=rep(NA, length(l.table)))
#tabulate frequencies only for single communities, extract selected terms for plotting, and scale
l.table.solar<-table(literals$subject[literals$community=="SoLAR"])
l.table.iedm<-table(literals$subject[literals$community=="IEDM"])
#injet proportions (as %) into DF
l.df[,"SoLAR"]<- 100*l.table.solar[row.names(l.df)]/p.comm.table["SoLAR"]
l.df[,"IEDM"]<- 100*l.table.iedm[row.names(l.df)]/p.comm.table["IEDM"]
l.df[is.na(l.df)]<-0
#re-order according to the weighted frequency (the original table was ordered by raw total frequency)
weighted.freq<-l.df[,"SoLAR"]+l.df[,"IEDM"]
l.df<-cbind(l.df, sum=weighted.freq)
l.df<-l.df[order(weighted.freq, decreasing=T),]

Selective.Bar.Plot<-function(term.list, title, file.name){
   png(paste(plotDir,file.name,".png",sep=""), width = 1000, height = 1000)
   par(mar = c(4,10.5,2,2) + 0.1, cex=2)#must be set INSIDE png renderer
   df<-l.df[term.list,]
   df<-df[order(df$sum, decreasing=T),]
   barplot(t(as.matrix(df[,c("SoLAR","IEDM")])), horiz=T, names.arg=row.names(df), legend.text=colnames(df[,c("SoLAR","IEDM")]), cex.names=0.75, las=1, main=paste(title,"Keywords"), xlab="% of papers", ylim=c(0,5+max(30, length(term.list))))
   dev.off()
}

#1st select those terms which will be plotted,
#suppress "learning analytics", the top term and plot equal batches down to 4 occurrences
batch<-round((sum(l.table>3)-1)/3)
Selective.Bar.Plot(rownames(l.df)[2:batch], title="Highest Frequency", file.name = "Top Terms")
Selective.Bar.Plot(rownames(l.df)[batch+1:batch], title="High Frequency", file.name = "High Terms")
Selective.Bar.Plot(rownames(l.df)[2*batch+1:batch], title="Mid-range Frequency", file.name = "Mid Terms")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> Inspect some some manually-grouped term groups <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#quick pie plot for keywords that occur 3 or more times. plot shows the total number of keyword occurrences, not number of distinct keywords in the category
#some very bland terms have not been included: collaboration,learning,evaluation,education,multiple representations,automated detectors,discovery with models,fractions,portability,practice,special issue
tag.labels<-c("analytics technique","software system","human attribute, behaviour, or state","objective of analytics","educational context or activity","educational theory","general domain term","research or design method")
tag.shortlalels<-c("Techniques", "Software", "Human Subject", "Objectives", "Context", "Theory", "General", "ResearchMethods")
tag.counts<-c(164,80,66,76,150,9,332,8)
tag.counts2<-c(164,80,66,76,150,9,167,8)#omit "learing analytics" an "educational data mining"
tag.cols=rainbow(length(tag.labels))
png(paste(plotDir,"Categories.png",sep=""), width = 1000, height = 1000)
pie(x=tag.counts[order(tag.counts)], labels=tag.shortlalels[order(tag.counts2)] , main="Categorised Keyword Occurrence",init.angle=88, col = tag.cols[order(tag.counts)], radius = 0.5, cex.main=2.0, cex=2.0)
dev.off()
png(paste(plotDir,"Categories_excl.png",sep=""), width = 1000, height = 1000)
pie(x=tag.counts2[order(tag.counts2)],labels = tag.shortlalels[order(tag.counts2)], main="Categorised Keyword Occurrence (excl LA and EDM)",init.angle=88, col = tag.cols[order(tag.counts2)], radius = 0.5, cex.main=2.0, cex=2.0)
dev.off()
png(paste(plotDir,"Categories_Legend.png",sep=""), width=600, height=400)
op<-par(mar=c(2,2,2,2))
plot.new()
legend(x="bottomleft",legend=tag.labels, fill=tag.cols, title="Keyword Categories", cex=2.0)
dev.off()

#these are for 3 or more occurrences of the keyword
analytics.techniques<-c("knowledge tracing","social network analysis","clustering","item response theory","sequence mining","bayesian knowledge tracing","bayesian network","classification","natural language processing","feature engineering","logistic regression","text mining","association rule","cluster analysis","dynamic bayesian networks","hierarchical clustering","matrix factorization","process mining","psychometrics","regression","computational linguistics","data fusion","feature selection","k-means","multilevel bayesian models","predictive modeling","q-matrix","supervised learning")
sw.system<-c("intelligent tutoring system","learning management system","cognitive tutor","intelligent tutor","moodle","dialogue systems","exploratory learning environments","online learning environments","social media","tutoring system")
human<-c("affect","engagement","behavior","boredom","confusion","frustration","gaming the system","motivation","off-task behavior","participation","academic performance","learning dispositions","learning skills","metacognition","performance","personality","student performance")
obj.anal<-c("prediction","personalized learning","affective computing","affect detection","retention","user modeling","individual differences","causal discovery","learner modeling","cognitive modeling","effect of help on performance","knowledge retention","stealth assessment","student retention")
ed.ctxt<-c("massive open online course","collaborative learning","higher education","assessment","CSCL","educational assessment","educational game","elearning","online learning","mathematics education","networked learning","self-regulated learning","formative assessment","writing","blended learning","discourse","distance education","game-based learning","games","homework","mathematics","reading comprehension","science inquiry","serious game","social learning","tutorial dialogue")
res.theory<-c("constructionism","design research","evidence-centered design","pedagogy")
general.domain<-c("learning analytics","educational data mining","data mining","student model","visualization","machine learning","discourse analytics","social learning analytics","analytics","big data","data analysis","open source","academic analytics","eye tracking","log files","SoLAR","visual analytics","analysis","dynamical analysis","learning curves","open learner model","scientometrics","teaching analytics")


Selective.Bar.Plot(analytics.techniques,"Analytics Techniques","Cat-analytics_tech")
Selective.Bar.Plot(sw.system,"Software-related","Cat-software")
Selective.Bar.Plot(human,"Human Attributes, Behaviour, or State","Cat-human")
Selective.Bar.Plot(obj.anal,"Aims/Objectives of the Analytics","Cat-objective")
Selective.Bar.Plot(ed.ctxt,"Educational Context","Cat-ed_context")
Selective.Bar.Plot(res.theory,"Research and Theory","Cat-research_theory")
Selective.Bar.Plot(general.domain,"General Domain","Cat-general")

## >>>>>>>>>>>>>>>>>>>>>> keyword co-occurrence <<<<<<<<<<<<<<<<<<<<<<<<<
co.terms<-names(l.table[l.table>3])#freq>3 - may want to drop this selection completely - use for now to keep viz more simple
co.df<-literals[which(literals$subject %in% co.terms), c("paper","subject")]
co.table<-table(co.df)
co.incidence<-t(co.table) %*% co.table #elements are number of times keyword pair occurs
diag(co.incidence)<-0#diagonals have keyword count, but we'll suppress self-edges

#this computes the IEDM:SoLAR balance for each term. The proportion due to IEDM is computed
comm.df<-literals[which(literals$subject %in% co.terms), c("community","subject")]
comm.table<-table(comm.df)
comm.ratio<-comm.table["IEDM",]/(comm.table["IEDM",]+comm.table["SoLAR",])

#quick visualisation in R - not exporting this
library(network)
co.net<-network(co.incidence )
plot(co.net, vertex.cex=l.table[co.terms]/20, mode="fruchtermanreingold") #scale nodes according to term frequency

#save for gephi
node.labels<-rownames(co.incidence)
nodes<-data.frame(ID=seq(length(node.labels)), Label=node.labels, Weight=l.table[node.labels], IEDM_Portion=comm.ratio[node.labels])
write.csv(nodes, file=paste(outDir,"gephiNodes_gt3.csv",sep=""),row.names=F)
edges<-which(co.incidence!=0,arr.ind = T)#this gives spurious row names but we dont need anyway
edges<-cbind(edges,co.incidence[edges])
edges<-cbind(edges,rep("Undirected",length(edges[,1])))
colnames(edges)<-c("Source","Target","Weight","Type")
write.csv(edges, file=paste(outDir,"gephiEdges_gt3.csv",sep=""),row.names=F)

## - keyword occurrence for selected terms which were manually selected by inspection of all
# keywords to select those which appeared to bemost relevant to the topic of adoption or implementation issues
rq.terms<-c("analytics architecture", "open source","pedagogy","policy","privacy","capacity building",
"change management","community building","cost-effectiveness","cultural change","impact analysis","implementation","leadership","pedagogical models","privacy theory","school failure","strategic planning","sustainability","systemic application")
# not included as they were obviously off topic when the paper titles they relate to were looked up:
# "strategy performance","cyber security","quality control","software quality","software security","uptake"
rq.df<-literals[which(literals$subject %in% rq.terms), ]
write.csv(rq.df, file=paste(outDir,"RQ Papers Keywords.csv",sep=""))
rq.table<-table(rq.df[,c("paper","subject")])
rq.incidence<-t(rq.table) %*% rq.table #elements are number of times keyword pair occurs
diag(rq.incidence)<-0#diagonals have keyword count, but we'll suppress self-edges

#quick visualisation in R - not exporting this
rq.net<-network(rq.incidence )
plot(rq.net, vertex.cex=1.0, mode="fruchtermanreingold")

#save for gephi !!!!! this over-writes environment variables 
node.labels<-rownames(rq.incidence)
nodes<-data.frame(ID=seq(length(node.labels)), Label=node.labels, Weight=l.table[node.labels])
write.csv(nodes, file=paste(outDir,"gephiNodes_rq.csv",sep=""),row.names=F)
edges<-which(rq.incidence!=0,arr.ind = T)#this gives spurious row names but we dont need anyway
edges<-cbind(edges,rq.incidence[edges])
edges<-cbind(edges,rep("Undirected",length(edges[,1])))
colnames(edges)<-c("Source","Target","Weight","Type")
write.csv(edges, file=paste(outDir,"gephiEdges_rq.csv",sep=""),row.names=F)

# -------------
# DBPedia URIs
# -------------
URIs<- read.csv(paste(dataDir,"All URIs.csv", sep=""), stringsAsFactors=F)
#get distinct subject in alpha order
u.unique<-unique(URIs$subject)
u.unique<-u.unique[order(u.unique)]
write.csv(u.unique,file=paste(outDir,"Distinct Subject URIs.csv", sep=""))
#count occurrences and order by frequency
u.table<-table(URIs$subject)
u.table<-u.table[order(u.table, decreasing=T)]
write.csv(u.table,file=paste(outDir,"Subject URI Frequencies.csv",sep=""))

# .... could do more here

