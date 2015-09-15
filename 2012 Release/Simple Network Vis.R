## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2012, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************

# Some basic network visualisations of the LAK Dataset

# This code is also available from Github: https://github.com/arc12/LAK-Dataset
# See also http://www.solaresearch.org/resources/lak-dataset/

library(network)

load(file="LAK-Dataset.RData")

#a decoder from proceedings origin to venue (EDM, LAK, JETS)
origin<-c("http://data.linkededucation.org/resource/lak/conference/lak2011/proceedings"    ,
          "http://data.linkededucation.org/resource/lak/conference/lak2012/proceedings"    , 
          "http://data.linkededucation.org/resource/lak/conference/edm2008/proceedings" ,
          "http://data.linkededucation.org/resource/lak/conference/edm2009/proceedings" ,
          "http://data.linkededucation.org/resource/lak/conference/edm2010/proceedings" ,
          "http://data.linkededucation.org/resource/lak/conference/edm2011/proceedings" ,
          "http://data.linkededucation.org/resource/lak/conference/edm2012/proceedings" ,
          "http://data.linkededucation.org/resource/lak/specialissue/jets12/proceedings")
venue<-c("LAK","LAK","EDM","EDM","EDM","EDM","EDM","JETS")
venue.poly<-c(3,3,4,4,4,4,4,5)#network node polygon sides

#quick view of authorship network
# net1<-network(authorship[,c(1,3)])
# plot(net1, vertex.cex=0.6, arrowhead.cex=0.5)

#make colour of papers be different
#make shape of paper nodes be determined by venue: LAK, EDM, JETS
plot2<-function(net, authorship.table, filename=NA){
   people.bool<-(network.vertex.names(net) %in% levels(authorship.table[,1]))
   #people are green and papers are red
   vertex.col<-2+people.bool #adding a boolean adds 0 or 1
   #people are circles and venue.poly gives sides for papers
   vertex.sides<-rep(50,length(network.vertex.names(net)))#default is circles for people
   for(o in 1:length(origin)){
      vertex.sides[network.vertex.names(net) %in% authorship.table[authorship.table[,4]==origin[o],3]] <-venue.poly[o]
   }
   plot(net, vertex.cex=1.0-0.4*people.bool, arrowhead.cex=0.5, vertex.col=vertex.col, vertex.sides=vertex.sides, vertices.last=FALSE)
   if(!is.na(filename)){
      png(filename=filename, width=1000, height=1000)
      plot(net, vertex.cex=1.0-0.4*people.bool, arrowhead.cex=0.5, vertex.col=vertex.col, vertex.sides=vertex.sides, vertices.last=FALSE)
      dev.off()
   }
}

#all data
# net2<-network(authorship[,c(1,3)])
# plot2(net2,authorship)

#filter down to 2011 and 2012
filter.papers<-row.names(papers[papers[,"year"] %in% c(2011,2012),]) #URIs
authorship.filtered<-authorship[authorship[,3] %in% filter.papers,]
net3<-network(authorship.filtered[,c(1,3)])
plot2(net3, authorship.filtered, "LDK EDM and JETS 2011 and 2012.png")