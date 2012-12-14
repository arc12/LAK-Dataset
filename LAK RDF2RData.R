## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2012, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************

#A short piece of code to extract from the LAK Dataset, which is RDF, and to create data.frames for saving as .RData
#See http://www.solaresearch.org/resources/lak-dataset/

library("rrdf")

triples<-combine.rdf(
                     combine.rdf(load.rdf("Data/lak2011_fulltext.rdf", "RDF/XML"),
                                 load.rdf("Data/lak2012_fulltext.rdf", "RDF/XML")
                                 ),
                     load.rdf("Data/jets12_fulltext.rdf", "RDF/XML")
                     )
summarize.rdf(triples)

#these should be the times the source data files were created from the triple store
mtimes<-list(lak2011=file.info("Data/lak2011_fulltext.rdf")$mtime,
             lak2012=file.info("Data/lak2012_fulltext.rdf")$mtime,
             jets12=file.info("Data/jets12_fulltext.rdf")$mtime)

#these queries use OPTIONAL in case some content is missing. This may be unnecessary.
#people and papers have row.names set to the subject identifier but authorship has no such

#extract the people (but not their authorship)
people.query<- paste("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
                     "PREFIX swrc: <http://swrc.ontoware.org/ontology#>",
                     "PREFIX foaf: <http://xmlns.com/foaf/0.1/>",
                        "SELECT ?person ?name ?location ?affiliation WHERE {",
                              "?person rdf:type foaf:Person;",
                              "foaf:name ?name .",
                                 "OPTIONAL{",
                                 "?person foaf:based_near ?location;",
			                        "swrc:affiliation ?affiliation;",
                                 "foaf:firstName ?firstName;",
                                 "foaf:lastName ?lastName",
                                 "}",
                         "}")
people<-as.data.frame(sparql.rdf(triples,people.query,rowvarname="person"))

#extract the papers
papers.query<-paste("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
                    "PREFIX swrc: <http://swrc.ontoware.org/ontology#>",
                    "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
                    "PREFIX swc: <http://data.semanticweb.org/ns/swc/ontology#>",
                    "PREFIX led: <http://data.linkededucation.org/ns/linked-education.rdf#>",
                        "SELECT ?paper ?origin ?month ?year ?title ?abstract ?content WHERE {",
                           "?paper rdf:type swrc:InProceedings;",
                           "swc:isPartOf ?origin;",
                           "dc:title ?title;",
                           "swrc:abstract ?abstract;",
                           "swrc:month ?month;",
                           "swrc:year ?year .",
                           "OPTIONAL{?paper led:body ?content}",
                         "}")

papers<-as.data.frame(sparql.rdf(triples,papers.query,rowvarname="paper"))

#extract the authorship. a data.frame isn't elegant for doing anything with; should be transformed to a network object of some kind
authorship.query<-paste("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
                        "PREFIX swc: <http://data.semanticweb.org/ns/swc/ontology#>",
                        "PREFIX foaf: <http://xmlns.com/foaf/0.1/>",
                           "SELECT ?person ?paper ?origin WHERE {",
                              "?person rdf:type foaf:Person;",
                              "foaf:made ?paper .",
                              "OPTIONAL{?paper swc:isPartOf ?origin}",
                           "}")

authorship<-as.data.frame(sparql.rdf(triples,authorship.query))

save(people, papers, authorship, mtimes, file="LAK Dataset.rData")


  

