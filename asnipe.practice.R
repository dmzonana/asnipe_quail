install.packages("asnipe")
library("asnipe")
library("chron")

data("group_by_individual")
str(gbi)
data("times")
networks<-array(0,c(2, ncol(gbi),ncol(gbi)))
networks[1,,]<-get_network(gbi,data_format="GBI",association_index="SRI",
                           times=times,start_time=0,end_time=max(times)/2)

library(igraph)
net<-graph.adjacency(networks[1,,], mode="undirected",diag=FALSE,weighted=TRUE)
deg_weighted<-graph.strength(net)
detach(package:igraph)

data("identified_individuals")
identified_individuals$Loc_date <-
  paste(identified_individuals$Location,
        identified_individuals$Date,sep="_")

global_ids <- levels(identified_individuals$ID)
identified_individuals <- identified_individuals[which(identified_individuals$Date < 2),]

gmm_data <- gmmevents(time=identified_individuals$Time,
                      identity=identified_individuals$ID,
                      location=identified_individuals$Loc_date,
                      global_ids=global_ids)

gbi<-gmm_data$gbi
events<-gmm_data$metadata


##Trying to convert my data into something for asnipe
d <- read.csv('~/Downloads/gfwb.bgmmlabels.csv', header = TRUE)

#convert dates to Julian dates in order to make a location_date column#
letsee<-as.Date(d$date,format="%m/%d/%y")
d$julian<-format(letsee,"%j")

#convert my time column to minutes since midnight#
d$sincemidnight<-round(24*60*times(d$time))

#save RFID factors as integer indeces#
index<-as.numeric(d$id)
d$index<-index

#label location#
d$trap<-rep("gfwb",nrow(d))

#add location_date column#
d$Loc_date <-
  paste(d$trap,
        d$julian,sep="_")
d$Loc_date<-as.factor(d$Loc_date)

#try and for-loop through, saving birdXbird matrix for each Loc_date#
dates<-unique(d$Loc_date)
global_ids<-unique(d$index)
networks<-array(0,c(length(unique(d$Loc_date)),length(global_ids),length(global_ids)))

for (i in 1:length(dates)){
  subd<-d[d$Loc_date==dates[i],]
  birds<-length(unique(subd$id))
  if (birds==1) {
    next
  }else{
    gmm_data <- gmmevents(time=subd$sincemidnight,
                          identity=subd$index,
                          location=subd$trap,
                          global_ids=global_ids)
    gbi<-gmm_data$gbi
    if(dim(gbi)[1]==1){
      next
    }else{
    networks[i,,]<-get_network(gbi,data_format="GBI",association_index="SRI")
  }
  }
}

#Add across stacks - syntax is still confusing to me, trial and error to get this#
sum<-apply(networks,2:3,sum)

#Have a look at the network#
library(igraph)
library(scales)
net<-graph.adjacency(sum, mode="undirected",diag=FALSE,weighted=TRUE)
l<-layout_with_fr(net)
par(mar=c(2, 1, .5, .5))
plot.igraph(net, edge.color=alpha('black', .7), 
            vertex.size=2, vertex.label.family="Arial Black",vertex.label=NA, layout=l*8, edge.width=E(net)$weight/5)
plot(net,rescale=F,layout=l*0.4)
deg_weighted<-graph.strength(net)
detach(package:igraph)

#Playing around with single subsetted date#
subd<-d[d$Loc_date=="gfwb_191",]
gmm_data <- gmmevents(time=subd$sincemidnight,
                      identity=subd$index,
                      location=subd$Loc_date,
                      global_ids=global_ids)
gbi<-gmm_data$gbi
net<-get_network(gbi,data_format="GBI",association_index="SRI")
