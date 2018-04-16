###install.packages("zipcode");
###install_github('hrbrmstr/albersusa')
library(ggplot2);library(evaluate); library(mapproj); library(fiftystater)
library(zipcode);library(ggmap);library(tidyverse)
fn<- "C:/Users/Thomas/Documents/Github/Wang499/OrgNPVs2.csv";### choose the npv data ####
dat<-read_csv(fn)
str(dat);
drop.idx<-c(2,4,5,7,16,17,19,20,24,25);
#dat.keep<-dat[]
data(zipcode)
npv<-dat$NPV;
idx.zip.keep<-which(substr(zipcode$zip,5,5)==1); ## only look at those with 1 at the last number
tru.zip<-trunc(as.numeric(zipcode$zip[idx.zip.keep])/100)*100 ## remove the last two number to match
state_zip.ref.dup<-data.frame(zipcode$state[idx.zip.keep],tru.zip); ## keep those we want
colnames(state_zip.ref.dup)<-c('state','zip');

state_zip.ref<-state_zip.ref.dup[!duplicated(state_zip.ref.dup),];

head(state_zip.ref);

colnames(dat) <- make.names(colnames(dat))
#x<-c(1,2,3,3,3,2,1,2,3,6)+4; y<-data.frame(c(1,2,3)+4,c('a','b','c'))
#match(x,y[,1]);
zip.npv<-clean.zipcodes(as.numeric(dat$Postal.Code))
idx.state<-match(zip.npv,state_zip.ref[,2]); 
## head(zip.npv);state_zip.ref[head(idx.state),2]; ## they are matched
zip_state.npv<-data.frame(zip.npv,state_zip.ref[idx.state,1],dat$NPV);
colnames(zip_state.npv)<-c('zip','state','NPV');
#head(zip_state.npv);
zip_state.npv.com<-zip_state.npv[complete.cases(zip_state.npv),]
state.avg.npv<-tapply(zip_state.npv.com$NPV, zip_state.npv.com$state, mean);
state.avg.npv.com<-state.avg.npv[complete.cases(state.avg.npv)]
state<-rownames(state.avg.npv.com);
state.51.raw<-append(as.vector(tolower(na.omit(state.name[match(state,state.abb)]))),'district of columbia',after=7); 
## state.name and state.abb is matched
state.51.map<-unique(fifty_states$id)## the states name the map is using
id.swap<-match(state.51.map,state.51.raw)
#state.51<-state.51.raw[match(state.51.map,state.51.raw)]
#data.frame(state.51,state.51.map) # to show that id.swap is working
npvavg.df<-data.frame(state.avg.npv.com[-c(12,41)][id.swap]/1000,state.51.map);
colnames(npvavg.df)<-c("avg",'state');
#npvavg.df

state.sd.npv<-tapply(zip_state.npv.com$NPV, zip_state.npv.com$state, sd);
state.sd.npv.com<-state.sd.npv[complete.cases(state.sd.npv)]
npvsd.df<-data.frame(state.sd.npv.com[-c(12,41)][id.swap]/1000,state.51.map);
colnames(npvsd.df)<-c("sd",'state');

#########################################
##### the average map of 51 states ##################
#########################################
# map_id creates the aesthetic mapping to the state name column in your data
p <- ggplot(npvavg.df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = avg), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank());

# add border boxes to AK/HI
p + fifty_states_inset_boxes() + scale_fill_distiller(palette = "Spectral")
#########################################
##### the sd map ########################
#########################################
p <- ggplot(npvsd.df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = sd), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank());

npvsr.df <- data.frame(npvavg.df[,1]/npvsd.df[,1],npvavg.df[,2])
colnames(npvsr.df) <- c("ratio", "state")
# add border boxes to AK/HI
p + fifty_states_inset_boxes() + scale_fill_distiller(palette = "Spectral")
#####
p <- ggplot(npvsr.df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = ratio), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank());

# add border boxes to AK/HI
p + fifty_states_inset_boxes() + scale_fill_distiller(palette = "Spectral")

######Thomas 2/19/18#####
#Checking the difference between the average plot and SD plot
#Output both into Research Log in Google Drive
#It now looks like the two charts are significantly different


#Looking at the proportions of the states frequency
props <- prop.table(table(zip_state.npv.com$state))
hist(props[which(props >= 0.005)], breaks = 20)
suff <- dimnames(props)[[1]][which(props >= 0.005)]
keep = c()
#for some reason this code crashes my computer
keepr <- sapply(zip_state.npv.com$state, function(x){
  if(x %in% suff){
    keep <- append(keep,x)
  }
})
#Note that CA is the outlier with 17 percent of the data
######End of Thomas######



#################################
########### zip code? ######
#################################
library(zipcode); library(tidyverse) ;library(maps)
library(viridis); library(ggthemes) ;library(albersusa)
library(googleVis)





## dat.npv.zip[which(dat.npv.zip$state!=dat.npv.zip$dat.State),]
## there are situations which the state is mislabled.

mean.npv.zip<-round(tapply(dat$NPV,dat$Postal.Code, mean),0);
head(mean.npv.zip);

#idx.zip<-match(zip.npv,zipcode$zip);
#head(data.frame(mean.npv.zip,zip.npv,zipcode[idx.zip,]))
dup.zip<-duplicated(substr(zipcode$zip,1,3));

zipcode.nondup<-zipcode[!dup.zip,];
zipcode.nondup$zip<-substr(zipcode.nondup$zip,1,3)

zip.npv<-substr(clean.zipcodes(as.numeric(rownames(mean.npv.zip))),1,3);
idx.2zip<-match(zip.npv,zipcode.nondup$zip);
dat.mean.zip<-data.frame(mean.npv.zip,zipcode.nondup[idx.2zip,])
dat.mean.zip$LatLong<-apply(round(cbind(dat.mean.zip$latitude,dat.mean.zip$longitude),2), 1, paste, collapse=":")

dim(dat.mean.zip)
head(dat.mean.zip)

##### picture of the whole USA #########
pic.mean.npv.zip <- gvisGeoChart(dat.mean.zip, "LatLong",
                                 colorvar='mean.npv.zip',
                                 hovervar="city",
                                 options=list(region="US",  
                                              resolution='metros',
                                              displayMode='marker', ##change it to 'marker'
                                              colorAxis="{colors:['red','orange','yellow','green', 'blue']}"))

  plot(pic.mean.npv.zip)
  ##### picture of any state ######
  st.name="NC";
  dat.mean.zip.nc<-dat.mean.zip[which(dat.mean.zip$state==st.name),]
  pic.mean.npv.zip.nc <- gvisGeoChart(dat.mean.zip.nc, locationvar="LatLong",
                                   colorvar='mean.npv.zip',
                                   hovervar="city",
                                   options=list(region=paste("US",st.name,sep='-'),  
                                                colorAxis="{colors:['red','orange','yellow','green', 'blue']}",
                                                resolution='metros',
                                                displayMode='marker'))
  
  plot(pic.mean.npv.zip.nc)


  st.name="NC";
  dat.mean.zip.nc<-dat.mean.zip[which(dat.mean.zip$state==st.name),]
  pic.mean.npv.zip.nc.map <- gvisGeoMap(dat.mean.zip.nc, locationvar="LatLong",
                                      numvar='mean.npv.zip',
                                     # hovervar="city",
                                      options=list(region=paste("US",st.name,sep='-'),  
                                                   colors="{colors:['red','orange','yellow','green', 'blue']}",
                                                   #resolution='metros',
                                                   mapType='terrain',
                                                   useMapTypeControl=TRUE,
                                                   dataMode='marker'))
  
  plot(pic.mean.npv.zip.nc.map)
  

################ my code part stops here #######



