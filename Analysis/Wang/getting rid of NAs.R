library(ggplot2);library(evaluate); library(mapproj); library(fiftystater)
library(zipcode);library(ggmap);library(tidyverse)
fn<- "C:/Users/Thomas/Documents/Github/Wang499/OrgNPVs2.csv";### choose the npv data ####
dat<-read_csv(fn)
dat <- dat[-which(is.na(dat$`Postal Code`)),]
str(dat);
drop.idx<-c(2,4,5,7,16,17,19,20,24,25);
#dat.keep<-dat[]
data(zipcode)
npv<-dat$NPV;
f2 <- as.character(00:999)
sapply(1:100, function(x){
  f2[x] <<- paste("0", x-1, sep = "")
})
sapply(1:10, function(x){
  f2[x] <<- paste("00", x-1, sep = "")
})
f2
idx.zip.keep2 <- sapply(f2, function(x){
  app <- which(substr(zipcode$zip, 1, 3) == x)
  return(app[1])
})
repz <- idx.zip.keep2[!is.na(idx.zip.keep2)]
refdf <- rbind(names(repz),zipcode$zip[repz])
sapply(1:dim(refdf)[2],function(x){
  refdf[1,x] <<- paste(refdf[1,x], "00", sep = "")
})
refdf[,1:10]
sapply(1:dim(refdf)[2], function(x){
  gr <- which(zipcode$zip == refdf[2,x])
  refdf[2,x] <<- zipcode$state[gr]
})
refdf <- data.frame(refdf[2,], refdf[1,])
refdf[,2] <- as.numeric(paste(refdf[,2]))
head(refdf)
colnames(refdf) <- c('state', 'zip')


colnames(dat) <- make.names(colnames(dat))
#x<-c(1,2,3,3,3,2,1,2,3,6)+4; y<-data.frame(c(1,2,3)+4,c('a','b','c'))
#match(x,y[,1]);
zip.npv<-as.numeric(clean.zipcodes(as.numeric(dat$Postal.Code)))
idx.state<-match(zip.npv,refdf[,2])

## head(zip.npv);state_zip.ref[head(idx.state),2]; ## they are matched
zip_state.npv<-data.frame(zip.npv,refdf[idx.state,1],dat$NPV);
colnames(zip_state.npv)<-c('zip','state','NPV');
#head(zip_state.npv);
state.avg.npv<-tapply(zip_state.npv$NPV, zip_state.npv$state, mean);
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

#Additional Investigation
sum(sapply(unique(zip_state.npv$state), function(x){
  length(which(zip_state.npv$state == x)) <= 553
}))
length(which(zip_state.npv$state == "SD"))
#note that SD had the fourth fewest total number of loans at 533, 
#so that likely contributes to the odd result