library(dplyr)

#kolesa_df<-read.csv('C:\\Users\\shakraz\\Documents\\KnnR\\kolesa_df.csv',encoding = 'UTF-8')
# kolesa_df<-unique(kolesa_df)
# kolesa_df$car_id<-regmatches(kolesa_df$url, regexpr('\\d+', kolesa_df$url,perl=T))
# type_id<-kolesa_df[grep('\\d+', kolesa_df$type,perl=T),]$car_id
# t<-head(kolesa_df[order(kolesa_df$price, decreasing = T),]$car_id,10)
# t1<-head(kolesa_df[order(kolesa_df$price, decreasing = F),]$car_id,10)
# 
# kolesa_df<-filter(kolesa_df, !car_id %in% c(t,t1,type_id))
# 
# head(kolesa_df)


# sim_model<-as.matrix(dist(kolesa_df[1:1000,c('year', 'price', 'volume2')]))
# 
# rownames(sim_model)<-colnames(sim_model)<-kolesa_df[1:1000,c('car_id')]
# sim_model<-data.frame(sim_model)
# colnames(sim_model)<-gsub('X', '', colnames(sim_model), perl=T)
# sim_model$id<-rownames(sim_model)
# 
# rownames(sim_model)<-NULL


#write.csv(sim_model,paste0(getwd(),'/sim_model.csv'))

kolesa_df$volume<-sapply(kolesa_df$volume2, function(x) x/(10*x%/%10) )
181%%10

nrow(filter(kolesa_df, volume2>10000))

kolesa_df$year2<-kolesa_df$year-1900

kolesa_df$volume2/10*kolesa_df$volume2%/%10
nchar(12)
kolesa_df$volume<-round(kolesa_df$volume2/10^(nchar(kolesa_df$volume2)-1),1)



getDist<-function(id1, id2){
  row<-filter(kolesa_df, car_id==id1)
  row2<-filter(kolesa_df, car_id==id2)
  
  return(dist(select(rbind(row, row2), price, year2, volume), method= 'canberra'))
}

head(kolesa_df)
getDist(28069084,27862646)

nrow(t)
t<-merge(kolesa_df[1:1000,c('car_id')], kolesa_df[1:1000,c('car_id')],all=T)
t<-filter(t, t$x!=t$y)

t<-t[order(t$x),]

dim(sim_model2)


sim_model2<-t[1:1000,]
sim_model2$sim<-mapply(getDist,sim_model2[,1], sim_model2[,2])






