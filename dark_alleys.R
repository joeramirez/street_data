library(RMySQL)
library(zipcode)
library(dplyr)
library(ggmap)

keys<-fromJSON("../keys.json")$street_data

con <- dbConnect(RMySQL::MySQL(),
                 host = keys$host,
                 dbname = keys$dbname,
                 user = keys$id,
                 password = keys$pw)

spooky <- read.csv("halloween_words.csv")
names(spooky) <- "word"
data(zipcode)


query <- paste0("SELECT leftzip, name, count(*) as 'num' FROM address",
                " GROUP BY leftzip, name")
zip_streets <- dbGetQuery(con,query)
zip_streets$name <- tolower(zip_streets$name)

#zsample <- zip_streets[1:50,]
#zsample <- rbind(zs, data.frame(leftzip=0, name='aliens', num=3))


zs<- zip_streets %>%
  rowwise() %>%
  mutate(is_spooky = any(sapply(spooky$word, grepl, name)))

zs$leftzip <- clean.zipcodes(zs$leftzip)
zs2<- merge(zs,zipcode, by.x="leftzip",by.y="zip")
zs3 <- aggregate(num ~ state + is_spooky, data = zs2, sum)
state_spook <- zs3[which(zs3$is_spooky == T),c(1,3)]
names(state_spook) <- c("state","num_spooky")
state_nospook <- zs3[which(zs3$is_spooky == F), c(1,3)]
state_nospook <- merge(state_nospook, state_spook, by=c("state"), all=T)
state_ratio <- mutate(state_nospook, ratio = num_spooky/(num+num_spooky))
state_ratio <- mutate(state_ratio, total = num+num_spooky)
state_ratio$num_spooky[which(is.na(state_ratio$num_spooky))] <- 0
state_ratio$ratio[which(is.na(state_ratio$ratio))] <- 0
state_ratio<- state_ratio[which(!(state_ratio$state %in% c("AA","AE","AP","VI"))),]


#sapply(spooky$word, grepl, "Chatham")
#str(as.data.frame(unlist(lapply(zip_streets$name, function(x){ any(sapply(spooky$word,grepl,x))}))))

# df <- merge(city_ratio,zipcode,by=c('city','state'),all.x=T)
# us<- get_map(location='us')
# 
# ggmap(us, extent = "device") + 
#   geom_density2d(data = df, 
#                  aes(x = longitude, y = latitude), size = 0.3) + 
#   stat_density2d(data = df, 
#                  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
#                  size = 0.01, 
#                  bins = 16, geom = "polygon") + 
#   scale_fill_gradient(low = "green", high = "red") +
#   scale_alpha(range = c(0, 0.3), guide = FALSE)

write.csv(state_ratio,"spooky_state.csv")

