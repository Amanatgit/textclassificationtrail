#############################################################
# Data Mining
# Computer Lab Session n°4:
# Mining Data with R: Working with Big Data
#############################################################

#############
# R and SQL #
#############

############################
# SQLite or other SQL-like #

# install.packages("sqldf")
library(sqldf)

# First: save iris data set on your data folder.
write.csv(iris, "iris.csv", quote = FALSE, row.names = FALSE)
iris2 <- read.csv.sql("iris.csv",
                      sql = "select * from file where Species = 'setosa' ")

# install.packages("hflights")
library(hflights)
write.csv(hflights, '~/R/data/hflights.csv', row.names = FALSE)

library(sqldf)
# Destination selection: New York-John F. Kennedy International Airport (JFK)
# Month selection: December (Month=12) 
DecJFKflights <- read.csv.sql('~/R/data/hflights.csv',
                              sql = "select * from file where Dest = '\"JFK\"' and Month=12")

##########
# RMySQL #

library(RMySQL)

conn <- dbConnect(MySQL(), dbname = "cities", username="root",
                  password="")
# of course, the name of the database, of the username and the password need to be changed
# with your own values

dbListTables(conn)
# there is only one table on the data base: villes_france_free

dbListFields(conn, "villes_france_free")
# there are many fields: 27

cities <- dbGetQuery(conn, "SELECT * FROM `villes_france_free`")
# the data frame "cities" will store all the values of the table "villes_france_free"

summary(cities)
head(cities)

#########
# RODBC #

library(RODBC) 
channel <-  odbcConnect("R2MySQL", uid="root", pwd="") 
odbcDataSources(type = c("all", "user", "system")) 
odbcGetInfo(channel)
cities <- sqlQuery(channel, 
                   paste("select * from `villes_france_free`")) 
close(channel) 
head(cities)


################
# R and No SQL #
################

############
# RMongoDB #

# Install rmongodb.
# install.packages("rmongodb")

# Load class library.
library(rmongodb)

# Then, use mongo.create() to create a connection with MongoDB server.
# If it's a local connection, mongo.create() won't need parameters.
mongo <- mongo.create()
# Example for connecting mongodb server remotely:
# mongo<-mongo.create(host="192.168.1.199")

# Check whether the connection is normal.
print(mongo.is.connected(mongo))

################
# Insert data  #

# Define two variables, db and ns.
# db is the database we use, and ns is the database plus data set.
# Define db.
db <- "foobar"
# Define db.collection.
ns <- "foobar.blog"

# Next, we create a JSON object and save it to MongoDB.
#{
#  "_id" : ObjectId("51663e14da2c51b1e8bc62eb"),
#  "name" : "Echo",
#  "age" : 22,
#  "gender" : "Male",
#  "score" : {
#    "Mike" : 5,
#    "Jimmy" : 3.5,
#    "Ann" : 4
#  },
# "comments" : [
#  "a1",
#  "a2",
#  "a3"
#  ]
#}

# Organize class BSON data.
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "Echo")
mongo.bson.buffer.append(buf, "age", 22L)
mongo.bson.buffer.append(buf, "gender", 'Male')

# Object class.
score <- c(5, 3.5, 4)
names(score) <- c("Mike", "Jimmy", "Ann")
mongo.bson.buffer.append(buf, "score", score)

# Array class.
mongo.bson.buffer.start.array(buf, "comments")
mongo.bson.buffer.append(buf, "0", "a1")
mongo.bson.buffer.append(buf, "1", "a2")
mongo.bson.buffer.append(buf, "2", "a3")
mongo.bson.buffer.finish.object(buf)
b <- mongo.bson.from.buffer(buf) # Insert to mongodb.
mongo.insert(mongo,ns,b)


#################
# Display data  #

# Display single inserted data.
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "Echo")
query <- mongo.bson.from.buffer(buf)
print(mongo.find.one(mongo, ns, query))



################
# Modify data  #

# Use the modifiers $inc, $set, and $push to operate. 
# First use $inc to add 1 to age.
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "$inc")
mongo.bson.buffer.append(buf, "age", 1L)
mongo.bson.buffer.finish.object(buf)
objNew <- mongo.bson.from.buffer(buf)
mongo.update(mongo, ns, query, objNew)
print(mongo.find.one(mongo, ns, query))

# Use $set to set age = 1.
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "$set")
mongo.bson.buffer.append(buf, "age", 1L)
mongo.bson.buffer.finish.object(buf)
objNew <- mongo.bson.from.buffer(buf)
mongo.update(mongo, ns, query, objNew)
print(mongo.find.one(mongo, ns, query))

# Use $push to add "Orange" data to the comments array
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "$push")
mongo.bson.buffer.append(buf, "comments", "Orange")
mongo.bson.buffer.finish.object(buf)
objNew <- mongo.bson.from.buffer(buf)
mongo.update(mongo, ns, query, objNew)
print(mongo.find.one(mongo, ns, query))

# Use simplified statements to reassign value to the object.
mongo.update(mongo, ns, query, list(name="Echo", age=25))
print(mongo.find.one(mongo, ns, query))

# Last, delete object and disconnect.
# Delete object.
mongo.remove(mongo, ns, query)
# Destroy mongo connection.
mongo.destroy(mongo)



##########################################
# A Case of Performance Test of Rmongodb #

# Load stringr to achieve character string operation.
library(stringr)

mongo <- mongo.create()

# Insert data in batch.
batch_insert<-function(arr=1:10,ns){
  mongo_insert<-function(x){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "name", str_c("Dave",x))
    mongo.bson.buffer.append(buf, "age", x)
    mongo.bson.buffer.start.array(buf, "comments")
    mongo.bson.buffer.append(buf, "0", "a1")
    mongo.bson.buffer.append(buf, "1", "a2")
    mongo.bson.buffer.append(buf, "2", "a3")
    mongo.bson.buffer.finish.object(buf)
    return(mongo.bson.from.buffer(buf))
  }
  mongo.insert.batch(mongo, ns, lapply(arr,mongo_insert))
}

# Modification in batch of modifier function: $inc.
batch_inc<-function(data,ns){
  for(i in data){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "name", str_c("Dave",i))
    criteria <- mongo.bson.from.buffer(buf)
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.start.object(buf, "$inc")
    mongo.bson.buffer.append(buf, "age", 1L)
    mongo.bson.buffer.finish.object(buf)
    objNew <- mongo.bson.from.buffer(buf)
    mongo.update(mongo, ns, criteria, objNew)
  }
}

# Modification in batch of modifier function: $set.
batch_set<-function(data,ns){
  for(i in data){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "name", str_c("Dave",i))
    criteria <- mongo.bson.from.buffer(buf)
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.start.object(buf, "$set")
    mongo.bson.buffer.append(buf, "age", 1L)
    mongo.bson.buffer.finish.object(buf)
    objNew <- mongo.bson.from.buffer(buf)
    mongo.update(mongo, ns, criteria, objNew)
  }  
}


# Modification in batch of modifier function: $push.
batch_push<-function(data,ns){
  for(i in data){
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.append(buf, "name", str_c("Dave",i))
    criteria <- mongo.bson.from.buffer(buf)
    buf <- mongo.bson.buffer.create()
    mongo.bson.buffer.start.object(buf, "$push")
    mongo.bson.buffer.append(buf, "comments", "Orange")
    mongo.bson.buffer.finish.object(buf)
    objNew <- mongo.bson.from.buffer(buf)
    mongo.update(mongo, ns, criteria, objNew)
  }
}

# All of the three aforementioned modifier programs use for loop in statements,
# so their performance loss in for loop should be the same.
# Thus we'll not take this factor into consideration. 
# Now let's compare the speed of these three modifiers.

# Assign the table.
ns="foobar.blog"

# Time of loop.
data=1:1000

# Clear data.
mongo.remove(mongo, ns)

# Insert in batch.
system.time(batch_insert(data, ns))

# Modifier $inc.
system.time(batch_inc(data, ns))

# Modifier $se.
system.time(batch_set(data, ns))

# Modifier $push.
system.time(batch_push(data, ns))

# Conclusion :
# The speed of these three modifiers is: $push > $set > $inc. 
# Because each of these three modifiers operates on a different class, 
# $push on array, $set on any value and $inc on numbers, 
# all the test results will only serve as a reference.


#####################################
# Illustration: Spatial Data Mining #
#####################################

########################################
# Example 1: Mining French Cities Data #

# Connection with MySQL and cities database

library(RMySQL)

conn <- dbConnect(MySQL(), dbname = "cities", username="root",
                  password="")
# of course, the name of the database, of the username 
# and the password need to be changed with your own values

dbListTables(conn)
# there is only one table on the data base: villes_france_free

dbListFields(conn, "villes_france_free")
# there are many fields: 27

cities <- dbGetQuery(conn, "SELECT * FROM `villes_france_free`")
# the data frame "cities" will store all the values of 
# the table "villes_france_free"

summary(cities)
head(cities)

######################
# R packages for GIS #
library(RgoogleMaps) # - Provide a comfortable R interface to query
#                        the Google server for static maps
#                      - Use the map as a background image to overlay
#                        plots within R.
library(sp)          # Classes and Methods for Spatial Data
library(ggplot2)     # Implementation of the Grammar of Graphics
library(ggmap)       # Spatial Visualization with ggplot2
library(maps)        # Draw Geographical Maps

# Information for "Saint-Etienne":
cities[cities$ville_slug == "saint-etienne", ]
# => ville_departement: 42 (Loire)

# Loire department = 42
Loire <- cities[cities$ville_departement =="42",]


##############################################
# Select the relevant zoom level for the map #

# Zoom on Saint-Etienne satellite map:
ste_map <- get_map(location="saint-etienne, loire", 
                   maptype="satellite", 
                   zoom = 20)
plot(ste_map)
# - entrance of th city hall building, 
# - North part of the "Place de l'Hôtel de ville"

win.graph(800,600,10)
# Zoom on Saint-Etienne satellite map:
ste_map <- get_map(location="saint-etienne, loire", 
                   maptype="satellite", 
                   zoom = 17)
plot(ste_map)
# - city hall (on the center), 
# - Place Jean Jaures (on the top)
# - Saint-Charles cathedral (on the top left)
# - Place de l'Hôtel de ville (on the bottom)


# Zoom on Saint-Etienne satellite map:
ste_map <- get_map(location="saint-etienne, loire", 
                   maptype="satellite", 
                   zoom = 14)
plot(ste_map)
# the Cité du Design on the top (after Carnot train station)


france_map <- get_map(location="France", 
                   maptype="toner", 
                   zoom = 6)
plot(france_map)

# maptype: "terrain", "satellite", "roadmap", "hybrid", "watercolor", "toner" 


# The Loire department: changing the level of the zoom to 8
loire_map <- get_map(location="saint-etienne, loire", 
                     maptype="satellite", 
                     zoom = 8)
plot(loire_map)

# The Loire department: changing the level of the zoom to 8
loire_map2 <- get_map(location="saint-etienne, loire", 
                     maptype="terrain", 
                     zoom = 8)
plot(loire_map2)


# With the cities in red
ggmap(loire_map, 
      extent = "device") + 
  geom_point(aes(x = Loire$ville_longitude_deg, 
                 y = Loire$ville_latitude_deg), 
             colour = "red", 
             alpha = 0.1, size = 10, data = Loire)


ggmap(loire_map, 
      extent = "device") + 
  geom_density2d(data = Loire, 
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg),
                 size = 0.3) + 
  stat_density2d(data = Loire,
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg, fill = ..level.., alpha = ..level..),
                 size = 0.01, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


# Without the villages (population less than 2000 inhabitants)
LoireCities <- Loire[Loire$ville_population_2012 >= 2000, ]
ggmap(loire_map, 
      extent = "device") + 
  geom_density2d(data = LoireCities, 
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg),
                 size = 0.3) + 
  stat_density2d(data = LoireCities,
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg, 
                     fill = ..level.., alpha = ..level..),
                 size = 0.01, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# => There are two basins of attraction in the Loire departement:
# on the North, around Roanne
# on the South, around Saint-Etienne (the big one)

# On a map with name (not a satellite map):
loire_map2 <- get_map(location="saint-etienne, loire", zoom = 8)
plot(loire_map2)
ggmap(loire_map2, extent = "device") + 
  geom_density2d(data = LoireCities, 
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg),
                 size = 0.3) + 
  stat_density2d(data = LoireCities,
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg, 
                     fill = ..level.., alpha = ..level..),
                 size = 0.01, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)


# If we take into account the cities of the other French departments
# in the Auvergne-Rhône-Alpes Region:
plot(loire_map2)
FrenchCities <- cities[cities$ville_population_2012 >= 2000, ]
ggmap(loire_map2, extent = "device") + 
  geom_density2d(data = FrenchCities, 
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg),
                 size = 0.3) + 
  stat_density2d(data = FrenchCities,
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg, 
                     fill = ..level.., alpha = ..level..),
                 size = 0.01, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# Saint-Etienne is on the basin of attraction of Lyon


# For the Metropolitan France 
france_map <- get_map(location = "france", zoom = 6)
# Try:
# france_map <- get_map(location = "france", maptype = "satellite", zoom = 6)
plot(france_map)

ggmap(france_map, extent = "device") + 
  geom_density2d(data = FrenchCities, 
                 aes(x = ville_longitude_deg,
                     y = ville_latitude_deg),
                 size = 0.3) + 
  stat_density2d(data = FrenchCities,
                 aes(x = ville_longitude_deg, 
                     y = ville_latitude_deg, 
                     fill = ..level.., alpha = ..level..),
                 size = 0.01, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# The most important basin of attraction is Paris
# but there are other poles:
# Lyon
# Lille (on the North)
# Nantes (on the West)
# Bordeaux (on the South-West)
# Toulouse (South)
# Marseille, Aix-en-Provence and Montpellier (South and South-East)
# Strasbourg (East) 
ggmap(france_map, extent = "device") + 
  geom_point(aes(x = FrenchCities$ville_longitude_deg,
                 y = FrenchCities$ville_latitude_deg), 
             colour = "red",
             alpha = 0.1, 
             size = 5, 
             data = FrenchCities)

# With all the communes
ggmap(france_map, extent = "device") + 
  geom_point(aes(x = cities$ville_longitude_deg,
                 y = cities$ville_latitude_deg), 
             colour = "red",
             alpha = 0.5, size = 1, data = cities)

# With cities with more than 20,000 inhabitants
BigCities <- FrenchCities[FrenchCities$ville_population_2012 >= 20000, ]
ggmap(france_map, extent = "device") + 
  geom_point(aes(x = BigCities$ville_longitude_deg,
                 y = BigCities$ville_latitude_deg), 
             colour = "red", 
             alpha = 0.5, size = 10, data = BigCities)

# With cities with more than 100,000 inhabitants
LargestCities <- FrenchCities[FrenchCities$ville_population_2012 >= 100000, ]
ggmap(france_map, extent = "device") + 
  geom_point(aes(x = LargestCities$ville_longitude_deg,
                 y = LargestCities$ville_latitude_deg), 
             colour = "red", 
             alpha = 0.5, size = 10, data = LargestCities)

# With cities with an altitude up to at least 500 meters
HigherCities <- FrenchCities[FrenchCities$ville_zmax >= 500, ]
summary(HigherCities$ville_zmax)
HigherCities <- na.omit(HigherCities)
summary(HigherCities$ville_zmax)

ggmap(france_map, extent = "device") + 
  geom_point(aes(x = HigherCities$ville_longitude_deg,
                 y = HigherCities$ville_latitude_deg), 
             colour = "red", 
             alpha = 0.1, size = 10, data = HigherCities)
# => cities of the Pyrénées (South-West), the Massif central (in the center), the Alpes,
# the Jura and the Vosges (at the East)... and cities of Corsica (the island in the South-East)



france_map <- get_map(location = "france", maptype = "hybrid", zoom = 6)
plot(france_map)

summary(cities$ville_population_2012)
hist(cities$ville_population_2012)

# Remove cities with no inhabitants
cities <- cities[cities$ville_population_2012 > 0,]
summary(cities$ville_population_2012)

hist(log(cities$ville_population_2012))
cities$log_pop <- log(cities$ville_population_2012)
summary(cities$log_pop)

pop_min <- min(cities$log_pop)
pop_max <- max(cities$log_pop)

cities$pop_graph <- (cities$log_pop - pop_min) / (pop_max - pop_min) 
hist(cities$pop_graph)
hist(sqrt(cities$pop_graph))

summary(sqrt(cities$pop_graph))

ggmap(france_map, extent = "device") + 
  geom_point(aes(x = cities$ville_longitude_deg,
                 y = cities$ville_latitude_deg), 
             colour = gray(sqrt(cities$pop_graph)),
             alpha = 0.8, size = 2, data = cities)   

######################
# Population density #
             
win.graph(800,600,10)
hist(cities$ville_densite_2010)
hist(log(cities$ville_densite_2010))
cities <- cities[cities$ville_densite_2010 > 0,]
cities$log_dens <- log(cities$ville_densite_2010)
summary(cities$log_dens)

dens_min <- min(cities$log_dens)
dens_max <- max(cities$log_dens)

cities$dens_graph <- (cities$log_dens - dens_min) / (dens_max - dens_min) 
hist(cities$dens_graph)
summary(cities$dens_graph)

france_map <- get_map(location = "france", 
                      maptype = "hybrid", 
                      zoom = 6)
plot(france_map)

ggmap(france_map, extent = "device") + 
  geom_point(aes(x = cities$ville_longitude_deg,
                 y = cities$ville_latitude_deg), 
             colour = gray(sqrt(cities$dens_graph)),
             alpha = 0.8, size = 2, data = cities)   

# With cities with more than 100,000 inhabitants
LargestCities <- FrenchCities[FrenchCities$ville_population_2012 >= 100000, ]


ggmap(france_map, extent = "device") + 
  geom_point(aes(x = cities$ville_longitude_deg,
                 y = cities$ville_latitude_deg), 
             colour = gray(sqrt(cities$dens_graph)),
             alpha = 0.8, size = 2, data = cities) +   
  geom_point(aes(x = LargestCities$ville_longitude_deg,
                 y = LargestCities$ville_latitude_deg), 
             colour = "red", 
             alpha = 0.8, size = 2, data = LargestCities)


########################################### 
# Example 2: Mining Worldwide Cities Data #

library(RMySQL)

conn <- dbConnect(MySQL(), dbname = "worldcities", username="root",
                  password="")
# of course, the name of the database, of the username and
# the password need to be changed with your own values

dbListTables(conn)
# there is only one table on the data base: cities

dbListFields(conn, "cities")
# there are 6 fields

France <- dbGetQuery(conn, 
  "SELECT * FROM `cities` WHERE `country_code` = \"fr\"")

Algeria <- dbGetQuery(conn, 
 "SELECT * FROM `cities` WHERE `country_code` = \"dz\"")

str(Algeria)

Algeria$region <- as.numeric(Algeria$region)
Algeria$population <- as.numeric(Algeria$population)
Algeria$latitude <- as.numeric(Algeria$latitude)
Algeria$longitude <- as.numeric(Algeria$longitude)

str(Algeria)


library(RgoogleMaps)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)

algeria_map <- get_map(location="Algeria", maptype="satellite", zoom = 5)
plot(algeria_map)

algeria_map2 <- get_map(location="Algeria",  zoom = 5)
plot(algeria_map2)

# With the cities in red

ggmap(algeria_map, extent = "device") + 
  geom_point(aes(x = Algeria$longitude,
                 y = Algeria$latitude),
             colour = "red", 
             alpha = 0.1, size = 3,
             data = Algeria)

