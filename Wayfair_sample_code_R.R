#Install the packages if needed!!!
#install.packages('ggplot2')
#install.packages('dplyr')
library(dplyr)
library(ggplot2)

crime = read.csv(file.choose())

#The dataset contains crime in Cambridge, MA from 2009 to 2016.
str(crime)
summary(crime)
head(crime)

#The field Crime.Date.Time seems to have many different format, so it will be hard to analyze.
#Similar information is available in the field Date.of.Report so I'll drop the first field.
crime = crime[,-3]

#Lets clean the data a little bit. The field date.of.report should be a POSIXct
#Also there is two missing values in reporting area
crime$Date.of.Report = as.character(crime$Date.of.Report)
crime$Date.of.Report = as.POSIXct(crime$Date.of.Report, format = '%m/%d/%Y %I:%M:%S %p')

crime[is.na(crime$Reporting.Area),]
#One of the missing value is an error so I'll remove it. Also as I was looking at missing values
#I realize that some rows have a neighborhood of '', as shown with the next line.
levels(crime$Neighborhood)
crime[crime$Neighborhood == '',]

#It turns out that the missing values were caused by the absence of neighborhood.
crime = crime[!is.na(crime$Reporting.Area),]

#I will also reset my factor levels for neighborhood
crime$Neighborhood = as.factor(as.character(crime$Neighborhood))

#The location field is not very usefull since with already have neighborhood. It would be 
#more useful if it could help us get the coordinates of each crime. I'll drop it.
crime = crime[,-6]

#I realized there are some duplicate files in the data so I'll remove the duplicated rows.
#I found that by looking at the number of levels for the field File.Number compared to number of 
#observations.
crime = unique(crime)
crime$File.Number = as.factor(as.character(crime$File.Number))

#To get a better grasp of the data, lets visualize some of its features
ggplot(crime, aes(x = Neighborhood)) + 
  geom_histogram(stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#It seems the neighborhoods with the highest number of crime are Cambridgeport and East Cambridge.
#However, it could only be because there are more people or because the neighborhood is larger.
crimes_number = crime %>%
  group_by(Crime) %>%
  mutate(count_per_crime = n()) %>%
  slice(1) 

crimes_number = crimes_number[order(-crimes_number$count_per_crime),]
top25_crimes = crimes_number[1:25,]

ggplot(top25_crimes, aes(x = Crime, y = count_per_crime)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust =1))
#The most common crimes are larceny from MV, hit and run and domestic dispute.

#Right now we tried grouping crimes by neighborhood and by type of crime. 
#Lets see if there is a better way to group them by using clustering methods.

#The first method I'll use is hierarchical clustering, because I don't know the number of 
#clusters I'm looking for. Also, considering the amount of observations I have and the time it 
#would take to run I will only take the crimes for 2016.
crime2016 = crime[format(crime$Date.of.Report, '%Y') == "2016",]

#To make sure the clustering methods will work, I will create a temparory column for each 
#factor variables and convert their levels to integers. I will also not use the date.of.report
#and file.number field because it would require more computation.
crime2016$replace_crime = NA
for (i in levels(crime2016$Crime)){
  crime2016$replace_crime[which(crime2016$Crime == i)] = as.integer(crime2016$Crime[crime2016$Crime == i])
}

crime2016$replace_neighbor = NA
for (i in levels(crime2016$Neighborhood)){
  crime2016$replace_neighbor[which(crime2016$Neighborhood == i)] = as.integer(crime2016$Neighborhood[crime2016$Neighborhood == i])
}


dist_matrix = dist(as.matrix(crime2016[,-c(1,2, 3, 5)]))

hc = hclust(dist_matrix)
plot(hc)

#There is no optimal number of clusters, so I'll choose 5.
clusters = cutree(hc, k = 5)
crime2016$cluster = clusters

#It turns out that the algorithm prefers clustering crime by neighborhood.It is possible to see
#the clusters in the next histogram.

ggplot(crime2016, aes(Neighborhood, fill = cluster)) + 
  geom_histogram(stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  scale_fill_gradientn(colours=rainbow(5)) +
  ggtitle('Crimes divided by clusters for 2016(Hierarchical)')

#Another way to come up with clusters is to use the kmeans algorithm. To use this algorithm
#I needed to know the number of clusters, which is why I use hierarchical clustering before.
crime2016 = crime2016[,-6]
km = kmeans(crime2016[,-c(1,2,3, 5)], centers = 5, nstart = 20)
clusters = km$cluster
crime2016$cluster = clusters


ggplot(crime2016, aes(Neighborhood, fill = cluster)) + 
  geom_histogram(stat = 'count') +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  scale_fill_gradientn(colours=rainbow(5)) +
  ggtitle('Crimes divided by clusters for 2016(kmeans)')

#Kmeans also prefers clustering by neighborhood, but the clusters are different. It is 
#important to keep in mind that the clusters created with kmeans will always be different except
#if we set a seed, because it randomly creates the initial clusters.

#In this code sample I did some cleaning and exploration of the data. I then created clusters
#two populare models : hierarchical clustering and kmeans. These clusters represent group 
#of similar neighborhood. I don't know what makes them similar, this would require a deeper
#analysis. However, given the data that I used it could be related to the crimes reported in
#each neighborhood.