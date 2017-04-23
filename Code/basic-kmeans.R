setwd("C:/Users/davem/Projects/201703_crime-data/")
library(stats)

df <- read.csv("Data/crime-types.csv")

df2 <- df[rowSums(df[, 2:15]) > 0, ] # no observations have zero crimes
# trim the dataset for easier kmeans visualization
df2 <- df[which(df$drugs > 25 & df$vehicle_crime > 25), c("drugs", "vehicle_crime")]

pt1.1 <- ggplot(df, aes(x=bicycle_theft, y=drugs))
pt1.1 + geom_point()

pt1.2 <- ggplot(df, aes(x=vehicle_crime, y=drugs))
pt1.2 + geom_point()

# plot before kmeans clustering
pt2 <- ggplot(df2, aes(x=vehicle_crime, y=drugs))
pt2 + geom_point()

# KMEANS CLUSTERING
set.seed(7)
km2 <- kmeans(df2, 3, nstart=20)
plot(df2, col=km2$cluster, pch=20, cex=2)

# big multi faceted plot - not helpful
# km3 <- kmeans(df3, 6, nstart=20)
# plot(df3, col=(km2$cluster))