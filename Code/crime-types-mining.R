library(stats)
library(ggplot2)
library(ggbiplot)

df <- read.csv("Data/crime-types.csv")
# remove locations so dataset contains only the crime type variables 1:14
df3 <- df[, -1]

# finding the areas with the highest crime recording for each type of crime
crimes <- colnames(df3)
hc <- NULL
for (i in 2:(length(crimes) + 1)) {
  area <- df[which(df[, i] == max(df[, i])), ]
  hc <- rbind(hc, data.frame(area))
}

# find variable describing the most variance
summary(df3)
df3.var <- apply(df3, 2, var)
print(df3.var)
max(df3.var)
which(df3.var == max(df3.var))
###############
# PCA no centering or scaling - NOT ADVISED
pca <- prcomp(df3)
print(pca)
plot(pca, type = "lines")
summary(pca)

pca$rotation <- -pca$rotation
pca$x <- -pca$x
biplot(pca, scale = 0)
###############
# PCA version 2
pca2 <- prcomp(df3, center = TRUE, scale. = TRUE)
print(pca2)
plot(pca2, type = "lines")
summary(pca2)
# flip values to positives
pca2.1 <- pca2
pca2.1$rotation <- -pca2.1$rotation
pca2.1$x <- -pca2.1$x
# PCA basic biplot
biplot(pca2.1, scale = 0)
# PCA ggbiplot
bp1 <- ggbiplot(pca2.1, obs.scale = 1, var.scale = 1,
                ellipse = TRUE, circle = TRUE)
bp1 <- bp1 + scale_colour_discrete(name = "") +
  labs(title = "Biplot of PCA output for principal components 1 and 2")
  theme(legend.direction = "horizontal", legend.position = "top")
print(bp1)
# PCA scatter plot
pc12 <- data.frame(pca2.1$rotation[, 1:2])
pc12$crimes <- rownames(pc12)
pt1 <- ggplot(pc12, aes(x = PC1, y = PC2))
pt1 <- pt1 + geom_point(size = 3) +
  geom_text(aes(label = crimes), vjust = 1) +
  labs(title = "Principal components 1 and 2 scatter plot")
print(pt1)
###############
# k-means on principal components 1 and 2
set.seed(7)
pc.km <- kmeans(pc12[, 1:2], 3, nstart = 100)
# basic plot clusters
plot(pc12[, 1:2], col = pc.km$cluster, pch = 20, cex = 2)
# ggplot clusters
pt.km <- ggplot(pc12, aes(x = PC1, y = PC2))
pt.km <- pt.km + geom_point(aes(colour = factor(pc.km$cluster)), size = 3) +
  scale_colour_manual(values = c("green", "red", "blue")) +
  geom_text(aes(label = crimes), vjust = 1) +
  labs(title = "k-means cluster on principal components 1 and 2", colour = "Cluster")
print(pt.km)
###############
# analysis without other_crimes
pc12a <- pc12[which(pc12$crimes != "other_crime"), ]
print(pc12a)

set.seed(8)
pc12a.km <- kmeans(pc12a[, 1:2], 3, nstart = 100)
# basic plot clusters
plot(pc12a[, 1:2], col = pc12a.km$cluster, pch = 20, cex = 2)
# ggplot clusters
pt.km.a <- ggplot(pc12a, aes(x = PC1, y = PC2))
pt.km.a <- pt.km.a + geom_point(aes(colour = factor(pc12a.km$cluster)), size = 3) +
  scale_colour_manual(values = c("blue", "red", "green")) +
  geom_text(aes(label = crimes), vjust = 1) +
  labs(title = "k-means cluster on principal components 1 and 2 (excluding other_crimes variable)", colour = "Cluster")
print(pt.km.a)
###############

