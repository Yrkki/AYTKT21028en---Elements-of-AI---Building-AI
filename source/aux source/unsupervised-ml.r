# Clustering
# 
# Hierarchical clustering
# k-means
# Heatmaps
# Filtering features


# Hierarchical clustering
data("movielens")
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)
x <- movielens %>%
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>%
  select(title, userId, rating) %>%
  spread(userId, rating)
row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

d <- dist(x)

h <- hclust(d)
# We can see the resulting groups using a dendrogram.
plot(h, cex = 0.65, main = "", xlab = "")


groups <- cutree(h, k = 10)
names(groups)[groups==4]
names(groups)[groups==9]

h_2 <- dist(t(x)) %>% hclust()
plot(h_2, cex = 0.65, main = "", xlab = "")


# Sample data
# dat = solar_system
dat = planets_good %>% na.omit()


## k-means

# k-means function
k_means = function (dat, labels, nstart = 25, verbose=FALSE, col=grid_col) {
  dat <- dat %>% as.matrix()
  dat[is.na(dat)] <- 0
  
  centers = length(unique(labels)) + 1
  km <- kmeans(dat, centers = centers, nstart = nstart)
  print(km$size)
  
  if(verbose) {
    print(km$centers)
    print(km$iter)
    print(km$ifault)
    
    mds = cmdscale(dist(dat))
    
    plot(mds, col=km$cluster)
    abline(h=0, col=col, lwd=2)
    abline(v=0, col=col, lwd=2)
    abline(0, 3, lty=2, col=col)
  }
  
  km
}

# usage example (uses <exoplanets>)
planets_samples = dat[c("pl_bmasse", "pl_rade", "pl_bmasse_class", "pl_rade_class")]
km = k_means(planets_samples %>% select(pl_bmasse, pl_rade), class_labels, verbose = F)
planets_samples[, "pl_class_kmeans"] <- factor(km$cluster)
head(planets_samples)



## Heatmaps

# simple heatmap
m = dat %>% select(
  pl_bmasse_log,
  pl_bmasse1_log,
  # pl_bmasse2_log,
  pl_rade_log,
  pl_rade1_log,
  pl_rade2_log,
  pl_volumee_log,
  pl_dens_log,
  pl_dense_log,
) %>% as.matrix()
rownames(m) = dat$pl_name
x_mean_0 <- sweep(m, 2, colMeans(m))
x  <- sweep(x_mean_0, 2, colSds(m), FUN = "/")

h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))
image(x[h_1$order, h_2$order])


dx = dist(x)
sum(is.na(dx))
h_1 <- hclust(dx)

dtx = dist(t(x))
dtx[is.na(dtx)] <- 0
sum(is.na(dtx))
h_2 <- hclust(dtx)
image(x[h_1$order, h_2$order])

heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))


# tree cutting
#
# To generate actual groups we can do one of two things:
#   1) decide on a minimum distance
# needed for observations to be in the same group or
#   2) decide on the number of groups you
# want and then find the minimum distance that achieves this.
h = h_2
plot(h, cex = 0.65)
(groups <- cutree(h, k = 5))
names(groups)[groups==4]

h = h_1
plot(h, cex = 0.65)
(groups <- cutree(h, k = 8^2))
names(groups)[groups==15]

tx = dist(t(x))
tx[is.na(tx)] <- 0
sum(is.na(tx))
dist(dat$pl_bmasse2_log)

# feature ordering
library(genefilter)
heatmap(x)
heatmap(x[order(-rowVars(x))[1:25],])


# color palettes
library(RColorBrewer)
hmcol = colorRampPalette(brewer.pal(9, "GnBu"))(100)
# hmcol = RColorBrewer::brewer.pal(11, "Spectral")

# special heatmap with side colors
library(gplots)
library(rafalib)
cols = palette(brewer.pal(7, "Dark2"))[dat$pl_class]
cbind(rownames(x), cols)
heatmap.2(x, labRow = dat$pl_name,
          trace="none",
          RowSideColors = cols,
          col=hmcol)


## Filtering features
library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:4]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))

