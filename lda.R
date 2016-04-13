
library("tm")
library("lsa")
library("topicmodels")
library("lda")
library("ape")
#library("Rgraphviz")
ape:

setwd("/home/kingfish/Propp/goldStandard")
text <- system.file("texts", "txt", package="tm")

text <- system.file("texts", "txt", package="tm");
corpus <- Corpus(DirSource())
as.matrix(corpus)
inspect(corpus)
#corpus <- tm_map(corpus, stripWhitespace)
dfm <- DocumentTermMatrix(corpus)
as.matrix(dfm)
dfm <- removeSparseTerms(dfm, .99)
dfm_distro <- dist(dfm, method="binary") # euclidian, manhattan,
maximum, canberra, binary, minkowski
#dfm_distro <- dist(dfm)
#dfm_ward <- hclust(dfm_distro, method="ward")
dfm_complete <- hclust(dfm_distro, method="ward")
#dfm_complete <- hclust(dfm_distro, method="mcquitty")
#dfm_single <-hclust(dfm_distro, method="single")
#dfm_mcquitty <- hclust(dfm_distro, method="mcquitty")
#dfm_median <-hclust(dfm_distro, method="median")
#dfm_centroid <-hclust(dfm_distro, method="centroid")



plot(dfm_complete, hang=1, axes = TRUE, ann=TRUE, main = "Cluster
     Dendrogram of Proppian Narreme Matrix",
     xlab="Tale Name", ylab = "DNM Distance")
phyl <- as.phylo(hclust(dfm_distro))



plot(phyl, type="fan", edge.col=c("blue", "green", "red")[c(TRUE,
                                                            FALSE) + 1 + (phyl$edge.length > 20)])


as.matrix(dfm)
#library(sparcl)
#install.packages("sparcl")
#library(cluster)
#install.packages(fpc)
#library(fpc)


lsa <- lsa(dfm, dims = dimcalc_share(share = .5));

lsa_k <- lsa(dfm, dims = dimcalc_kaiser());

plot(dfm);

plot(dfm, corThreshold = 0.5);

plot(dfm, corThreshold = 0.5, terms = findFreqTerms(dtm, 6, Inf));

inspect(corpus)[[20]];

summary(lsa);


#afandtm <- removeSparseTerms(dtm, 0.99);
# CTM - correlated topic models
# afan_CTM <- CTM(dfm[1:25,], k = 5, method = "VEM", control = NULL,
model = NULL);
# afan_CTM <- CTM(dfm[1:25,], k = 5);
# dfm;
#build_graph(afan_CTM, 0, and = TRUE);
#build_graph(afan_CTM, 1, and = TRUE);
#build_graph(afan_CTM, 1, and = FALSE);


#LDA
#library(lda)
#p_LDA <- LDA(dfm[1:25,], control = list(alpha = 0.1), 10);

#post <- posterior(p_LDA, newdata = dfm[-c(1:25),]);

#round(post$topics[1:5,], digits = 2);

#get_terms(p_LDA, 5);


cluster <- cutree(dfm_complete, k=10)
xy <- data.frame(cmdscale(dfm_distro), factor(cluster))
names(xy) <- c("x", "y", "cluster")
xy$model <-rownames(xy)
library(ggplot2)
ggplot(xy, aes(x, y)) + geom_point(aes(colour=cluster), size=3)
#library(A2R)
dim(dfm)
dfm_CTM <- CTM(dfm[1:50], k=5)
post <- posterior(dfm_CTM, newdata = dfm[-c(1:50),])
round(post$topic[1:5], digits=2)
get_terms(dfm_CTM, 5)
