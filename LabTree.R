library(textmineR)
library(ape)
library(ggplot2)
library(ggdendro)
library(dendextend)
library(SoundexBR)
library(sparcl)


# Reading my csv file with the list of papers
papers <- read.csv(file = "GustavoDeco_papers_summary.csv", head = TRUE, sep=",") 

titles <- papers$Title
citations <- papers$Citations
years <- papers$Year
topics <- papers$Topic
colors <- papers$Color
abstracts <- papers$Abstract
keywords <- papers$Keywords
names <- papers$Name
branches <- papers$Branch

# Numerically representing text
corpus = tm::Corpus(tm::VectorSource(keywords))  #<---- topics, keywords, colors, abstracts
tdm <- tm::DocumentTermMatrix(corpus) 
tdm.tfidf <- tm::weightTfIdf(tdm) 

# Now that the text is numerically represented, we compute the distance between them
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 

# Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

#Clustering 
hc <- hclust(dist.matrix, method = "ward.D2") 

#-------- Ploting--------- 
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

# Cladogram  
hc$labels<- names    #<----labels
colors = c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black" )
clus4 = cutree(hc, 13)
plot(as.phylo(hc), type = "cladogram", cex = 0.7, direction="upwards",show.tip.label=TRUE,show.node.label =TRUE,srt=-10,
     label.offset = 0, tip.color = colors[clus4], edge.color = "tan4", edge.width = 1, edge.lty = 1, main="Cladogram") #<--- COOL 
legend("topright", 
       legend = c("Physics" , "Mathematical optimization" , "Visual cortex" , "Memory", "Decision making" , "Oscillations", "Sensory system", "Connectomis", "Pathoconnectomics", "Network dynamics",  "Consciousness",  "Wakefulness" ), 
       col =c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black"),
       pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19 ), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1))  



# ------- Better visualization and saving it as png -------------
png("Cladogram.png",width=1600,height=800)
par(cex=1, font=3)
plot(as.phylo(hc), type = "cladogram", cex = 0.7, direction="upwards",show.tip.label=TRUE,show.node.label =TRUE,srt=-10,
     label.offset = 0, tip.color = colors[clus4], edge.color = "tan4", edge.width = 1, edge.lty = 1, main="Cladogram") #<--- COOL 
legend("topright", 
       legend = c("Physics" , "Mathematical optimization" , "Visual cortex" , "Memory", "Decision making" , "Oscillations", "Sensory system", "Connectomis", "Pathoconnectomics", "Network dynamics",  "Consciousness",  "Wakefulness" ), 
       col =c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black"),
       pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19 ), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1))  
dev.off()


# Unrooted
hc$labels<- names   #<----labels
colors = c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3" )
clus4 = cutree(hc, 13)
plot(as.phylo(hc), type = "unrooted", cex = 0.3, no.margin = FALSE,  
     tip.color = colors[clus4], edge.color = "tan4",node.depth=1, show.node.label= TRUE, main="Unrooted")
legend("topright", 
       legend = c("Physics" , "Mathematical optimization" , "Visual cortex" , "Memory", "Decision making" , "Oscillations", "Sensory system", "Connectomis", "Pathoconnectomics", "Network dynamics",  "Consciousness",  "Wakefulness" ), 
       col =c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black"),
       pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19 ), bty = "n",  pt.cex = 1.5, cex =1.0 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1)) 




# Fan
plot(as.phylo(hc), type = "fan", cex = 0.8)

# Cut the dendrogram into clusters  #### 13 clusters in excel 
# colors in R https://bookdown.org/hneth/ds4psy/D-2-apx-colors-essentials.html
colors = c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black" )
clus4 = cutree(hc, 13)
plot(as.phylo(hc), type = "fan",col = "#487AA1", tip.color = colors[clus4], label.offset = 0, cex = 0.5, main="Fan")
legend("topright", 
       legend = c("Physics" , "Mathematical optimization" , "Visual cortex" , "Memory", "Decision making" , "Oscillations", "Sensory system", "Connectomis", "Pathoconnectomics", "Network dynamics",  "Consciousness",  "Wakefulness" ), 
       col =c("deepskyblue1", "orchid1","darkorchid1","mediumblue", "springgreen4", "chartreuse3", "orange", "snow4", "tan4", "violetred2","cyan3", "red", "black"),
       pch = c(19,19,19,19,19,19,19,19,19,19,19,19,19 ), bty = "n",  pt.cex = 1.5, cex =1.0 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1)) 





# -------Convert hclust into a dendrogram -------
# #http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning#change-labels
dend <- as.dendrogram(hc)

#Changing the labels
labels(dend)
labels(dend)<-topics
labels(dend)

# Extract the data (for rectangular lines)
dend_data <- dendro_data(dend, type="triangle") # --->"rectangle" or "triangle"


##OPTIONS
#https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html
#leaves_cex - set the leaves’ point size 
#by_labels_branches_lwd - set the line width of branches with specific labels (using branches_attr_by_labels)
#by_labels_branches_lty - set the line type of branches with specific labels (using branches_attr_by_labels)

#---- TRADITIONAL STYLE-----------
# Changing the label names, color and size
dend %>% set("labels", c(names)) %>% #<-------changing labels names, topics
  set("branches_k_color", k=10) %>% 
  set("labels_colors", k=10) %>% # changing colors of labels
  set("labels_cex", 0.3) %>% # Changing size
  set("leaves_cex", 0.01) %>%   #set the leaves’ point size 
  set("branches_lwd", 2) %>% #width of branch 
  plot(main = "Gustavo Deco Lab tree", horiz=FALSE) # plotting

#Traditional style cropped to fit titles of papers
## https://stackoverflow.com/questions/13046323/how-to-change-the-label-size-of-an-r-plot
hc$labels<- names
par(cex=0.3)
ColorDendrogram(hc, y=colors[clus4], main = "Clusters from 216 samples",
branchlength = 0.30, labels = hc$labels, xlab = NULL,
sub = NULL, ylab = "", cex.main = NULL)


## ------TREE STYLE ------- ******
tree <- as.dendrogram(hc)
labels(tree)<-names   #<------- Labels
tree <- set(tree, "labels_cex", 0.5)
plot(tree, type="triangle", horiz = TRUE, main = "A larger font for labels")

# modify the dendrogram to have some colors in the branches and labels
tree <- tree %>% 
  color_branches(k=7) %>% 
  color_labels
  plot(tree, type="triangle", main = "Gustavo Deco Lab tree")
 # legend("topright", 
 #        legend = c("Physics" , "Mathematical optimization" , "Visual cortex" , "Memory" , "Oscillations"), 
 #        col = c("red", "blue" , "blue" , "red" , "Darkgreen"), 
 #        pch = c(20,20,4,4,4), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
 #        text.col = "black", horiz = FALSE, inset = c(0, 0.1))  

  
# ------- Better visualization and saving it as png -------------
  png("plotdendogramCOOL.png",width=1600,height=800)
  par(cex=1, font=3)
  #plot(hc, hang=-1, main="Gustavo Deco Lab Tree", label=topics)
  plot(tree, type="triangle", main = "Gustavo Deco Lab tree")
  dev.off()
  
  
  
## ------ USING GGPLOT -----------
  ggtree <- as.ggdend(tree)
  ggplot(ggtree, horiz = TRUE, type="triangle")
  
  
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_text(size=10),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())
  
##  ggplot test2 
  ddata <- dendro_data(dend, type = "triangle")
  ggplot(segment(ddata)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip() + 
    scale_y_reverse(expand = c(0.2, 0)) +
    theme_dendro()
  
  
