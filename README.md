# K Means Clustering

A k-means cluster analysis of the similarity of pitches in MLB



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r echo = F}

suppressMessages(suppressWarnings(library(MASS)))
suppressMessages(suppressWarnings(library(stats)))
suppressMessages(suppressWarnings(library(cluster)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(knitr)))


```

## Introduction

One of the hallmarks of professional baseball are the pitchers' ability to throw many different pitches effectively. In fact, some pitchers throw different assortments of what are generally considered to be the same pitch, such as the different assortments of fastball (cut, two-seam, four-seam, and others). Different pitchers also tend to throw different types of pitches with different grips. All of this can seem a bit complicated and cumbersome for those who are not well-versed in baseball's pitching subculture. But one might wonder, are all of these pitches $\textit{actually}$ different?

As a huge baseball fan, I have wondered if it might be more prudent to classify pitches by spin, speed, and movement rather than simply by how the pitcher holds the ball when he throws it. For many avid baseball watchers, some pitchers throw a cut-fastball, some throw a slider. Both pitches, however, are generally in the range of 88-92 mph and move down and away from a right handed batter at roughly the same angle. In reality, it seems as if the only difference in these pitches is the name being given to them. To the hitter, viewer, and even most pitchers, the pitches are virtually identical. Simplifying classification to call both of these pitches a slider (or cut-fastbal or any other name) would seem to make life easier for casual fans and would likely do little to reduce the information they receive about the pitch being thrown. 

## Data Collection

All data has been collected using Major League Baseball's Statcast database, which records a number of interesting variables on every single pitch. These variables include "Spin Rate", "Pitch Velocity", "pf_x", "pf_z", "pitch type", and many others. The data pulled from the statcast database for this project is every pitch from the 2018 season through 5/23 this consists of n = 217,521 total pitches. By necessity, since we are using pitch movement as a clustering feature, I needed to limit the pitches to right-handed pitchers, though we could have chosen left-handed pitchers instead.. I do not think that we lose much, if any, in terms of limiting the dataset this way. I then simplified the dataset by removing pitches titled "null, "UN", "EP", and "PO" since "null" and "UN" are both indicators that the pitch classification algorithm felt this pitch was unlike any pitch it recognizes, "EP" is an "eephus" pitch which is thrown about 10 times a year, and "PO, meaning pickoff, which isn't a real pitch. I then reduced the dataset further by making sure that every pitch included had a recorded "pitch velocity", "pf_x", "pf_z", and "spin rate" value. Ultimately, this resulted in a dataset of n = 152,509 pitches.

One worry I had was that some pitchers throw around 100 mph while others throw in the range of 90 mph. The changeup for a 100 mph pitcher is usually in the range of 88-90 mph. If we simply use velocity as a feature, we may run the risk of having a changeup and a fastball classified as the same pitch. While you could argue that a 100 mph fastball is a different pitch than a 90 mph fastball, I found it more intuitive to simply transform the velocity column into "velocity minus maximim velocity" where I found the maximum velocity for each pitcher in 2018 and subtracted each pitch from that value. This gives us a better measure of the relative velocity differences in pitches which I think is more valuable than pure velocity. 

The variables "pf_x" and "pf_z" are considered "horizontal movement" and "vertical movement" respectively. Both values are measured relative to a theoretical pitch with no spin-related movement. For example, "vertical movement"" would be equal to 0 on a pitch that simply dropped the amount we would expect it to drop due to gravity alone. 

In order for the clustering algorithm to work effectively, we also need to scale the data since Euclidean distances are being calculated. Obviously, since we are running a clustering algorithm, we scale this dataset without the pitch classifications but we will hold onto the original classifications for later in order to analyze the sucess of our clustering algorithm. All of the work on the data set is included in the code in the appendix.

## Methods

For this project, I have used what is likely the simplest clustering algorithm, the k-means clustering technique. In the k-means clustering algorithm, we first center and scale the data. We then pick $k$ starting values as our initialized cluster centers. Each iteration through the algorithm, we calculate the Euclidean distance from all $k$ centers for each point in our dataset. We assign each point to the cluster where the calculated Euclidean distance is smallest. We then redefine the $k$ centers as the mean of each of the $k$ clusters defined in the previous step. These last two steps are repeated until convergence.

While there are many possible clustering algorithms that we may use for smaller datasets, many of these algorithms require the construction of a "distance matrix" that increases exponentially in size as we increase $n$. For a dataset with n = 152,509, the required amount needed to hold the distance matrix was 86.6 GB. I considered choosing a subset of my data to analyze, possibly a week or two, but the dataset would still be massive and without at least a month's worth of data, we would be selectively sampling pitches that come from starters much more than that of relievers since relievers may pitch quite a bit less in any given two-week period. Thus, I felt it was best to simply run a k-means clustering algorithm with k ranging from 2 all the way to 12. The original number of pitch classifications was 12, as will be seen in the tables in the next section. Since our goal is to possibly reduce the number of pitch clusters, I decided not to go larger than k = 12.

For each individual clustering algorithm, several things are calculated. First, I have computed silhouette diagrams for all 11 different clustering algorithms. Since for each clustering algorithm, a distance matrix is required, I have sampled 2000 data points randomly from the original dataset, with the proportion of each cluster in the 2000 randomly selected points equal to the proportion of each cluster in the overall dataset. This method is prone to error, so I have done this 20 times for each cluster and averaged the average silhoutte length to hopefully get a more stable estimate of average silhoutte length. The plots, however, are simply the plot of the 20th silhouette for each cluster and are available as a visual guide of what happens in run through each clustering algorithm. The second thing I have computed is the ratio of within-cluster sum of squares to total sum of squares for each clustering algorithm.

The reason I have calculated both of these is that I want to choose the appropriate number of $k$ according to both of these criterion functions later on. 


```{r echo = FALSE}

x2018data = read.csv("C:/Users/ryanh/OneDrive/Documents/x2018data.csv", header = TRUE)

#need single-handed data only, chose rhp out of convenience

x2018data.rhp = x2018data[which(x2018data$p_throws == "R"),]

pitchdata.rhp = data.frame(player_name = as.character(x2018data.rhp$player_name), 
                           pitch_type = as.character(x2018data.rhp$pitch_type), 
                           pitch_velocity = x2018data.rhp$release_speed, 
                           spin_rate = x2018data.rhp$release_spin_rate,
                           horiz_movement = x2018data.rhp$pfx_x, 
                           vert_movement = x2018data.rhp$pfx_z)

pitchdata.rhp = data.frame(pitchdata.rhp[-which(pitchdata.rhp[,2] == "null"),])
pitchdata.rhp = data.frame(pitchdata.rhp[-which(pitchdata.rhp[,2] == "UN"), ])
pitchdata.rhp = data.frame(pitchdata.rhp[-which(pitchdata.rhp[,2] == "EP"), ])
pitchdata.rhp = data.frame(pitchdata.rhp[-which(pitchdata.rhp[,2] == "PO"), ])
pitchdata.rhp = data.frame(pitchdata.rhp[complete.cases(pitchdata.rhp),])
pitchdata.rhp = data.frame(pitchdata.rhp[,], velo_minus_max = vector(length = nrow(pitchdata.rhp)))
rownames(pitchdata.rhp) = 1:nrow(pitchdata.rhp)
                       
#convert velocity into velocity - maximum velocity to account for pitcher ability and correlated velocities

pitchdata.sorted.rhp = pitchdata.rhp %>% group_by(player_name)
maximum.velo.rhp = pitchdata.sorted.rhp %>% summarise(pitch_velocity = max(pitch_velocity))


for(i in 1:length(maximum.velo.rhp$player_name)){

      pitchdata.rhp$velo_minus_max[which(pitchdata.rhp$player_name == maximum.velo.rhp$player_name[i])] =
      pitchdata.rhp$pitch_velocity[which(pitchdata.rhp$player_name == maximum.velo.rhp$player_name[i])] 
     -maximum.velo.rhp$pitch_velocity[i]
    
}

#scaling data to use in clustering

cluster.data = scale(pitchdata.rhp[,-(1:3)])
rownames(cluster.data) = 1:nrow(cluster.data)

#kmeans clustering, also computing the criterion (within.ss/tot.ss)
#also plotting silhouttes of random subsets of the data proportional to clustered outcomes
#and computing the mean average silhoutte length over 10 random samples

clustering = function(data, k, silhouette.size = 1000){
  
  clusters = vector("list", length = k-1)
  
  ss.ratio = vector(length = k-1)
  names(ss.ratio) = as.character(seq(from = 2, to = k, by = 1))
  
  silhouette.stuff = vector("list", length = k)
  
  est.ave.sil = vector(length = k-1)
  names(est.ave.sil) = as.character(seq(from = 2, to = k, by = 1))
  
  for (i in 2:k){
    
    clusterk = kmeans(data, centers = i, nstart = 10, iter.max = 200, algorithm = "MacQueen")
    
    clusters[[i-1]] = clusterk
    
    ss.ratio[i-1] = clusterk$tot.withinss/clusterk$totss
    
    ind = c()
    ave.sil = c()
    
    for(p in 1:20){
      
      s = c()
    
      for(j in 1:k){
      
        props = length(which(clusterk$cluster == j))/length(clusterk$cluster)
      
        s = append(s, sample(clusterk$cluster[which(clusterk$cluster == j)], 
                             size = round(props*silhouette.size)))
      
      }
    
    ind = as.numeric(names(s))
    
    subdata = data.frame(cluster.data[ind, ], new_class = s)
    
    silhouette.stuff[[i-1]] = silhouette(s, dist(subdata[,-ncol(subdata)]))
    
    ave.sil[p] = summary(silhouette.stuff[[i-1]])[[4]]
    
    }
    
    est.ave.sil[i-1] = mean(ave.sil)
    
  }
  
  title.vec = as.character(seq(from = 2, to = k, by = 1))
  
  for(i in 1:(k-1)){
    
    
    plot(silhouette.stuff[[i]], border = NA, 
         main = substitute(paste('Silhoutte plot for ', a, ' clusters'), 
                           list(a = title.vec[i])))
    
  }
    
  return(list(clusters, c("1" = 1, ss.ratio), est.ave.sil))
  
}

```


## Results

On the next few pages, you will see one of the silhoutte plots calculated for each of the clustering algorithms from k = 2 to k = 11. Directly after these silhouette plots, you will see a table that shows the mean average silhouette length for each k. One popular way of selecting $k$ for clustering algorithms is choosing $k$ to be the number of centers that produces the smallest average silhouette length. Once again, as was stated previously, these average silhouette lengths are prone to some error since we are only sampling a random subset of each cluster for each algorithm. Thus, I have calculated the mean average silhouette length over m = 20 different random subsets of the data in the hopes of reducing the variability of this estimate.

```{r, echo = FALSE}

all.clusters = suppressWarnings(clustering(cluster.data, 11, silhouette.size = 2000))

```



```{r echo = FALSE}

#enumerating mean average silhoutte length for each k
kable(t(all.clusters[[3]]), digits = 3, caption = "Mean Average Silhoutte Length for k = 1, ..., 11")

```

### Analysis of K-Means Clustering Algorithm with k = 11

As we can see in the previous table, the mean average silhouette length favors k = 11 over the pack of k = 7, 8, 9, & 10 which are all in roughly the same range. We will proceed with an analysis of the clustering method for k = 11 first, though our ultimate goal is to appropriately reduce the number of clusters. 

Below we have three tables that describe the clustering method in different ways:


#### Table 1A: Proportion Within Cluster
- This first table shows the proportion of  pitches classified under their original classification within each of the 11 new clusters. For example, "Pitch 1" is made up of 72% curveballs, 19% knuckle-curves, and 9% sliders. In other words, the row sums in this table are equal to 1. 


```{r echo = FALSE}

#choose k = 11 due to smallest average silhoutte length


cluster.no = 11
best.cluster = all.clusters[[1]][[(cluster.no - 1)]]

#mapping new clusters back to old names to determine what types of pitch names are going into each cluster

proportions = matrix(nrow = cluster.no, ncol = 11)
pitchprop = matrix(nrow = cluster.no, ncol = 11)

colnames(proportions) = c("Changeup", "Curveball", "Cut FB", "Four-Seam FB", 
                         "Forkball", "Splitfinger FB", "Two-Seam FB", 
                         "Knuckle Curve", "Knuckleball", "Sinker", "Slider")
rownames(proportions) = c("Pitch 1", "Pitch 2", "Pitch 3", "Pitch 4", "Pitch 5", 
                         "Pitch 6", "Pitch 7", "Pitch 8", "Pitch 9", "Pitch 10", 
                         "Pitch 11")
colnames(pitchprop) = c("Changeup", "Curveball", "Cut FB", "Four-Seam FB", 
                       "Forkball", "Splitfinger FB", "Two-Seam FB", 
                       "Knuckle Curve", "Knuckleball", "Sinker", "Slider")
rownames(pitchprop) = c("Pitch 1", "Pitch 2", "Pitch 3", "Pitch 4", "Pitch 5", 
                       "Pitch 6", "Pitch 7", "Pitch 8", "Pitch 9", "Pitch 10", 
                          "Pitch 11")

#two different proportions being reported, one with respect to total within cluster, 
#one with respect to total within previous classification

for(i in 1:cluster.no){
  
 prop.change = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                   == "CH")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.curve = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                  == "CU")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.cut = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                == "FC")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.four = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                 == "FF")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]) 
 prop.fork = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                 == "FO")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.split = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                  == "FS")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.two = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                == "FT")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.kcurve = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                   == "KC")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.knuck = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                  == "KN")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.sink = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                 == "SI")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 prop.slider = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                   == "SL")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
 
 prop.change.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                       == "CH")/sum(pitchdata.rhp$pitch_type == "CH")
 prop.curve.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "CU")/sum(pitchdata.rhp$pitch_type == "CU")
 prop.cut.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                    == "FC")/sum(pitchdata.rhp$pitch_type == "FC")
 prop.four.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                     == "FF")/sum(pitchdata.rhp$pitch_type == "FF") 
 prop.fork.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                     == "FO")/sum(pitchdata.rhp$pitch_type == "FO")
 prop.split.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "FS")/sum(pitchdata.rhp$pitch_type == "FS")
 prop.two.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                    == "FT")/sum(pitchdata.rhp$pitch_type == "FT")
 prop.kcurve.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                       == "KC")/sum(pitchdata.rhp$pitch_type == "KC")
 prop.knuck.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "KN")/sum(pitchdata.rhp$pitch_type == "KN")
 prop.sink.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                     == "SI")/sum(pitchdata.rhp$pitch_type == "SI")
 prop.slider.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                       == "SL")/sum(pitchdata.rhp$pitch_type == "SL")
 
 
 
 #proportion of each classified pitch with respect to its cluster classification
 #i.e. proportion of the pitches in class i that were classified changeup originally 
 proportions[i,] = c(prop.change, prop.curve, prop.cut, prop.four, prop.fork, 
                     prop.split, prop.two, prop.kcurve, prop.knuck, prop.sink, 
                     prop.slider)
 
 
 #proportion of each classified pitch with respect to its original classification
 #i.e. proportion of pitches that were originally classified as changeup that were classified as pitch i 
 pitchprop[i,] = c(prop.change.tot, prop.curve.tot, prop.cut.tot, prop.four.tot, 
                   prop.fork.tot, prop.split.tot, prop.two.tot, prop.kcurve.tot, 
                   prop.knuck.tot, prop.sink.tot, prop.slider.tot)
 
 
}



#mapping clusters back into the feature space to determine 90% CI for each feature within each cluster
#gives a general range of values that could be used to predict future pitch classification.

features = matrix(nrow = 4, ncol = 2*cluster.no)

feature.name = vector(length = 2*cluster.no)
pitch.no = seq(1, cluster.no, 1)

for(i in 1:cluster.no){
  feature.name[2*i-1] = paste("Pitch", pitch.no[[i]], "5%")
  feature.name[2*i] = paste("Pitch", pitch.no[[i]], "95%")
}
  
colnames(features) = feature.name
rownames(features) = c("Spin Rate", "Horiz. Movement", "Vert. Movement", "Velo - Max")


for(i in 1:cluster.no){
  
  for(j in 1:4){
    
    q = quantile(pitchdata.rhp[which(best.cluster$cluster == i),j+3], c(0.05, 0.95))
    
    features[j,2*i-1] = q[1]
    features[j, 2*i] = q[2]
    
  }
  
}

suppressWarnings(library(pander))
panderOptions('digits', 3)
panderOptions('round', 3)
panderOptions('keep.trailing.zeros', TRUE)

pander(proportions)

```


#### Table 2A: Proportion Within Original Classification
- This second table shows how the original pitch classifications are distributed through the 11 new pitch clusters. For example, 0.3% of changeups are classified as "Pitch 2", 0.3% are classified as "Pitch 3", 61.6% as "Pitch 4", 3.7% as "Pitch 5" and so on. In other words, the column sums in this table are equal to 1.

```{r echo = F}

pander(pitchprop)

```

#### Table 3A: Middle 90% of Feature Space
- This third table shows the middle 90% of the feature space in each new pitch cluster. For example, in Pitch 1, the middle 90% of spin rate is between 2487.0 and 3114.1 reveolutions per minute (rpms), the middle 90% of horizontal movement ranges between 0.447 and 1.598 inches, the middle 90% of vertical movement ranges between -1.600 and -0.411 inches and the middle 90% of velocity relative to the maximum is -21.90 to -12.70 miles per hour (mph).

```{r echo = F}
pander(features)

```

The most interesting part of this reclustering is that we have simply reclustered pitches into the same number of clusters as we started with. Originally, we started with 11 pitch classifications and the goal was to reduce the number of clusters to simplify pitch classification. What we have done here is recluster pitches, and show that the current pitch classification rule is not all that good at identifying how a particular pitch looks or moves to a hitter. An example of this is the relationship between curveball and slider. If we look at our first table, Pitch 1 is made up mostly of curveballs and knuckle-curveballs with some sliders mixed in. The interesting thing is that "Pitch 3" is also made up of the same three pitches, just pitches that clearly move differently or travel at different speeds. The original pitch classificiation method is failing us here. 

### Analysis of K-Means Clustering Algorithm with k = 5

The primary goal of this project was to see if it was possible to reduce the number of pitch clusters. In our previous analysis, we noticed that there are two new pitch clusters that consist mostly of curveballs, knuckle-curves, and sliders. Intuitively, it would make sense to be able to cluster those two pitches together. But how should we determine the number of k to use if not via the average silhouette length as before?

One method of selecting $k$ is what is called "The Elbow Method." This method only requires that we calculate the ratio of within-cluster sum of squares to the total sum of squares for each $k$. As we would expect with any sum of squares statistic, this value will always decrease as $k$ increases. Thus, we need to select $k$ where we start to see little-to-no decrease in our within-cluster sum of squares ratio. The following plot and table shows the relationship between $k$ and within-cluster sum of squares divided by total sum of squares.

```{r, echo = F}

##########################################################################################################################

#now lets use the "elbow method" to select k

#plotting within.ss/tot.ss over k

plot(all.clusters[[2]], xlim = c(0, 13), xlab = "k", ylab = "Within SS/Total SS", main = "Within SS/Tot SS by k")

#enumerating within.ss/tot.ss for each k
kable(t(all.clusters[[2]]), digits = 3, caption = "Within SS/Total SS by K")


```

Looking at the plot, the argument could be made for $k$ = 3, 4, or 5 in my opinion. The drop in the ratio of within-cluster sum of squares from $k$ = 2 to $k$ = 3 is substantial (about 0.15), and the drop from $k$ = 3 to $k$ = 4 is quite a bit smaller (0.06), but I think the most logical $k$ to choose is $k$ = 5 since the within-cluster sum of squares ratio drops consistantly as $k$ increases up until we from $k$ = 5 to $k$ = 6 where the decreases is less than 0.02. Choosing a cluster size is also about the number of clusters I had hoped to be able to achieve when I started this project. Thus, I will proceed with a similar analysis as with $k$ = 11, but now with $k$ = 5.


```{r echo = F}

##now choose k = 5 due to the "elbow method" where we select based, somewhat subjectively, on where the "elbow" in the plot of
#within.ss/tot.ss over k is. This is equivalent to choosing k based on where the reduction in variability starts to level off

#choose k = 5 via the elbow method

cluster.no = 5

best.cluster = all.clusters[[1]][[cluster.no-1]]



#mapping new clusters back to old names to determine what types of pitch names are going into each cluster

proportions = matrix(nrow = cluster.no, ncol = 11)
pitchprop = matrix(nrow = cluster.no, ncol = 11)

colnames(proportions) = c("Changeup", "Curveball", "Cut FB", "Four-Seam FB", 
                          "Forkball", "Splitfinger FB", "Two-Seam FB", "Knuckle Curve",
                          "Knuckleball", "Sinker", "Slider")
rownames(proportions) = c("Pitch 1", "Pitch 2", "Pitch 3", "Pitch 4", "Pitch 5")
colnames(pitchprop) = c("Changeup", "Curveball", "Cut FB", "Four-Seam FB", 
                        "Forkball", "Splitfinger FB", "Two-Seam FB", "Knuckle Curve",
                        "Knuckleball", "Sinker", "Slider")
rownames(pitchprop) = c("Pitch 1", "Pitch 2", "Pitch 3", "Pitch 4", "Pitch 5")

#two different proportions being reported, one with respect to total within cluster, one with respect to total within previous classification

for(i in 1:cluster.no){
  
  prop.change = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                    == "CH")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.curve = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                   == "CU")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.cut = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                 == "FC")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.four = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                  == "FF")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]) 
  prop.fork = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                  == "FO")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.split = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                   == "FS")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.two = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                 == "FT")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.kcurve = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                    == "KC")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.knuck = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                   == "KN")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.sink = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                  == "SI")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  prop.slider = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                    == "SL")/length(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)])
  
  prop.change.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                        == "CH")/sum(pitchdata.rhp$pitch_type == "CH")
  prop.curve.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                       == "CU")/sum(pitchdata.rhp$pitch_type == "CU")
  prop.cut.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                     == "FC")/sum(pitchdata.rhp$pitch_type == "FC")
  prop.four.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "FF")/sum(pitchdata.rhp$pitch_type == "FF") 
  prop.fork.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "FO")/sum(pitchdata.rhp$pitch_type == "FO")
  prop.split.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                       == "FS")/sum(pitchdata.rhp$pitch_type == "FS")
  prop.two.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                     == "FT")/sum(pitchdata.rhp$pitch_type == "FT")
  prop.kcurve.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                        == "KC")/sum(pitchdata.rhp$pitch_type == "KC")
  prop.knuck.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)]
                       == "KN")/sum(pitchdata.rhp$pitch_type == "KN")
  prop.sink.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                      == "SI")/sum(pitchdata.rhp$pitch_type == "SI")
  prop.slider.tot = sum(pitchdata.rhp$pitch_type[which(best.cluster$cluster == i)] 
                        == "SL")/sum(pitchdata.rhp$pitch_type == "SL")
  
  
  
  #proportion of each classified pitch with respect to its cluster classification
  #i.e. proportion of the pitches in class i that were classified changeup originally 
  proportions[i,] = c(prop.change, prop.curve, prop.cut, prop.four, prop.fork, 
                      prop.split, prop.two, prop.kcurve, prop.knuck, prop.sink, 
                      prop.slider)
  
  
  #proportion of each classified pitch with respect to its original classification
  #i.e. proportion of pitches that were originally classified as changeup that were classified as pitch i 
  pitchprop[i,] = c(prop.change.tot, prop.curve.tot, prop.cut.tot, prop.four.tot,
                    prop.fork.tot, prop.split.tot, prop.two.tot, prop.kcurve.tot,
                    prop.knuck.tot, prop.sink.tot, prop.slider.tot)
  
  
}

#mapping clusters back into the feature space to determine 90% CI for each feature within each cluster
#gives a general range of values that could be used to predict future pitch classification.

features = matrix(nrow = 4, ncol = 2*cluster.no)

feature.name = vector(length = 2*cluster.no)
pitch.no = seq(1, cluster.no, 1)

for(i in 1:cluster.no){
  feature.name[2*i-1] = paste("Pitch", pitch.no[[i]], "5%")
  feature.name[2*i] = paste("Pitch", pitch.no[[i]], "95%")
}

colnames(features) = feature.name
rownames(features) = c("Spin Rate", "Horiz. Movement", "Vert. Movement", "Velo - Max Velo")


for(i in 1:cluster.no){
  
  for(j in 1:4){
    
    q = quantile(pitchdata.rhp[which(best.cluster$cluster == i),j+3], c(0.05, 0.95))
    
    features[j,2*i-1] = q[1]
    features[j, 2*i] = q[2]
    
  }
  
}


```

#### Table 1B: Proportion Within Cluster

The table below shows us several things:

1) "Pitch 1" is made up of 77% Changeups with a smaller number (15%) of split-finger fastballs. We can probably label this pitch as simply a "changeup", though we will investigate how these pitches truly look when we look at the feature space laer.

2) "Pitch 2" is made up of mostly cut-fastballs and sliders, two pitches that baseball aficionados know move very similarly. This pitch can probably be labeled more simply as a "slider."

3) "Pitch 3" is made up mostly of curveballs, sliders and knuckle-curves. This pitch would likely be called a curveball or slurve by most coaches.

4) "Pitch 4" is comprised almost exclusively of four-seam fastballs (89%). I imagine this pitch would be considered the "hard, straight fastball" by most players and coaches, which has little to no sinking action and may even appear to rise to hitters.

5) "Pitch 5" is made up of a combination of four-seam fastballs, two-seam fastballs, and sinkers. These pitches are likely the fastballs that have some sinking action as they move towards the hitter and are generally just considered "sinkers" by players and coaches.

```{r echo = F}

pander(proportions)

```

We have to be wary with the proportions in this table, however. It might be more useful to look at how each of the original pitch classifications are distributed across the new pitch clusters

#### Table 2B: Proportion Within Original Classification

If we look at the table below, we can see that most changeups, forkballs, splitfinger fastballs, and knuckleballs are classified as "pitch 1", most cut fastballs and sliders are classified as "pitch 2", most curveballs and knuckle-curveballs are classified as "pitch 3", most four-seam fastballs are classified as "pitch 4", and most two-seam fastballs and sinkers are classified as pitch 5.

```{r echo = F}
pander(pitchprop)

```

Based on these results, the naming mechanisms I have proposed in the previous section still seem to be reasonable. The last thing we should do, however, is investigate the feature space of each of our new clusters to see how these pitches act in general.

###Table 3B: Feature Space

A few notable things about the feature spaces of these pitches seen in the table below.:

- Pitch 1 has a noticeable difference in spin rate from all other pitches. A pitch under 1500 rpms will almost always be classified as pitch 1.
- Pitch 4 and 5 have very similar profiles except for differences in vertical movement, providing more evidence for the sinking fastball vs. rising fastball naming convention I have proposed a couple of sections earlier.
- Pitch 2 and 3 are quite similar, both of which being pitches with high spin rates and lots of downward vertical movement. Pitch 3 has more significant vertical movement, while Pitch 2 has more vertical movement. Another stark difference is the speed at which each of these breaking pitches are thrown. Pitch 2 is thrown about 5-7 mph faster relative to maximum velocity than Pitch 3 is. 

```{r echo = F}

pander(features)

```

Ultimately, after analyzing the feature space of these pitch clusters, the naming conventions I have proposed continue to seem reasonable. Thus, in the case where we choose $k$ = 5, we end up with only 5 total types of pitches in baseball: A changeup, slider, curveball, rising fastball, and sinking fastball. This naming convention simplifies the nature of pitching instead of having multiple names for pitches that look and act the same to most observers. 

## Discussion

Pitching in baseball can seem quite complicated and convoluted to the casual fan. Understanding things like pitch sequencing is even further muddled by the current naming conventions of pitches in the Major League Baseball. The goal of this project was to reduce the complexity of the naming structure for casual fans and see how much, if any, information we lose.

Using k-means reclustering, out of necessity, I used two different methods to determine $k$. One method, average silhouette lengths, led me to choose $k$ = 11, exactly the number of clusters I started with. Even though the data was clustered differently than the original data, a cluster size of $k$ = 11 was not the original goal of the project. That being said, the fact that the data was reclustered differently than the original data despite having the same number of clusters leads me to believe that the naming conventions in baseball currently are not exceptionally good at describing the way a pitch looks to a hitter or well-informed observer anyway. Thus, reclustering with a smaller $k$ seems reasonable and not particularly harmful if we choose $k$ in an intelligent way.

Using the "elbow method" to select $k$, I chose $k$ = 5 since the within-cluster sum of squares levels off and decreases much more slowly after $k$ = 5. This clustering algorithm was particularly insightful, since we managed to reduce the types of pitches down to changeup, slider, curveball, rising fastball, and sinking fastball. 

Ultimately, this project was insightful in that I confirmed a lot of suspicions about which pitches are similar to each other and how the naming conventions in modern baseball seem precise but can be convoluted and imprecise as well. 
