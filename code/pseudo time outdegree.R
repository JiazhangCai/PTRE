library(dplyr)
library(igraph)
library("ggpubr")
library(TSCAN)
library(gridExtra)


label = read.csv("/Users/james/Documents/lab/2021/covid-19 graph/COVID19_edge.csv")
label = data.frame(from = label$confirmed_parent_id, to = label$id2)
label[label == ""] = NA
label = (label %>% na.omit ) # 288 labeled edges
intersect(paste(label$from, label$to), paste(label$to, label$from))

# label = read.csv("/Users/jiazhangcai/Documents/study/lab/covid-19 graph/edge_diff_standar.csv")
# label = data.frame(from = label$confirmed_parent_id_5days, to = label$id2)
# label[label == ""] = NA
# label = (label %>% na.omit ) # 288 labeled edges
# intersect(paste(label$from, label$to), paste(label$to, label$from))



data1 = read.csv("/Users/james/Documents/lab/2021/covid-19 graph/case.csv")
data1[data1 == ""] = NA
data1$severity2 = ifelse(data1$severity2 == "asymptomatic", 1, 0)
data1$onset_date =  as.Date(data1$onset_date,"%Y-%m-%d",tz = "EST5EDT")
data1$confirm_date =  as.Date(data1$confirm_date,"%Y-%m-%d",tz = "EST5EDT")


edges1 = data1[,c("id2","contact1_id")]
colnames(edges1)= c("id","contact_id")
edges2 = data1[,c("id2","contact2_id")]
colnames(edges2)= c("id","contact_id")
edges3 = data1[,c("id2","contact3_id")]
colnames(edges3)= c("id","contact_id")
edges4 = data1[,c("id2","contact4_id")]
colnames(edges4)= c("id","contact_id")
edges = rbind(edges1,edges2,edges3,edges4)
edges = na.omit(edges)
# save(edges, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/edges_adjust.rda")


g = graph_from_data_frame(edges, directed = FALSE)
nodes = data1[data1$id2 %in% names(V(g)),]

edges_onset = edges
edges_onset$diff_date = 0
for(i in 1:nrow(edges_onset))
{
  diff = as.numeric(difftime(nodes$onset_date[which(nodes$id2 == edges_onset$id[i])],
                             nodes$onset_date[which(nodes$id2 == edges_onset$contact_id[i])], units = "days"))
  if(diff > 0)
  {
    temp = edges_onset$id[i]
    edges_onset$id[i] = edges_onset$contact_id[i]
    edges_onset$contact_id[i] = temp
  }
  edges_onset$diff_date[i] = abs(diff)
}

temp_degree = data.frame(degree = degree(g))
temp_degree$id2 = names(degree(g))

g_onset = graph_from_data_frame(edges_onset, directed = TRUE)
temp_out_degree = data.frame(out_degree = degree(g_onset, mode = "out"))
temp_out_degree$id2 = names(degree(g_onset))
temp_degree = merge(temp_degree, temp_out_degree, by = "id2")

mg = merge(temp_degree, nodes,by="id2")

mg$occupation = as.factor(mg$occupation)
levels(mg$occupation)[!(levels(mg$occupation) %in%
                          c("农民","商业服务","家务及待业","工人","离退人员"))]="others"
levels(mg$occupation) = c("others","farmer","business",
                          "unemployed","worker","retired")
mg$onset_date2 = as.numeric(difftime(mg$onset_date,
                                     as.Date("2020-01-01","%Y-%m-%d",tz = "EST5EDT")))

mg = mg[which(mg$degree > 0), ]
dat_new = mg[,c("degree", "out_degree", "age_year", "severity2", "onset_date2", "occupation")]

dat_new$occupation = factor(dat_new$occupation)
levels(dat_new$occupation) = c(6,2,1,3,4,5)
dat_new$occupation = as.numeric(dat_new$occupation)

rownames(dat_new) = mg$id2

dat_new$position = 0
g = graph_from_data_frame(edges, directed = FALSE)
f <- function(graph, data, extra) data['succ'] == -1
for(i in 1:nrow(dat_new))
{
  result = bfs(g, root = rownames(dat_new)[i], dist = TRUE, callback = f)
  dist = result$dist
  vertex_before = c(rownames(dat_new)[which(dat_new$onset_date2 <= dat_new$onset_date2[i])], rownames(dat_new)[i])
  depth_before = max(na.omit(dist[names(dist) %in% vertex_before]))
  vertex_after = c(rownames(dat_new)[which(dat_new$onset_date2 > dat_new$onset_date2[i])], rownames(dat_new)[i])
  depth_after = max(na.omit(dist[names(dist) %in% vertex_after]))
  dat_new$position[i] = depth_before/(depth_after+depth_before)
}

dat_new1 = dat_new[,c("degree", "out_degree", "onset_date2", "severity2", "position", "occupation", "age_year")]

dat_new1 = na.omit(dat_new1)

data_nodes = dat_new1
data_nodes_label = data_nodes[which((rownames(data_nodes) %in% label$from) | 
                                      (rownames(data_nodes) %in% label$to)), ]

save(data_nodes, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/data_nodes.rda")
save(data_nodes_label, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/data_nodes_label.rda")


dat_new2 = dat_new1
dat_new3 = as.data.frame(t(dat_new2))


#### build the model
lpsmclust = exprmclust(dat_new3, reduce=T)
plotmclust(x = 1, y = 2, lpsmclust, show_cell_names =F)

lpsmclust$clusterid %>% table 
id_cls = data.frame(cls = lpsmclust$clusterid)
id_cls$id = rownames(id_cls)
# save(id_cls, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/id_cls.rda")

#### calculate the mean onset in each cluster
clu_id = lpsmclust$clusterid %>% unique()
onset_mean = rep(0, length(clu_id))
for(i in 1:length(onset_mean))
{
  clu_nodes = names(lpsmclust$clusterid[which(lpsmclust$clusterid == clu_id[i])])
  onset_mean[i] = mean(dat_new$onset_date2[which(rownames(dat_new) %in% clu_nodes)])
}
temp = data.frame(id = clu_id, mean = onset_mean) %>% arrange(mean)
MSTorder = temp$id

tsorder = TSCANorder(lpsmclust, MSTorder = MSTorder)
tsorder %>% length
TSCANorder(lpsmclust,orderonly = F) %>% tail


edges$cls1 = NA
edges$cls2 = NA
edges$tsorder1 = NA
edges$tsorder2 = NA

for(i in 1:nrow(edges)){
  # cat(i,"\n")
  cls1 = id_cls$cls[ id_cls$id %in% edges[i,1]]
  cls2 = id_cls$cls[ id_cls$id %in% edges[i,2]]
  if(length(cls1)>0)
  {
    which(tsorder %in% edges[i,2] )
    edges$cls1[i] = cls1
  }
  if(length(which(tsorder %in% edges[i,1] ))>0)
    edges$tsorder1[i] = which(tsorder %in% edges[i,1] )
  if(length(cls2)>0)
    edges$cls2[i]  = cls2
  
  if(length(which(tsorder %in% edges[i,2] ))>0)
    edges$tsorder2[i] = which(tsorder %in% edges[i,2] )
  
  if(length(which(rownames(dat_new) %in% edges[i, 1])>0))
    edges$position1[i] = dat_new$position[rownames(dat_new) %in% edges[i, 1]]
  if(length(which(rownames(dat_new) %in% edges[i, 2])>0))
    edges$position2[i] = dat_new$position[rownames(dat_new) %in% edges[i, 2]]
  
  edges$degree1[i] = (mg$degree [mg$id2 %in% edges[i,1]])
  edges$degree2[i] = (mg$degree [mg$id2 %in% edges[i,2]])
  edges$out_degree1[i] = (mg$out_degree [mg$id2 %in% edges[i,1]])
  edges$out_degree2[i] = (mg$out_degree [mg$id2 %in% edges[i,2]])
  
  edges$onset1[i] = as.character(mg$onset_date[mg$id2 %in% edges[i,1]])
  edges$onset2[i] = as.character(mg$onset_date[mg$id2 %in% edges[i,2]])
  edges$age_year1[i] = (mg$age_year[mg$id2 %in% edges[i,1]])
  edges$age_year2[i] = (mg$age_year[mg$id2 %in% edges[i,2]])
  edges$occupation1[i] = as.character(mg$occupation[mg$id2 %in% edges[i,1]])
  edges$occupation2[i] = as.character(mg$occupation[mg$id2 %in% edges[i,2]])
  edges$severity1[i] = (mg$severity2[mg$id2 %in% edges[i,1]])
  edges$severity2[i] = (mg$severity2[mg$id2 %in% edges[i,2]])
}



wh1 = c(which(is.na(edges$cls1)), which(is.na(edges$cls2)))
wh2 = c(which(is.na(edges$tsorder1)), which(is.na(edges$tsorder2)))
length(wh1)
length(wh2)
data_edges = edges[-c(wh1, wh2), ]


data_edges$cls_dir = 0
data_edges$cls_dir = (data_edges$tsorder1 < data_edges$tsorder2)*2 #first node transmits to second node
wh = which(data_edges$tsorder1 == data_edges$tsorder2)
if(length(wh) > 0) 
  data_edges = data_edges[-wh, ]


data_edges$onset1 = as.Date(data_edges$onset1,"%Y-%m-%d",tz = "EST5EDT")
data_edges$onset2 = as.Date(data_edges$onset2,"%Y-%m-%d",tz = "EST5EDT")


label$order = paste(label$from, "+",label$to)
data_edges$order1  = paste(data_edges$id, "+",data_edges$contact_id )
data_edges$order2 = paste(data_edges$contact_id, "+",data_edges$id )
for(i in 1:nrow(data_edges)){
  wh1 = which(label$order %in% data_edges$order1[i])
  wh2 = which(label$order %in% data_edges$order2[i])
  if(length(wh1)>0)
    data_edges$direction[i] = 1 # "id to contact"
  else if(length(wh2)>0)
    data_edges$direction[i] = 0 # "contact to id"
  else
    data_edges$direction[i]  = NA
}


data_edges$onset_dir = (data_edges$onset1 < data_edges$onset2)*2 # 2 represents id to contact
# 0 represents contact to id
data_edges$onset_dir[data_edges$onset1 == data_edges$onset2] = 1 # 1 represents not clear

data_edges$onset_dir = as.factor(data_edges$onset_dir)
levels(data_edges$onset_dir ) = c("contact to id","not clear","id to contact")
data_edges$cls_dir = as.factor(data_edges$cls_dir)
levels(data_edges$cls_dir ) = c("contact to id", "id to contact")
data_edges$direction = as.factor(data_edges$direction)
levels(data_edges$direction ) = c("contact to id", "id to contact")

data_edges_label = data_edges[which(! is.na(data_edges$direction)), ]

tab = table(data_edges$cls_dir,data_edges$onset_dir)
#columns: onset_dir; rows:cls_dir
(tab[1,1] + tab[2,3])/sum(tab) # 81% overlap

tab = table(data_edges$cls_dir,data_edges$direction)
(tab[1,1]+tab[2,2] )/ sum(tab) # 86% overlap
tab = table(data_edges$onset_dir,data_edges$direction)
(tab[1,1]+tab[3,2] )/ sum(tab) # 85% overlap


save(data_edges, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/data_edges.rda")
save(data_edges_label, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/data_edges_label.rda")

#### Data adjust
#### Unlabel data
edges_diff = data_edges

unlabel_edges = edges_diff[is.na(edges_diff$direction), ]
temp1 = unlabel_edges$cls_dir
temp2 = unlabel_edges$onset_dir
unlabel_edges = unlabel_edges %>%
  dplyr::select(-cls_dir, -direction, -onset_dir) %>%
  mutate(cls_dir = temp1, onset_dir = temp2)

unlabel_edges_adjust = unlabel_edges
colnames(unlabel_edges_adjust)[1:2] = c("from", "to")
for(i in 1:nrow(unlabel_edges_adjust))
{
  if(unlabel_edges_adjust$cls_dir[i] == "contact to id")
  {
    for(j in 1: 11)
    {
      temp = unlabel_edges_adjust[i, 2*j-1]
      unlabel_edges_adjust[i, 2*j-1] = unlabel_edges_adjust[i, 2*j]
      unlabel_edges_adjust[i, 2*j] = temp
    }
  }
}

unlabel_edges_adjust$if_same = ifelse(as.character(unlabel_edges_adjust$cls_dir) == 
                                        as.character(unlabel_edges_adjust$onset_dir), 1, 0)
unlabel_edges_adjust = unlabel_edges_adjust %>% dplyr::select(-onset_dir, -cls_dir)

unlabel_edges_adjust$occupation1 = as.factor(unlabel_edges_adjust$occupation1)
unlabel_edges_adjust$occupation2 = as.factor(unlabel_edges_adjust$occupation2)

#### Label data
label_edges_adjust = data_edges[! is.na(edges_diff$direction), ]

temp1 = label_edges_adjust$cls_dir
temp2 = label_edges_adjust$onset_dir
temp3 = label_edges_adjust$direction
label_edges_adjust = label_edges_adjust %>%
  dplyr::select(-cls_dir, -direction, -onset_dir) %>%
  mutate(cls_dir = temp1, onset_dir = temp2, direction = temp3)
label_edges_adjust$cls_dir = as.character(label_edges_adjust$cls_dir)
label_edges_adjust$onset_dir = as.character(label_edges_adjust$onset_dir)
label_edges_adjust$direction = as.character(label_edges_adjust$direction)

colnames(label_edges_adjust)[1:2] = c("from", "to")
for(i in 1:nrow(label_edges_adjust))
{
  if(label_edges_adjust$direction[i] == "contact to id")
  {
    for(j in 1: 11)
    {
      temp = label_edges_adjust[i, 2*j-1]
      label_edges_adjust[i, 2*j-1] = label_edges_adjust[i, 2*j]
      label_edges_adjust[i, 2*j] = temp
    }
  }
  label_edges_adjust$cls_dir[i] = ifelse(label_edges_adjust$cls_dir[i] == label_edges_adjust$direction[i],
                                         label_edges_adjust$order1[i], label_edges_adjust$order2[i])
  if(label_edges_adjust$onset_dir[i] != "not clear")
    label_edges_adjust$onset_dir[i] = ifelse(label_edges_adjust$onset_dir[i] == label_edges_adjust$direction[i],
                                             label_edges_adjust$order1[i], label_edges_adjust$order2[i]) 
}

label_edges_adjust = label_edges_adjust %>% dplyr::select(-direction)

label_edges_adjust$occupation1 = as.factor(label_edges_adjust$occupation1)
label_edges_adjust$occupation2 = as.factor(label_edges_adjust$occupation2)




## find the location of wrong predicted edges
g_label = graph_from_data_frame(label_edges_adjust[, 1:2])

label_edge = data.frame(from = label_edges_adjust$from, to = label_edges_adjust$to, edge_type = 0)
head = label_edge[! label_edge$from %in% label_edge$to, 1:2]
tail = label_edge[! label_edge$to %in% label_edge$from, 1:2]

single = intersect(head, tail)
root = setdiff(head, single)
# leaf = setdiff(tail, single)

for(i in 1:nrow(label_edge))
{
  if(nrow(intersect(label_edge[i, 1:2], root)) > 0)
    label_edge$edge_type[i] = 3
  else if(nrow(intersect(label_edge[i, 1:2], tail)) > 0)
    label_edge$edge_type[i] = 1
  else
    label_edge$edge_type[i] = 2
}


label_edge$order = label_edges_adjust$order1
label_edge$TSCAN_wrong = 0
label_edge$onset_wrong = 0

for(i in 1:nrow(label_edge))
{
  if(label_edge$order[i] != label_edges_adjust$cls_dir[i])
    label_edge$TSCAN_wrong[i] = 1
  if(label_edge$order[i] != label_edges_adjust$onset_dir[i])
    label_edge$onset_wrong[i] = 1
}

save(label_edge, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/label_edge.rda")

table(label_edge$edge_type[label_edge$TSCAN_wrong > 0])
table(label_edge$edge_type[label_edge$onset_wrong > 0])
table(label_edge$edge_type[label_edge$TSCAN_wrong > 0])/sum(table(label_edge$edge_type[label_edge$TSCAN_wrong > 0]))
table(label_edge$edge_type[label_edge$onset_wrong > 0])/sum(table(label_edge$edge_type[label_edge$onset_wrong > 0]))
sum(table(label_edge$edge_type[label_edge$TSCAN_wrong > 0]))
sum(table(label_edge$edge_type[label_edge$onset_wrong > 0]))

#### Histogram of the difference of the onset date
dif_onset = (difftime(data_edges$onset1, data_edges$onset2,units = "days") %>% as.numeric)
hist(abs(dif_onset))



## after influence

after_ifl = data.frame(from = label_edges_adjust$from, to = label_edges_adjust$to, 
                       order = label_edges_adjust$order1, TSCAN_wrong = 0, onset_wrong = 0, total = 0)
for(i in 1:nrow(after_ifl))
{
  if(after_ifl$order[i] != label_edges_adjust$cls_dir[i])
    after_ifl$TSCAN_wrong[i] = 1
  if(after_ifl$order[i] != label_edges_adjust$onset_dir[i])
    after_ifl$onset_wrong[i] = 1
}


f <- function(graph, data, extra) data['succ'] == -1
g = graph_from_data_frame(edges,directed = F)
for(i in 1:nrow(after_ifl))
{
  result = bfs(g, root = after_ifl$to[i], dist = TRUE, callback = f)
  after_ifl$total[i] = length(na.omit(result$order)) - 2
}

save(after_ifl, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/after_ifl.rda")


summary(after_ifl[after_ifl$TSCAN_wrong > 0, "total"])
summary(after_ifl[after_ifl$onset_wrong > 0, "total"])
sum(after_ifl[after_ifl$TSCAN_wrong > 0, "total"])
sum(after_ifl[after_ifl$onset_wrong > 0, "total"])
sum(after_ifl$TSCAN_wrong > 0)
sum(after_ifl$onset_wrong > 0)



#### 2. Analysis on the unlabeled data
#### Some plots

#### basic
# onset date
data_onset_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                              onset = c(data_nodes$onset_date2, data_nodes_label$onset_date2),
                              name = "onset date")
data_onset_basic %>%
  ggplot(aes(x = onset)) +
  geom_histogram(aes(col = type), binwidth = 3, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_onset_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                   onset = c(abs(data_edges$onset1 - data_edges$onset2), 
                                             abs(data_edges_label$onset1 - data_edges_label$onset2)),
                                   name = "difference of onset date")
data_onset_basic_diff %>%
  ggplot(aes(x = onset)) +
  geom_histogram(aes(col = type), binwidth = 3, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))
  

# age
data_age_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                              age = c(data_nodes$age_year, data_nodes_label$age_year),
                              name = "age")
data_age_basic %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(col = type), binwidth = 5, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_age_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                   age = c(abs(data_edges$age_year1 - data_edges$age_year2), 
                                             abs(data_edges_label$age_year1 - data_edges_label$age_year2)),
                                   name = "difference of age")
data_age_basic_diff %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(col = type), binwidth = 5, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

# depth
data_depth_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                            depth = c(data_nodes$position, data_nodes_label$position),
                            name = "depth")
data_depth_basic %>%
  ggplot(aes(x = depth)) +
  geom_histogram(aes(col = type), binwidth = 0.1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.75, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_depth_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                 depth = c(abs(data_edges$position1 - data_edges$position2), 
                                         abs(data_edges_label$position1 - data_edges_label$position2)),
                                 name = "difference of depth")
data_depth_basic_diff %>%
  ggplot(aes(x = depth)) +
  geom_histogram(aes(col = type), binwidth = 0.1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.75, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

# degree
data_degree_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                              degree = c(data_nodes$degree, data_nodes_label$degree),
                              name = "degree")
data_degree_basic %>%
  ggplot(aes(x = degree)) +
  geom_histogram(aes(col = type), binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_degree_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                   degree = c(abs(data_edges$degree1 - data_edges$degree2), 
                                             abs(data_edges_label$degree1 - data_edges_label$degree2)),
                                   name = "difference of degree")
data_degree_basic_diff %>%
  ggplot(aes(x = degree)) +
  geom_histogram(aes(col = type), binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

# out degree
data_outdegree_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                               outdegree = c(data_nodes$out_degree, data_nodes_label$out_degree),
                               name = "out degree")
data_outdegree_basic %>%
  ggplot(aes(x = outdegree)) +
  geom_histogram(aes(col = type), binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_outdegree_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                    outdegree = c(abs(data_edges$out_degree1 - data_edges$out_degree2), 
                                               abs(data_edges_label$out_degree1 - data_edges_label$out_degree2)),
                                    name = "difference of out degree")
data_outdegree_basic_diff %>%
  ggplot(aes(x = outdegree)) +
  geom_histogram(aes(col = type), binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

# severity
length(which(data_nodes$severity2 == 0))
length(which(data_nodes$severity2 == 1))
length(which(data_nodes_label$severity2 == 0))
length(which(data_nodes_label$severity2 == 1))
length(which(data_edges$severity1 == data_edges$severity2))
length(which(data_edges$severity1 != data_edges$severity2))
length(which(data_edges_label$severity1 == data_edges_label$severity2))
length(which(data_edges_label$severity1 != data_edges_label$severity2))

# occupation
data_occup_basic = data.frame(type = as.factor(c(rep("whole", nrow(data_nodes)), rep("label", nrow(data_nodes_label)))),
                                  occup = c(data_nodes$occupation, data_nodes_label$occupation),
                                  name = "occupation")
for(i in 1:nrow(data_occup_basic))
{
  if(data_occup_basic$occup[i] == 1)
    data_occup_basic$occup[i] = "business"
  else if(data_occup_basic$occup[i] == 2)
    data_occup_basic$occup[i] = "farmer"
  else if(data_occup_basic$occup[i] == 3)
    data_occup_basic$occup[i] = "unemployed"
  else if(data_occup_basic$occup[i] == 4)
    data_occup_basic$occup[i] = "worker"
  else if(data_occup_basic$occup[i] == 5)
    data_occup_basic$occup[i] = "retired"
  else if(data_occup_basic$occup[i] == 6)
    data_occup_basic$occup[i] = "others"
}
data_occup_basic %>%
  ggplot(aes(x = occup)) +
  geom_bar(aes(col = type), alpha = 0.6, position = "dodge") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))

data_occup_basic_diff_cal = data.frame(label = is.na(data_edges$direction), 
                                       occup1 = (data_edges$occupation1), 
                                       occup2 = (data_edges$occupation2))
for(i in 1:nrow(data_occup_basic_diff_cal))
{
  for(j in 2:3)
  {
    if(data_occup_basic_diff_cal[i,j] == "business")
      data_occup_basic_diff_cal[i,j] = 1
    if(data_occup_basic_diff_cal[i,j] == "farmer")
      data_occup_basic_diff_cal[i,j] = 2
    if(data_occup_basic_diff_cal[i,j] == "unemployed")
      data_occup_basic_diff_cal[i,j] = 3
    if(data_occup_basic_diff_cal[i,j] == "worker")
      data_occup_basic_diff_cal[i,j] = 4
    if(data_occup_basic_diff_cal[i,j] == "retired")
      data_occup_basic_diff_cal[i,j] = 5
    if(data_occup_basic_diff_cal[i,j] == "others")
      data_occup_basic_diff_cal[i,j] = 6
  }
}
data_occup_basic_diff_cal$occup1 = as.numeric(data_occup_basic_diff_cal$occup1)
data_occup_basic_diff_cal$occup2 = as.numeric(data_occup_basic_diff_cal$occup2)

data_occup_basic_diff = data.frame(type = as.factor(c(rep("whole", nrow(data_edges)), rep("label", nrow(data_edges_label)))),
                                       occup = c(abs(data_occup_basic_diff_cal$occup1 - data_occup_basic_diff_cal$occup2), 
                                                     abs(data_occup_basic_diff_cal[which(data_occup_basic_diff_cal$label == FALSE),]$occup1 - 
                                                         data_occup_basic_diff_cal[which(data_occup_basic_diff_cal$label == FALSE),]$occup2)),
                                       name = "difference of occupation")
data_occup_basic_diff %>%
  ggplot(aes(x = occup)) +
  geom_histogram(aes(col = type), binwidth = 1, alpha = 0.5, position = "identity") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + 
  theme(legend.position = c(0.85, 0.85), legend.background = element_rect(fill = "white", color = "grey"))
#########################

diff_set = unlabel_edges_adjust[unlabel_edges_adjust$if_same != 1,]
# write.csv(diff_set, "unlable_edges_in_different_group")
same_set = unlabel_edges_adjust[unlabel_edges_adjust$if_same == 1,]
# write.csv(same_set, "unlable_edges_in_same_group")

data_prop = data.frame(group = c("same", "different"), count = c(nrow(same_set), nrow(diff_set)))
data_prop %>%
  ggplot(aes(x = "", y = count, fill = group)) +
  geom_col(color = "black") +
  coord_polar("y", start=0) +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() + theme_void() +
  ggtitle("Propotion of Same and Different Predicted Edges")

data_onset = data.frame(type = c(rep("different", nrow(diff_set)), rep("same", nrow(same_set))),
                        difference = c(difftime(diff_set$onset1, diff_set$onset2, units = "days"),
                                       abs(difftime(same_set$onset1, same_set$onset2, units = "days"))),
                        name = "onset date difference")
data_onset %>%
  ggplot(aes(x = type, y = difference)) +
  geom_boxplot(aes(fill = type)) +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL) + theme(legend.position = "none")

# data_position = data.frame(type = c(rep("different", nrow(diff_set)), rep("same", nrow(same_set))),
#                         difference = c(abs(diff_set$position1 - diff_set$position2),
#                                        abs(same_set$position1 - same_set$position2)))
# data_position %>%
#   ggplot(aes(x = type, y = difference)) +
#   geom_boxplot(aes(fill = type)) +
#   ggtitle("Boxplot of Difference of Depth in Different Group")

# data_age = data.frame(type = c(rep("from", nrow(diff_set)), rep("to", nrow(diff_set))),
#                       age = c(diff_set$age_year1, diff_set$age_year2))
# data_age %>%
#   ggplot(aes(x = type, y = age)) +
#   geom_boxplot(aes(fill = type)) +
#   ggtitle("Boxplot of Difference of Age in Different Group")

data_degree = data.frame(type = c(rep("our", nrow(diff_set)), rep("traditional", nrow(diff_set)), 
                                  rep("our", nrow(diff_set)), rep("traditional", nrow(diff_set))),
                      value = c(diff_set$degree1, diff_set$degree2, diff_set$out_degree1, diff_set$out_degree2),
                      degree = c(rep("degree", 2*nrow(diff_set)), rep("out-degree", 2*nrow(diff_set))))
data_degree %>%
  ggplot(aes(x = type, y = value)) +
  geom_boxplot(aes(fill = type)) +
  scale_fill_brewer() + theme_light() +
  facet_wrap(~ degree) + xlab(NULL) + ylab(NULL) + theme(legend.position = "none")

# data_outdegree = data.frame(type = c(rep("from", nrow(diff_set)), rep("to", nrow(diff_set))),
#                       outdegree = c(diff_set$out_degree1, diff_set$out_degree2))
# data_outdegree %>%
#   ggplot(aes(x = type, y = outdegree)) +
#   geom_boxplot(aes(fill = type)) +
#   ggtitle("Boxplot of Difference of Out Degree in Different Group") +
#   scale_fill_brewer() + theme_light()

data_occup = data.frame(type = c(c(rep("our", nrow(diff_set)), rep("traditional", nrow(diff_set)))),
                        occup = c(as.character(diff_set$occupation1), as.character(diff_set$occupation2)),
                        name = rep("occupation", 2*nrow(diff_set)))
data_occup %>%
  ggplot() +
  geom_bar(aes(x = type, fill = occup), position = "fill") +
  scale_fill_brewer() + theme_light() + facet_wrap(~ name) +
  xlab(NULL) + ylab(NULL)



#### Workflow plot

data_plot = data.frame(dat_new2)
prcomp(dat_new2)$rotation[,1:2]
prcomp(dat_new2)
pc = prcomp(dat_new2)$rotation
new = as.matrix(dat_new2) %*% pc
plot(new[,1],new[,2],col=lpsmclust$clusterid)



pc = lpsmclust$pcareduceres
pc = cbind(pc, lpsmclust$clusterid)
center = lpsmclust$clucenter
center = center[c(2,1,3),]
#### origin
plot(pc[, 1], pc[, 2], xaxt = 'n', yaxt = 'n', ann=FALSE)
#### after cluster
plot(pc[, 1], pc[, 2], col = c("#FF6600", "#CC0000", "#FFCC33")[pc[, 3]], 
     xaxt = 'n', yaxt = 'n', ann=FALSE)
text(x = center[ ,1], y = center[, 2], c("1", "2", "3"), cex = 2)
#### find the shortest path
plot(pc[, 1], pc[, 2], col = c("#FF6600", "#CC0000", "#FFCC33")[pc[, 3]], 
     xaxt = 'n', yaxt = 'n', ann=FALSE)
text(x = center[ ,1], y = center[, 2], c("1", "2", "3"), cex = 2)
lines(x = center[, 1], y = center[, 2], lwd = 2)
#### project on the path
plot(x = pc[, 1], y = rep(1, nrow(pc)), col = c("#FF6600", "#CC0000", "#FFCC33")[pc[, 3]],
     xaxt = 'n', yaxt = 'n', ann=FALSE)
abline(h = 1)
text(x = center[ ,1], y = rep(1, nrow(center)), c("1", "2", "3"), cex = 2)
text(x = 12, y = 0.95, "pseudo time")











