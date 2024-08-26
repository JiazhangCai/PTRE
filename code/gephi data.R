#### data for gephi
library(tidyverse)

load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/label_edge.rda")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/data_nodes.rda")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/data_nodes_label.rda")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/id_cls.rda")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/data_edges.rda")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/data_edges_label.rda")



#### Labeled edges
gephi_nodes_label = data_nodes_label
gephi_nodes_label$Label = rownames(gephi_nodes_label)
gephi_nodes_label$Id = seq(1:nrow(gephi_nodes_label))
rownames(gephi_nodes_label) = gephi_nodes_label$Id
gephi_nodes_label$cls = NA
summ = summary(gephi_nodes_label$age_year)
for(i in 1:nrow(gephi_nodes_label))
{
  gephi_nodes_label$cls[i] = id_cls$cls[which(id_cls$id == gephi_nodes_label$Label[i])]
  if(gephi_nodes_label$age_year[i] <= summ[2])
    gephi_nodes_label$age_year[i] = 1
  else if((gephi_nodes_label$age_year[i] > summ[2]) & (gephi_nodes_label$age_year[i] <= summ[3]))
    gephi_nodes_label$age_year[i] = 2
  else if((gephi_nodes_label$age_year[i] > summ[3]) & (gephi_nodes_label$age_year[i] <= summ[5]))
    gephi_nodes_label$age_year[i] = 3
  else
    gephi_nodes_label$age_year[i] = 4
}
gephi_nodes_label = gephi_nodes_label %>%
  select(Id, Label, everything())
write.csv(gephi_nodes_label, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/nodes_label.csv",
          row.names = FALSE)

gephi_edges_label = data_edges_label
gephi_edges_label$Source = gephi_edges_label$Target = NA
gephi_edges_label$if_cls_correct = gephi_edges_label$if_onset_correct = NA
gephi_edges_label$diff_time = gephi_edges_label$dff_age = gephi_edges_label$edge_type = NA
gephi_edges_label$Type = "Directed"
gephi_edges_label$Id = seq(1, nrow(gephi_edges_label))
for(i in 1:nrow(gephi_edges_label))
{
  if(gephi_edges_label$direction[i] == "contact to id")
  {
    gephi_edges_label$Source[i] = gephi_nodes_label$Id[which(gephi_nodes_label$Label == gephi_edges_label$contact_id[i])]
    gephi_edges_label$Target[i] = gephi_nodes_label$Id[which(gephi_nodes_label$Label == gephi_edges_label$id[i])]
    gephi_edges_label$edge_type[i] = label_edge$edge_type[which(label_edge$order == gephi_edges_label$order2[i])]
  }
  else
  {
    gephi_edges_label$Source[i] = gephi_nodes_label$Id[which(gephi_nodes_label$Label == gephi_edges_label$id[i])]
    gephi_edges_label$Target[i] = gephi_nodes_label$Id[which(gephi_nodes_label$Label == gephi_edges_label$contact_id[i])]
    gephi_edges_label$edge_type[i] = label_edge$edge_type[which(label_edge$order == gephi_edges_label$order1[i])]
  }
  gephi_edges_label$if_cls_correct[i] = ifelse(gephi_edges_label$cls_dir[i] == gephi_edges_label$direction[i], 1, 0)
  gephi_edges_label$if_onset_correct[i] = ifelse(gephi_edges_label$onset_dir[i] == gephi_edges_label$direction[i], 1, 0)
  gephi_edges_label$diff_time[i] = abs(difftime(gephi_edges_label$onset1[i], gephi_edges_label$onset2[i], units = "days"))
  gephi_edges_label$diff_age[i] = abs(as.numeric(gephi_edges_label$age_year1[i]) - as.numeric(gephi_edges_label$age_year2[i]))
}
gephi_edges_label_cls = gephi_edges_label
gephi_edges_label_cls$edge_type[which(gephi_edges_label_cls$if_cls_correct == 1)] = 0
gephi_edges_label_cls$Weight = abs(gephi_edges_label_cls$if_cls_correct - 2)
gephi_edges_label_cls$Weight[which(gephi_edges_label_cls$Weight == 2)] = 3
gephi_edges_label_cls = gephi_edges_label_cls %>%
  select(Source, Target, Type, Id, Weight, if_cls_correct, if_onset_correct, diff_time, diff_age, edge_type)
write.csv(gephi_edges_label_cls, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/cls_edges_label.csv",
          row.names = FALSE)

gephi_edges_label_onset = gephi_edges_label
gephi_edges_label_onset$edge_type[which(gephi_edges_label_onset$if_onset_correct == 1)] = 0
gephi_edges_label_onset$Weight = abs(gephi_edges_label_onset$if_onset_correct - 2)
gephi_edges_label_onset$Weight[which(gephi_edges_label_onset$Weight == 2)] = 3
write.csv(gephi_edges_label_onset, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/onset_edges_label.csv",
          row.names = FALSE)


#### Unlabeled Edges
gephi_nodes = data_nodes
gephi_nodes$Label = rownames(gephi_nodes)
gephi_nodes$Id = seq(1:nrow(gephi_nodes))
rownames(gephi_nodes) = gephi_nodes$Id
gephi_nodes$cls = gephi_nodes$tsorder = NA
for(i in 1:nrow(gephi_nodes))
{
  gephi_nodes$cls[i] = id_cls$cls[which(id_cls$id == gephi_nodes$Label[i])]
  gephi_nodes$tsorder[i] = which(tsorder == gephi_nodes$Label[i])
}
gephi_nodes = gephi_nodes %>% unique() %>%
  select(Id, Label, everything())
write.csv(gephi_nodes, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/nodes.csv",
          row.names = FALSE)

gephi_edges = data_edges
gephi_edges$Source = gephi_edges$Target = NA
gephi_edges$if_cls_correct = gephi_edges$if_onset_correct = 2
gephi_edges$diff_time = gephi_edges$dff_age = NA
gephi_edges$Type = "Directed"
gephi_edges$Id = seq(1, nrow(gephi_edges))
for(i in 1:nrow(gephi_edges))
{
  if(gephi_edges$cls_dir[i] == "contact to id")
  {
    gephi_edges$Source[i] = gephi_nodes$Id[which(gephi_nodes$Label == gephi_edges$contact_id[i])]
    gephi_edges$Target[i] = gephi_nodes$Id[which(gephi_nodes$Label == gephi_edges$id[i])]
  }
  else if(gephi_edges$cls_dir[i] == "id to contact")
  {
    gephi_edges$Source[i] = gephi_nodes$Id[which(gephi_nodes$Label == gephi_edges$id[i])]
    gephi_edges$Target[i] = gephi_nodes$Id[which(gephi_nodes$Label == gephi_edges$contact_id[i])]
  }
  
  if(! is.na(gephi_edges$direction[i]))
  {
    gephi_edges$if_cls_correct[i] = ifelse(gephi_edges$cls_dir[i] == gephi_edges$direction[i], 1, 0)
    gephi_edges$if_onset_correct[i] = ifelse(gephi_edges$onset_dir[i] == gephi_edges$direction[i], 1, 0) 
  }
  gephi_edges$diff_time[i] = abs(difftime(gephi_edges$onset1[i], gephi_edges$onset2[i], units = "days"))
  gephi_edges$diff_age[i] = abs(as.numeric(gephi_edges$age_year1[i]) - as.numeric(gephi_edges$age_year2[i]))
}
gephi_edges = gephi_edges %>% unique() %>%
  select(Source, Target, Type, Id, if_cls_correct, if_onset_correct, diff_time, diff_age)
write.csv(gephi_edges, file = "/Users/jiazhangcai/Documents/study/lab/covid-19 graph/final paper/gephi/edges.csv",
          row.names = FALSE)
