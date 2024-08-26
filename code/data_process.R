data_all = matrix(NA, nrow(data_edges), 3)
data_edges$direction = as.character(data_edges$direction)
data_edges$direction[which(is.na(data_edges$direction))] = 0
for(i in 1:nrow(data_edges))
{
  if(data_edges$direction[i] == "contact to id")
  {
    data_all[i, 1] = data_edges$contact_id[i]
    data_all[i, 2] = data_edges$id[i]
    data_all[i, 3] = "Directed"
  }
  else if(data_edges$direction[i] == "id to contact")
  {
    data_all[i, 2] = data_edges$contact_id[i]
    data_all[i, 1] = data_edges$id[i]
    data_all[i, 3] = "Directed"
  }
  else
  {
    data_all[i, 2] = data_edges$contact_id[i]
    data_all[i, 1] = data_edges$id[i]
    data_all[i, 3] = "Undirected"
  }
}
data_all = as.data.frame(data_all)
colnames(data_all) = c("from", "to", "Direction")
load("/Users/james/Documents/lab/covid-19 graph/final paper/gephi/data_nodes.rda")
data_nodes$Label = rownames(data_nodes)
rownames(data_nodes) = NULL
data_nodes$Id = rownames(data_nodes)

data_all$Source = data_all$Target = NA
for(i in 1:nrow(data_all))
{
  data_all$Source[i] = data_nodes$Id[which(data_nodes$Label == data_all$from[i])]
  data_all$Target[i] = data_nodes$Id[which(data_nodes$Label == data_all$to[i])]
}
data_nodes = data_nodes %>% select(Id, Label, degree, onset_date2)

write.csv(data_all, file = "/Users/james/Documents/lab/covid-19 graph/final paper/data_all_edges.csv")
write.csv(data_nodes, file = "/Users/james/Documents/lab/covid-19 graph/final paper/data_all_nodes.csv")
