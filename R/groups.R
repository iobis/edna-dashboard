groups <- read.csv("data/supporting_data/groups.csv")
group_names <- groups %>% 
  distinct(group, label)
group_names <- setNames(as.list(group_names$group), group_names$label)
group_names <- c(list("All species" = "all species"), group_names)
