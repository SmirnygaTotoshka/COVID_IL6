setwd("~/Documents/Piter/COVID_IL6/notebooks/")
library(dplyr)
library(ggplot2)
data = readxl::read_excel("../tables/for_Anton.xlsx", sheet = "Цитокины",col_types = "num")
str(data)
data1 = apply(data, MARGIN = 2, function(x){gsub("<|>",NA,x)}) %>% 
  as.data.frame()%>% 
  mutate_at(colnames(data)[-1], as.numeric)
str(data1)
heatmap(data1[,-1] %>% as.matrix())
data.withoutID = as.data.frame(data[,-1])
plot(data.withoutID)
V = var(data.withoutID,na.rm = T)
cor_df <- round(cov2cor(V), 2)
rownames(cor_df) = colnames(cor_df)
melted_cor <- reshape2::melt(cor_df)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
