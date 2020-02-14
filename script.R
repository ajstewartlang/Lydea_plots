library(tidyverse)

# the following function takes the parameters of a dataset and uses them to 
# build a dataframe. 
create_df <- function(my_df){
  set.seed(1234)
  y <- c(my_df$gp1_pop_mean, my_df$gp2_pop_mean)
  x <- c(my_df$gp1_label, my_df$gp2_label)
  df <- as_tibble(cbind(y, x)) %>%
    mutate(y = as.double(y)) %>%
    mutate(x = as.factor(x))
}

# creating a wrapper function to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# the following function takes a tibble, a label for the x-axis, a label for 
# the y-axis, and a title and constructs a bar graph
# geom_point with alpha 0 is used to extend the y axis 1.15x the height of the tallest bar
my_bar_graph <- function(df, labx, laby, title) {
  df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(x = x, y = 1.15*y), alpha = 0) +
    stat_summary(geom = "col", fun.y = mean, width = 0.5) +
    labs(x = labx, y = laby, title = title) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank()) +
    expand_limits(y = 0) +
    ggtitle(wrapper(title, width = 45)) +
    theme(panel.background = element_rect(fill = "white"))
}

# the following functions take a graph and saves it in the graphs folder

save_neutral_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_neutral_", index, ".jpg"), current_graph)
}  

save_neg_slant_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_neg_slant_", index, ".jpg"), current_graph)
}  

# MAIN CODE ####
# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book.csv")

# this loops through the my_graphs which contains the paramters of the 
# graphs to be generated.  It runs once per each unique graph_id 

# build bar graphs
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  my_bar_graph(build_this_one, 
               my_graphs[my_graphs$graph_id == index,]$x_label, 
               my_graphs[my_graphs$graph_id == index,]$y_label, 
               my_graphs[my_graphs$graph_id == index,]$neutral_title) %>%
    save_neutral_graph()
  
  my_bar_graph(build_this_one, 
              my_graphs[my_graphs$graph_id == index,]$x_label, 
              my_graphs[my_graphs$graph_id == index,]$y_label, 
              my_graphs[my_graphs$graph_id == index,]$neg_slant_title) %>%
    save_neg_slant_graph()
}

