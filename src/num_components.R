# Function to display the number of components used over time.

# Input: t_0, T, list of number of components and dates
# Output: Figure of the number of components over time

num_components <- function(t_0, T, comp.num_list, dates){
  x <- 1:length(comp.num_list)
  df.toplot_compnum <- data.frame(x)
  ggplot(df.toplot_compnum, aes(x)) + 
    geom_line(aes(y=comp.num_list, color="1"), size=0.8) + 
    scale_colour_manual(name = "Legend", 
                        values=c("cadetblue3"), 
                        labels = c("DPLS")) + 
    scale_x_continuous(breaks = seq(t_0+1,t_0+T,T/20), labels = dates[seq(t_0+1,t_0+T,T/20)]) +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust=1.2)) +
    labs(y = "Number of components", x="")
  ggsave(file="figure/Number_of_components_DPLS.png", width=10, height=4, dpi=400)
}