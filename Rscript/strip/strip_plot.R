library(ggplot2)
library(dplyr)

draw_plot <- function(selected_data, type) {
print("here?1")
  data <- selected_data[1]
  n <- selected_data[2]

data <- as.data.frame(lapply(data, function(x) {
  if (all(!is.na(as.numeric(as.character(x))))) {
    return as.numeric(as.character(x))
  } else if (is.factor(x)) {
    return as.numeric(x)
  } else {
    return x
  }
}))

  
  # Create species labels
  species_labels <- ifelse(substr(selected_data$Species, 1, 1) == "1", 
                           paste(levels(selected_data$Strain_no), "faecalis", sep = " "), 
                           paste(levels(selected_data$Strain_no), "faecium", sep = " "))
print(species_labels)


                           
  # Add a new column for plotting labels
#   data <- data %>%
#     mutate(label = species_labels)
  
#   # Create a ggplot
#   p <- ggplot(data) +
#     # Background grid lines
#     geom_hline(yintercept = seq(0.5, n + 0.5, 1), color = "grey90") +
#     geom_hline(yintercept = c(10.5, 20.5), linetype = "solid", size = 1) +
    
#     # Points and segments
#     geom_point(aes(x = mode.log.MIC, y = 1:n), shape = 0, size = 2) +
#     geom_point(aes(x = E.log.MIC, y = 1:n - 0.25), shape = 15, size = 2) +
#     geom_segment(aes(x = lower.log.MIC, xend = upper.log.MIC, y = 1:n - 0.25, yend = 1:n - 0.25)) +
#     geom_point(aes(x = lower.log.MIC.ref, y = 1:n + 0.25), color = "black", size = 2) +
#     geom_point(aes(x = upper.log.MIC.ref, y = 1:n + 0.25), shape = 16, color = "black", size = 2) +
#     geom_segment(aes(x = lower.log.MIC.ref, xend = upper.log.MIC.ref, y = 1:n + 0.25, yend = 1:n + 0.25), color = "black") +
    
#     # Axis labels and title
#     scale_x_continuous("MIC", breaks = seq(-1, 9), labels = 2^(-1:9)) +
#     scale_y_continuous("", breaks = 1:n, labels = species_labels) +
#     labs(title = name) +
    
#     # Theme adjustments
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(size = 10),
#       axis.text.y = element_text(size = 8),
#       axis.title.x = element_text(size = 12),
#       plot.title = element_text(hjust = 0.5, size = 14),
#       plot.margin = unit(c(1, 1, 2, 1), "lines")
#     )
  
#   # Add legend
#   legend_data <- data.frame(
#     x = c(NA, NA, NA, NA),
#     y = c(NA, NA, NA, NA),
#     labels = c("Mode MICs", "Mean MICs", "Interval Censoring", "Reference MICs"),
#     shapes = c(0, 15, 1, 16)
#   )
  
#   p <- p +
#     geom_point(data = legend_data, aes(x = x, y = y, shape = labels), show.legend = TRUE) +
#     scale_shape_manual(values = c(0, 15, 1, 16)) +
#     theme(legend.position = "bottomright") +
#     guides(shape = guide_legend(override.aes = list(size = 5)))
  
#   print(p)
# }


# combine_plots<-function(data, data2,len_col,name){
  
# n <- len_col
  
#   # Create species labels
#   species_labels <- ifelse(substr(data$Species, 1, 1) == "1", 
#                            paste(levels(data$Strain_no), "faecalis", sep = " "), 
#                            paste(levels(data$Strain_no), "faecium", sep = " "))

#   d <- 0.25
#   d2 <- 0.42

#   # Prepare data for plotting
#   data <- data %>%
#     mutate(species_label = species_labels, group = "Etest")
  
#   data2 <- data2 %>%
#     mutate(species_label = species_labels, group = "MTS")

#   # Combine both dataframes
#   combined_data <- bind_rows(
#     data %>% mutate(offset = 0, d_offset = d, color = "red"),
#     data2 %>% mutate(offset = -0.1, d_offset = d2, color = "blue")
#   )

#   # Plot
#   p <- ggplot(combined_data, aes(x = mode.log.MIC, y = 1:n + offset, color = color)) +
#     geom_hline(yintercept = seq(0.5, n + 0.5, 1), color = "grey90") +
#     geom_hline(yintercept = c(10.5, 20.5), linetype = "solid", size = 1) +
#     geom_point(aes(shape = "Mode MICs"), size = 2) +
#     geom_point(aes(x = E.log.MIC, y = 1:n - d_offset, shape = "Mean MICs"), size = 2) +
#     geom_segment(aes(x = lower.log.MIC, xend = upper.log.MIC, y = 1:n - d_offset, yend = 1:n - d_offset)) +
#     geom_point(aes(x = lower.log.MIC.ref, y = 1:n + d_offset, shape = "Interval censoring"), size = 2) +
#     geom_point(aes(x = upper.log.MIC.ref, y = 1:n + d_offset, shape = "Reference MICs"), size = 2) +
#     geom_segment(aes(x = lower.log.MIC.ref, xend = upper.log.MIC.ref, y = 1:n + d_offset, yend = 1:n + d_offset)) +
#     scale_x_continuous("MIC", breaks = seq(-1, 10), labels = 2^(-1:10)) +
#     scale_y_continuous("", breaks = 1:n, labels = species_labels) +
#     scale_color_manual(values = c("red" = "red", "blue" = "blue"), guide = "none") +
#     scale_shape_manual(values = c("Mode MICs" = 0, "Mean MICs" = 15, "Interval censoring" = 1, "Reference MICs" = 16)) +
#     labs(title = name) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(size = 10),
#       axis.text.y = element_text(size = 8),
#       axis.title.x = element_text(size = 12),
#       plot.title = element_text(hjust = 0.5, size = 14),
#       plot.margin = unit(c(1, 1, 2, 1), "lines")
#     ) +
#     guides(shape = guide_legend(title = "Legend", override.aes = list(color = "black")))

#   # Print plot
#   print(p)

}

