install.packages("circlize")
install.packages("networkD3")
install.packages(c("plotly", "tidyverse"))
library(networkD3)
library(circlize)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(plotly)

url <- "C:\\Users\\Я\\OneDrive\\Рабочий стол\\lpnu lab\\4 curs\\Visualization\\Project\\Migration_Flows_from_2010_to_2019.csv"
# url <- "~/Developer/Visualization/Migration_Flows_from_2010_to_2019.csv"
us_migration_data <- read.csv(url)

filtered_data <- us_migration_data[!grepl("^abroad_", us_migration_data$from), ]
data <- head(filtered_data, 1000)

states <- unique(c(data$current_state, data$from))

# Initialize a matrix with all values set to zero
migration_matrix <- matrix(0, nrow = length(states), ncol = length(states),
                           dimnames = list(states, states))

# Fill the matrix with the number of people migrating from one state to another
for (i in 1:nrow(data)) {
  # Update the value for migration from current state to origin state
  migration_matrix[data$current_state[i], data$from[i]] <- data$number_of_people[i]
  
  # Update the value for migration from origin state to current state (assuming two-way migration)
  migration_matrix[data$from[i], data$current_state[i]] <- data$number_of_people[i]
}

chordDiagram(migration_matrix, transparency = 0.5, reduce = 0)
#---------------------------------------------------------------------------------------------------------------

sankey_data <- data %>%
  select(current_state, from, number_of_people) %>%
  rename(source = current_state, target = from, value = number_of_people)

node_colors <- RColorBrewer::brewer.pal(length(unique(c(sankey_data$source, sankey_data$target))), "Set3")

plot_ly(
  type = "sankey",
  orientation = "h",  # Horizontal orientation
  node = list(
    label = unique(c(sankey_data$source, sankey_data$target)),
    pad = 15,
    thickness = 20,
    line = list(color = "black", width = 0.5),
    color = node_colors  # Assign unique colors to each node
  ),
  link = list(
    source = match(sankey_data$source, unique(c(sankey_data$source, sankey_data$target))) - 1,
    target = match(sankey_data$target, unique(c(sankey_data$source, sankey_data$target))) - 1,
    value = sankey_data$value,
    color = scales::col_numeric(palette = "Blues", domain = NULL)(sankey_data$value)  # Color links by value
  )
) %>%
  layout(title = "State Migration Sankey Diagram")
#------------------------------------------------------------------------------
cleaned_data <- filtered_data %>%
  group_by(from, current_state) %>%
  summarise(number_of_people = sum(number_of_people, na.rm = TRUE), .groups = "drop")

# Create the migration matrix
migration_matrix <- cleaned_data %>%
  pivot_wider(
    names_from = current_state,
    values_from = number_of_people,
    values_fill = 0  # Fill missing values with 0
  ) %>%
  column_to_rownames(var = "from")  # Convert 'from' column to rownames

# Convert to a matrix
migration_matrix <- as.matrix(migration_matrix)

# Plot the heatmap
heatmap(
  migration_matrix,
  Rowv = NA,  # Disable reordering of rows
  Colv = NA,  # Disable reordering of columns
  scale = "none",  # Keep the original scale
  col = colorRampPalette(c("white", "blue"))(50),  # Color gradient from red to blue
  main = "Migration Heatmap",
  xlab = "To (Current State)",
  ylab = "From"
)
#----------------------------------------------------------------------------------

heatmap(
  migration_matrix,
  scale = "none", 
  col = colorRampPalette(c("white", "blue"))(50),
  main = "Migration Heatmap with Dendrogram",
  xlab = "To (Current State)",
  ylab = "From",
  margins = c(5, 10)
)










