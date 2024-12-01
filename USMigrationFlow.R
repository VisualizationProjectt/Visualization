install.packages("circlize")
install.packages("networkD3")
install.packages(c("plotly", "tidyverse"))
library(networkD3)
library(circlize)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(plotly)

url <- "C:\\Users\\Я\\OneDrive\\Рабочий стол\\lpnu lab\\4 curs\\Visualization\\Visualization\\Migration_Flows_from_2010_to_2019.csv"

# Load data
us_migration_data <- read.csv(url)

# Filter out entries with 'abroad_' and limit dataset to 800 entries
filtered_data <- us_migration_data[!grepl("^abroad_", us_migration_data$from), ]
data <- head(filtered_data, 800)

# Calculate total migration for each state (origin + destination)
state_totals <- data %>%
  group_by(from) %>%
  summarise(total_from = sum(number_of_people, na.rm = TRUE)) %>%
  bind_rows(
    data %>%
      group_by(current_state) %>%
      summarise(total_to = sum(number_of_people, na.rm = TRUE)) %>%
      rename(from = current_state)
  ) %>%
  group_by(from) %>%
  summarise(total_migration = sum(total_from, total_to, na.rm = TRUE)) %>%
  arrange(desc(total_migration))

# Select the top 10 states based on total migration
top_10_states <- state_totals$from[1:10]

# Filter the data to include only the top 10 states
data <- data %>%
  filter(from %in% top_10_states & current_state %in% top_10_states)

# Extract unique states
states <- unique(c(data$current_state, data$from))

# Initialize a migration matrix for the top 10 states
migration_matrix <- matrix(0, nrow = length(states), ncol = length(states),
                           dimnames = list(states, states))

# Populate the migration matrix
for (i in 1:nrow(data)) {
  migration_matrix[data$current_state[i], data$from[i]] <- data$number_of_people[i]
  migration_matrix[data$from[i], data$current_state[i]] <- data$number_of_people[i]
}

# Plot the chord diagram
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










