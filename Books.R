# Install necessary packages if not already installed
install.packages("dplyr")
install.packages("tm")
install.packages("ggplot2")
install.packages("text2vec")
install.packages("dendextend")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidyr")

# Load necessary libraries
library(igraph)
library(ggraph)
library(dplyr)
library(tm)
library(ggplot2)
library(text2vec)
library(dendextend)
library(tidyr)

# Step 1: Load the dataset from CSV
url <- "C:\\Users\\Я\\OneDrive\\Рабочий стол\\lpnu lab\\4 curs\\Visualization\\Project\\Books data.csv"  # Update path as needed
# url <- "~/Developer/Visualization/Books data.csv"  # Update path as needed

data <- read.csv(url)

# Step 2: Select only the relevant columns (Book Title and Author)
text_data <- data %>%
  select(Book.Title, Author)

it <- itoken(text_data$Book.Title, tokenizer = word_tokenizer)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)

# Create document term matrices (DTM)
dtm <- create_dtm(it, vectorizer)

# Step 6: Perform hierarchical clustering using Ward's method
distance_matrix <- dist(as.matrix(dtm), method = "euclidean")  # Euclidean distance on DTM
hc <- hclust(distance_matrix, method = "ward.D2")  # Hierarchical clustering

# Step 7: Convert the hierarchical clustering to a dendrogram object
dend <- as.dendrogram(hc)

# Step 8: Color the dendrogram based on the clusters
# Cut the dendrogram into a desired number of clusters (e.g., 3)
dend_colored <- color_branches(dend, k = 3)  # "k" defines the number of clusters
dend_labeled <- dend_colored %>%
  set("labels", text_data$Book.Title)
dend_labeled <- dend_labeled %>%
  set("labels_cex", 0.6)

par(mar = c(15, 10, 3, 2))  # Modify margins (bottom, left, top, right)

# Plot the colored and labeled dendrogram
plot(dend_labeled, main = "Dendrogram of Book Dataset (Title & Author) with Color",
     xlab = "", ylab = "Distance")

mtext("Books", side = 1, line = 14, cex = 1) 
#------------------------------------------------------------------------------------

# Prepare data for graph analysis
books <- data %>%
  select(Book.Title, Category) %>%
  distinct()

# Create a data frame for edges (relations between categories and books)
edges <- data %>%
  select(Book.Title, Category) %>%
  rename(from = Book.Title, to = Category)

# Ensure all vertices are included in the vertices data frame
all_vertices <- unique(c(edges$from, edges$to))
vertices <- data.frame(name = all_vertices, stringsAsFactors = FALSE)

# Create a graph from the edges data frame
graph <- graph_from_data_frame(d = edges, directed = TRUE, vertices = vertices)

# Create a network diagram using ggraph
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(size = degree(graph)), color = "skyblue") +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.5, "lines")) +
  theme_void() +
  labs(title = "Network Diagram of Books and Categories")

# Step 1: Prepare edges for Arc Diagram based on common categories
edges_arc <- data %>%
  select(Book.Title, Category) %>%
  distinct() %>%
  group_by(Category) %>%
  filter(n() > 1) %>%  # Filter categories with only one book
  mutate(other_books = list(setdiff(Book.Title, Book.Title))) %>%
  ungroup() %>%
  unnest(other_books) %>%
  rename(from = Book.Title, to = other_books)

# Step 2: Create a graph object for the arc diagram
graph_arc <- graph_from_data_frame(d = edges_arc, directed = TRUE)

# Step 3: Define layout for Arc Diagram
# In arc diagram, nodes are arranged in a linear layout
layout_arc <- create_layout(graph_arc, layout = "linear")

# Step 4: Plot the Arc Diagram
ggraph(layout_arc) +
  geom_edge_arc(aes(color = Category), alpha = 0.5, show.legend = FALSE) +  # Display edges as arcs
  geom_node_point(color = "lightblue", size = 4) +  # Display nodes as points
  geom_node_text(aes(label = name), size = 3, angle = 90, hjust = 1, vjust = 0.5, repel = TRUE) +  # Add labels to nodes
  theme_void() +  # Clean theme for better visualization
  labs(title = "Arc Diagram of Books with Common Categories")
