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
url <- "C:\\Users\\Я\\OneDrive\\Рабочий стол\\lpnu lab\\4 curs\\Visualization\\Visualization\\Books data.csv"
#url <- "~/Developer/Visualization/Books data.csv"  # Update path as needed

data <- read.csv(url)
#View(data)
text_data <- data %>%
  select(Book.Title, Author)

text_data$combined_text <- paste(text_data$Book.Title, text_data$Author, sep = " ")

# Create tokens from the combined text field
it <- itoken(text_data$combined_text, tokenizer = word_tokenizer)

# Create a vocabulary from the tokens
vocab <- create_vocabulary(it)

# Create the vectorizer using the vocabulary
vectorizer <- vocab_vectorizer(vocab)

# Create the document-term matrix (DTM)
dtm <- create_dtm(it, vectorizer)

# Perform hierarchical clustering using Ward's method
distance_matrix <- dist(as.matrix(dtm), method = "euclidean")  # Euclidean distance on DTM
hc <- hclust(distance_matrix, method = "ward.D2")  # Hierarchical clustering

# Convert the hierarchical clustering into a dendrogram object
dend <- as.dendrogram(hc)

# Color the dendrogram based on the clusters
# Cut the dendrogram into a desired number of clusters (e.g., 3)
dend_colored <- color_branches(dend, k = 3)  # "k" defines the number of clusters

# Add book titles as labels to the dendrogram
dend_labeled <- dend_colored %>%
  set("labels", text_data$Book.Title)

# Set label size
dend_labeled <- dend_labeled %>%
  set("labels_cex", 0.6)

# Adjust margins for readability
par(mar = c(15, 10, 3, 2))  # Modify margins (bottom, left, top, right)

# Plot the dendrogram
plot(dend_labeled, main = "Dendrogram of Book Dataset (Title & Author) with Color",
     xlab = "", ylab = "Distance")

# Add x-axis label for clarity
mtext("Books", side = 1, line = 14, cex = 1)
#------------------------------------------------------------------------------------
#data <- head(data, 20)

arc_data <- data %>%
  select(Book.Title, Category)

# Identify shared categories
shared_categories <- arc_data %>%
  group_by(Category) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(Book.Title, Category) %>%
  distinct()

# Create a data frame for edges (relations between books that share the same category)
edges <- shared_categories %>%
  inner_join(shared_categories, by = "Category") %>%
  filter(Book.Title.x != Book.Title.y) %>%
  select(from = Book.Title.x, to = Book.Title.y) %>%
  distinct()

# Ensure all vertices are included in the vertices data frame
all_vertices <- unique(c(edges$from, edges$to))
vertices <- data.frame(name = all_vertices, stringsAsFactors = FALSE)

# Create a graph from the edges data frame
graph <- graph_from_data_frame(d = edges, directed = FALSE, vertices = vertices)

# Create an arc diagram using ggraph
ggraph(graph, layout = "linear") +
  geom_edge_arc(aes(edge_alpha = ..index..), edge_width = 1, edge_colour = "blue") +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.5, "lines")) +
  theme_void() +
  labs(title = "Arc Diagram of Books with Shared Categories")

---------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
