# Top commands ----

# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

# load library
library(synthpop)
library(tidyverse)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Create simulated data ----

# Generate a data frame with 1,000 observations and 4 columns, each column having random "0"/"1" values
df_ods <- data.frame(
  var1 = sample(c(0, 1), size = 1000, replace = TRUE),
  var2 = sample(c(0, 1), size = 1000, replace = TRUE),
  var3 = sample(c(0, 1), size = 1000, replace = TRUE),
  var4 = sample(c(0, 1), size = 1000, replace = TRUE),
  type = "original"
)


# Number of observations
n <- 1000

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Initialize the dataset
D <- data.frame(matrix(ncol = 4, nrow = n))
colnames(D) <- c("var1", "var2", "var3", "var4")

# Sample the observations with equal probability
for (i in 1:(n)) {
  sampled_row <- sample(1:16, 1)
  D[i,] <- combinations[sampled_row,]
}

# Convert to data frame and print the first few rows
df_ods <- as.data.frame(D)
df_ods$type <- "original"
head(df_ods)

# Reshape the data for plotting
df_combine_long <- df_ods %>%
  pivot_longer(!type, names_to = "variable", values_to = "value") %>%
  mutate(value = as.factor(value)) %>%
  group_by(type,variable,value) %>%
  tally() %>%
  group_by(type,variable) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup()

df_combine_long

# Plot using facet_wrap
p <- ggplot(subset(df_combine_long,type == "original"), aes(x = value, y=n)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = n),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  facet_wrap(~variable) +
  theme_bw() +
  scale_y_continuous(limits = c(0,700), breaks = seq(0,700,100)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

ggsave(p, filename = paste0(graphs,"graph_variable_frequency.pdf"), height = 4, width = 6, units = "in")


df_ods_frequency <- df_ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods_frequency)) * 100

# Plot using facet_wrap
p <- ggplot(subset(df_ods_frequency,type == "original"), aes(x = combine, y=Freq)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = Freq),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

ggsave(p, filename = paste0(graphs,"graph_variable_histogram.pdf"), height = 4, width = 6, units = "in")

# synthetic data ----

sds <- syn(df_ods, m = 1, seed = my.seed)
df_sds <- sds$syn


df_sds$type <- "synthetic"
df_ods$type <- "original"

df_combine <- rbind(df_sds,df_ods)

# Reshape the data for plotting
df_combine_long <- df_combine %>%
  pivot_longer(!type, names_to = "variable", values_to = "value") %>%
  mutate(value = as.factor(value)) %>%
  group_by(type,variable,value) %>%
  tally() %>%
  group_by(type,variable) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup()

df_combine_long

# Plot using facet_wrap
p <- ggplot(subset(df_combine_long), aes(x = value, y=n, fill = type)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = n),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  facet_wrap(~variable) +
  theme_bw() +
  scale_y_continuous(limits = c(0,700), breaks = seq(0,700,100)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

ggsave(p, filename = paste0(graphs,"graph_variable_frequency_compare.pdf"), height = 4, width = 6, units = "in")

# Compare histogram ----

df_ods_frequency <- df_ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
df_ods_frequency$type <- "original"

df_sds_frequency <- df_sds
df_sds_frequency$combine <- paste(df_sds_frequency$var1, df_sds_frequency$var2, df_sds_frequency$var3, df_sds_frequency$var4, sep = "")
df_sds_frequency <- df_sds_frequency %>%
  select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds_frequency))
df_sds_frequency$pct <- (df_sds_frequency$Freq / nrow(df_sds)) * 100
df_sds_frequency$type <- "original"
df_sds_frequency$type <- "synthetic"

df_combine <- rbind(df_ods_frequency,df_sds_frequency)

# Plot using facet_wrap
p <- ggplot(subset(df_combine), aes(x = combine, y=Freq, fill = type)) +
  geom_bar(stat = 'identity',position = position_dodge(.9)) +
  geom_text(aes(label = Freq),vjust = -0.5, size = 4,position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

p

ggsave(p, filename = paste0(graphs,"graph_variable_histogram_compare.pdf"), height = 4, width = 6, units = "in")

# utility ----

compare(sds, df_ods, plot = FALSE, utility.stats = "all")

# privacy ----

t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, 
                         keys = c("var1", "var2", "var3"), target = "var4")
print(t1)
print(t1,to.print = "ident")

t1 <- disclosure.summary(sds, df_ods, print.flag = FALSE, plot = TRUE, 
                         keys = c("var2", "var3", "var4"), target = "var1")
print(t1)
print(t1,to.print = "ident")

print(t1,to.print = "attrib")
print(t1, to.print = "allCAPs")
print(t1,plot = TRUE)
replicated.uniques (sds, df_ods)
