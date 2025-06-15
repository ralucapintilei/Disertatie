#####################################################################
###                        2. Data Preparation                    ###
#####################################################################

options(scipen = 999)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggforce)
library(DataExplorer)
library(readxl)


setwd('C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R')

df <- read_excel("main_df.xlsx", sheet = "AllResults", col_types = "text") %>%
  clean_names() %>%
  rename(
    model_db = model,
    no_of_joins = join_uri,
    where_clauses = clauze_where,
    group_by_clauses = clauze_group_by,
    having_clauses = clauze_having,
    no_of_subqueries = subinterogari,
    no_of_distinct = distinct,
    
    no_of_subqueries_lookup_pipeline = subconsultari_lookup_pipeline,
    no_of_group_clauses = grupari_group,
    no_of_match_filters = filtre_match,
    no_of_lookup_joins = nr_join_uri_lookup,
    no_of_where_filters = filtre_in_clauza_where,
    no_of_subqueries_aggregate = nr_subconsultari_aggregate
  ) %>%
  mutate(
    execution_time_ms = as.numeric(execution_time_ms),
    no_of_nodes = as.integer(no_of_nodes),
    scale_factor = as.numeric(scale_factor),
    
    no_of_joins = as.numeric(no_of_joins),
    where_clauses = as.numeric(where_clauses),
    group_by_clauses = as.numeric(group_by_clauses),
    having_clauses = as.numeric(having_clauses),
    no_of_subqueries = as.numeric(no_of_subqueries),
    no_of_distinct = as.numeric(no_of_distinct),
    
    no_of_subqueries_lookup_pipeline = as.numeric(no_of_subqueries_lookup_pipeline),
    no_of_group_clauses = as.numeric(no_of_group_clauses),
    no_of_match_filters = as.numeric(no_of_match_filters),
    no_of_lookup_joins = as.numeric(no_of_lookup_joins),
    no_of_where_filters = as.numeric(no_of_where_filters),
    no_of_subqueries_aggregate = as.numeric(no_of_subqueries_aggregate)
  )


#####################################################################
###                             Query batches                     ###
#####################################################################
main_df <- df %>%
  mutate(
    db_system = case_when(
      str_detect(model_db, "mongodb") ~ "1-mongodb",
      str_detect(model_db, "postgresql") ~ "0-postgresql",
      TRUE ~ model_db
    )
  ) %>%
  select(-model_db) %>%
  mutate(
    execution_time_ms = as.numeric(execution_time_ms),
    scale_factor = as.numeric(scale_factor),
    no_of_nodes = as.integer(no_of_nodes)
  )



glimpse(main_df)
table(main_df$scale_factor)
table(main_df$db_system)
table(main_df$no_of_nodes)

#####################################################################
###              Histogramă și distribuții de bază                ###
#####################################################################

library(stringr)

main_df <- main_df %>%
  mutate(db_system = str_remove(db_system, "^[0-1]-"))

main_df %>%
  filter(execution_time_ms <= 100000) %>%
  ggplot(aes(x = execution_time_ms, fill = db_system)) +
  geom_histogram(bins = 50, alpha = 0.8) +
  facet_wrap(~db_system, scales = "free_y") +
  ggtitle("Execution Time Distribution by Database System (< 100,000 ms)") +
  labs(x = "Execution Time (ms)", y = "Frequency") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


#####################################################################
###             Grafic comparativ între sisteme                   ###
#####################################################################

main_df %>%
  group_by(db_system, scale_factor, no_of_nodes) %>%
  summarise(mean_exec_time = mean(execution_time_ms, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(scale_factor), y = mean_exec_time, fill = db_system)) +
  geom_col(position = "dodge") +
  facet_wrap(~ no_of_nodes) +
  labs(
    title = "Average Execution Time by System, Scale Factor, and Number of Nodes",
    x = "Scale Factor",
    y = "Execution Time (ms)",
    fill = "System"
  ) +
  theme_minimal()


#####################################################################
###            Analiză influență metadate pentru PostgreSQL       ###
#####################################################################

main_df_pg <- main_df %>%
  filter(db_system == "postgresql") %>%
  select(
    execution_time_ms,
    no_of_joins,
    where_clauses,
    group_by_clauses,
    having_clauses,
    no_of_subqueries,
    no_of_distinct
  ) %>%
  mutate(across(everything(), as.numeric))

# Compute correlation matrix and plot
main_df_pg %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(
    method = "number",
    type = "upper",
    tl.cex = 0.7,
    number.cex = 0.6
  )

#####################################################################
###            Analiză influență metadate pentru MongoDB          ###
#####################################################################
main_df_mongo <- main_df %>%
  filter(db_system == "mongodb") %>%
  select(
    execution_time_ms,
    no_of_lookup_joins,
    no_of_match_filters,
    no_of_group_clauses,
    no_of_subqueries_lookup_pipeline,
    no_of_subqueries_aggregate,
    no_of_where_filters
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0)) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  corrplot::corrplot(
    method = "number",
    type = "upper",
    tl.cex = 0.7,
    number.cex = 0.6
  )
#####################################################################
###                 Export pentru analiză ulterioară              ###
#####################################################################
rio::export(main_df, file = "clean_main_df.xlsx")


#####################################################################
###                  Distribuții și diagrame brute                ###
#####################################################################
main_df_pg_meta <- main_df %>%
  filter(db_system == "postgresql") %>%
  select(
    execution_time_ms,
    no_of_joins,
    where_clauses,
    group_by_clauses,
    having_clauses,
    no_of_subqueries,
    no_of_distinct,
    scale_factor,
    no_of_nodes
  ) %>%
  mutate(across(everything(), as.numeric))


main_df_pg_meta %>% plot_bar()

main_df_pg_meta %>%
  select(execution_time_ms, no_of_joins, where_clauses, group_by_clauses, having_clauses, no_of_subqueries, no_of_distinct) %>%
  plot_histogram()

main_df %>%
  mutate(execution_time_ms = as.numeric(execution_time_ms)) %>%
  ggplot(aes(execution_time_ms)) +
  geom_histogram(bins = 50) +
  ggforce::facet_zoom(xlim = c(0, 1000)) +
  facet_wrap(~ db_system)

main_df_pg_meta %>%
  pivot_longer(
    cols = c(
      no_of_joins,
      where_clauses,
      group_by_clauses,
      having_clauses,
      no_of_subqueries,
      no_of_distinct
    ),
    names_to = "metadatum",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#69b3a2") +
  facet_wrap(~ metadatum, scales = "free") +
  labs(title = "Distribution of Query Metadata Features (PostgreSQL)") +
  theme_minimal()

# Histogramă pentru metadate MongoDB

main_df_mongo_meta <- main_df %>%
  filter(db_system == "mongodb") %>%
  select(
    execution_time_ms,
    no_of_lookup_joins,
    no_of_match_filters,
    no_of_group_clauses,
    no_of_subqueries_lookup_pipeline,
    no_of_subqueries_aggregate,
    no_of_where_filters,
    scale_factor,
    no_of_nodes
  ) %>%
  mutate(across(everything(), as.numeric))

main_df_mongo_meta %>%
  pivot_longer(
    cols = c(
      no_of_lookup_joins,
      no_of_match_filters,
      no_of_group_clauses,
      no_of_subqueries_lookup_pipeline,
      no_of_subqueries_aggregate,
      no_of_where_filters
    ),
    names_to = "metadatum",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#ff7f0e") +
  facet_wrap(~ metadatum, scales = "free") +
  labs(title = "Distribution of Query Metadata Features (MongoDB)") +
  theme_minimal()


# Distribuție generală timpi execuție
main_df_mongo <- main_df %>%
  filter(db_system == "mongodb") %>%
  as.data.frame()

# Ex: ai făcut split pe db_system
dfs <- split(main_df, main_df$db_system)
main_df_mongo <- dfs$mongodb

main_df_mongo %>%
  mutate(execution_time_ms = as.numeric(execution_time_ms)) %>%
  ggplot(aes(execution_time_ms)) +
  geom_histogram(bins = 50, fill = "#1f77b4") +
  ggforce::facet_zoom(xlim = c(0, 1000)) +
  labs(
    title = "Execution Time Distribution in MongoDB (0–1000 ms)",
    x = "Execution Time (ms)",
    y = "Frequency"
  ) +
  theme_minimal()


################################################ 
##### Adaugate Ulterior  -> ####################
## Statistici sumare pentru timpii de execuție
################################################

main_df %>%
  group_by(db_system) %>%
  summarise(
    n = n(),
    min_time = min(execution_time_ms, na.rm = TRUE),
    median_time = median(execution_time_ms, na.rm = TRUE),
    mean_time = mean(execution_time_ms, na.rm = TRUE),
    max_time = max(execution_time_ms, na.rm = TRUE),
    sd_time = sd(execution_time_ms, na.rm = TRUE)
  )

main_df %>%
  group_by(db_system) %>%
  summarise(
    n = n(),
    min_time = min(execution_time_ms, na.rm = TRUE),
    median_time = median(execution_time_ms, na.rm = TRUE),
    mean_time = mean(execution_time_ms, na.rm = TRUE),
    max_time = max(execution_time_ms, na.rm = TRUE),
    sd_time = sd(execution_time_ms, na.rm = TRUE),
    cv = sd_time / mean_time
  )

main_df %>%
  ggplot(aes(x = db_system, y = execution_time_ms, fill = db_system)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Execution Time Distribution by Database System",
    x = "Database System",
    y = "Execution Time (ms, log scale)"
  ) +
  theme_minimal()

main_df %>%
  group_by(db_system) %>%
  summarise(
    mean_time = mean(execution_time_ms, na.rm = TRUE),
    sd_time = sd(execution_time_ms, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = db_system, y = mean_time, fill = db_system)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_time - sd_time, ymax = mean_time + sd_time), width = 0.2) +
  labs(
    title = "Mean Execution Time with Standard Deviation by System",
    x = "Database System",
    y = "Execution Time (ms)"
  ) +
  theme_minimal()



## Boxplot pentru timpii de execuție pe sisteme și scale factor
main_df %>%
  filter(execution_time_ms < 100000) %>%
  ggplot(aes(x = factor(scale_factor), y = execution_time_ms, fill = db_system)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 10000)) +
  facet_wrap(~ no_of_nodes) +
  labs(
    title = "Boxplot of Execution Time by Scale Factor and DB System",
    x = "Scale Factor",
    y = "Execution Time (ms)",
    fill = "DBMS"
  ) +
  theme_minimal()

## Corelație între numărul de noduri și timpii de execuție
main_df %>%
  ggplot(aes(x = no_of_nodes, y = execution_time_ms, color = db_system)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(
    title = "Execution Time vs Number of Nodes",
    x = "Number of Nodes",
    y = "Execution Time (ms)",
    color = "DBMS"
  ) +
  theme_minimal()

main_df %>%
  filter(execution_time_ms < 1000000) %>%  # eliminăm outlierii extremi
  ggplot(aes(x = factor(no_of_nodes), y = execution_time_ms, fill = db_system)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 0.8) +
  labs(
    title = "Boxplot of Execution Time by Number of Nodes and DBMS",
    x = "Number of Nodes",
    y = "Execution Time (ms)",
    fill = "DBMS"
  ) +
  theme_minimal()

main_df %>%
  filter(execution_time_ms < 1000000) %>%
  ggplot(aes(x = factor(no_of_nodes), y = execution_time_ms, fill = db_system)) +
  geom_violin(scale = "width", alpha = 0.7, trim = TRUE) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, fill = "white") +
  labs(
    title = "Violin Plot of Execution Time by Number of Nodes and DBMS",
    x = "Number of Nodes",
    y = "Execution Time (ms)",
    fill = "DBMS"
  ) +
  theme_minimal()



## Distribuții comparative per Scale Factor
main_df %>%
  filter(execution_time_ms < 100000) %>%
  ggplot(aes(x = execution_time_ms, fill = db_system)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ scale_factor, scales = "free_y") +
  labs(
    title = "Execution Time Density by Scale Factor",
    x = "Execution Time (ms)",
    y = "Density",
    fill = "DBMS"
  ) +
  theme_minimal()



### Note: Pentru graficele și analizele prezentate mai sus (inclusiv boxplot, jitter plot, și corelații) am folosit:
## dplyr, ggplot2, tidyr, stringr