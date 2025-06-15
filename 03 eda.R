#####################################################################
###                   3. Exploratory Data Analysis                ###
#####################################################################

options(scipen = 999)  # renuntam la notatie stiitifica (cu exponent)
#install.packages('tidyverse')
library(tidyverse)
#install.packages('readxl')
library(readxl)  # import fisiere excel
library(DataExplorer)
library(corrplot)
library(corrgram)
library(scales)
library(patchwork)
library(viridis)
library(ggsci)
#install.packages('svglite')
library(svglite)
# https://corrr.tidymodels.org/reference/correlate.html
library(corrr)
library(gtsummary)


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
###                            The data set                       ###
#####################################################################

# Frecvența scale factor
table(main_df$scale_factor)

# Echivalent tidyverse
main_df %>%
  group_by(scale_factor) %>%
  tally()

# Sau direct
main_df %>%
  count(scale_factor)



table(main_df$db_system)
table(main_df$scale_factor, main_df$db_system)

# 
main_df |>
     group_by(query_id) |>
     tally() |>
     filter(n > 1)


# Setează folderul de lucru pentru figuri și tabele
main_dir <- "C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R"

# Creează subfolderul 'Diagrams' dacă nu există deja
dir.create(file.path(main_dir, "Diagrams"), showWarnings = FALSE)

# Setează folderul ca working directory
setwd(file.path(main_dir, "Diagrams"))
getwd()


#########################################################################
###                     I. Query parameters                           ###
#########################################################################
glimpse(main_df)

df <- main_df %>%
  select(
    scale_factor, execution_time_ms, no_of_nodes,
    no_of_joins, where_clauses, group_by_clauses, having_clauses,
    no_of_subqueries, no_of_distinct,
    no_of_subqueries_lookup_pipeline, no_of_group_clauses, no_of_match_filters,
    no_of_lookup_joins, no_of_where_filters, no_of_subqueries_aggregate,
    db_system
  ) %>%
  distinct() %>%
  mutate(
    scale_factor = factor(scale_factor),
    db_system = factor(db_system)
  )
glimpse(df)

# Raport exploratoriu rapid
DataExplorer::create_report(df, output_file = "eda_report.html", y = "execution_time_ms")
DataExplorer::create_report(main_df_pg, output_file = "eda_postgresql.html")
DataExplorer::create_report(main_df_mongo, output_file = "eda_mongodb.html")

main_df_pg_meta %>%
  ggplot(aes(x = factor(scale_factor), y = execution_time_ms)) +
  geom_boxplot() +
  labs(
    title = "PostgreSQL Execution Time by Scale Factor",
    x = "Scale Factor",
    y = "Execution Time (ms)"
  ) +
  theme_minimal()


main_df_mongo_meta %>%
  ggplot(aes(x = factor(scale_factor), y = execution_time_ms)) +
  geom_boxplot() +
  labs(
    title = "MongoDB Execution Time by Scale Factor",
    x = "Scale Factor",
    y = "Execution Time (ms)"
  ) +
  theme_minimal()


#########################################################################
##                Ia. Tables with descriptive statistics               ## 
#########################################################################
getwd()

## the simplest tables (yet not very useful)
## all
fig101 <- main_df |>
     tbl_summary()
fig101

fig101 |>
     gtsummary::as_flex_table() |>
     flextable::save_as_image(path = "101 descriptive_statistics1a.png")

fig101 |>
     as_gt() |>  # convert to gt table
     gt::gtsave( # save table as image
          filename = "101 descriptive_statistics1b.png")

fig101 |>
     as_gt() |>  # convert to gt table
     gt::gtsave( # save table as docx document
          filename = "101 descriptive_statistics1c.docx")

fig101 |>
     as_gt() |>  # convert to gt table
     gt::gtsave( # save table as pdf document
          filename = "101 descriptive_statistics1c.pdf")

## PostgreSQL 
fig201 <- main_df_pg_meta |>
  tbl_summary()
fig201

fig201 |>
  gtsummary::as_flex_table() |>
  flextable::save_as_image(path = "201 descriptive_statistics1a.png")

fig201 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as image
    filename = "201 descriptive_statistics1b.png")

fig201 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as docx document
    filename = "201 descriptive_statistics1c.docx")

fig201 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as pdf document
    filename = "201 descriptive_statistics1c.pdf")

## MongoDB 

# Selectăm doar coloanele numerice
main_df_mongo_numeric <- main_df_mongo_meta %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  select(where(is.numeric))

# Aplicăm tbl_summary()
fig301 <- main_df_mongo_numeric |>
  tbl_summary(
    statistic = list(all_continuous() ~ c("{median} [{min}, {max}]"))
  )

fig301


fig301 |>
  gtsummary::as_flex_table() |>
  flextable::save_as_image(path = "301 descriptive_statistics1a.png")

fig301 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as image
    filename = "301 descriptive_statistics1b.png")

fig301 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as docx document
    filename = "301 descriptive_statistics1c.docx")

fig301 |>
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as pdf document
    filename = "301 descriptive_statistics1c.pdf")

## a more elaborate table
glimpse(df)
------------------------------------------------------------------
  
## Citire date
  
library(readxl)
library(janitor)
library(dplyr)

# Setează directorul principal (dacă nu e deja)
setwd("C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R")

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
  --------------------------------------------------------------------------------
## PostgreSQL

library(gtsummary)
library(flextable)
library(gt)

# tabel sumar grupat după scale_factor
fig102 <- main_df_pg_meta %>%
  tbl_summary(
    by = scale_factor,
    statistic = list(
      all_continuous() ~ "{median} [{min}, {max}]"
    )
  )

# export ca imagine și .docx
fig102 %>%
  as_flex_table() %>%
  save_as_image("pg_102_descriptive_statistics_by_sf.png")

gtsave(as_gt(fig102), "pg_102_descriptive_statistics_by_sf.docx")


## an even more elaborate table
glimpse(main_df_pg_meta)
num_vars <- main_df_pg_meta |>
     keep(is.numeric) |>
     names()

t1 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{min}"))
          ) |>
     modify_header(stat_0 ~ "**Min**")
t1

t2 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{p25}"))) |>
     modify_header(stat_0 ~ "**Q1**")
t2 

t3 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{p50}"))) |>
     modify_header(stat_0 ~ "**Median**")
t3 

t4 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{p75}"))) |>
     modify_header(stat_0 ~ "**Q3**")
t4 

t5 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{max}"))) |>
     modify_header(stat_0 ~ "**Max**")
t5

t6 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{mean}"))) |>
     modify_header(stat_0 ~ "**Mean**")
t6

t7 <- main_df_pg_meta |>
     keep(is.numeric) |>
     tbl_summary(
          type = list(all_of(num_vars) ~ "continuous"),          
          statistic = list(all_continuous() ~ c("{sd}"))) |>
     modify_header(stat_0 ~ "**SD**")
t7

fig103 <- tbl_merge (list(t1, t2, t3, t4, t5, t6, t7), 
                     tab_spanner = NULL) |>
     modify_spanning_header(everything() ~ NA_character_) |>
     modify_footnote(everything() ~ NA) |>
     modify_header(label ~ "**Variable**") |>
     bold_labels()

fig103


fig103 |>
     gtsummary::as_flex_table() |>
     flextable::save_as_image(path = "103 descriptive_statistics3.png")

fig103 |>
     as_gt() |>  # convert to gt table
     gt::gtsave( # save table as docx document
          filename = "103 descriptive_statistics3.docx")

## MongoDB
library(gtsummary)
library(flextable)
library(gt)
library(dplyr)
library(purrr)


# fig102 - tabel sumar grupat după scale_factor
fig102_mongo <- main_df_mongo_meta %>%
  tbl_summary(
    by = scale_factor,
    statistic = list(
      all_continuous() ~ "{median} [{min}, {max}]"
    )
  )

# export fig102 ca imagine și .docx
fig102_mongo %>%
  as_flex_table() %>%
  save_as_image("mongo_102_descriptive_statistics_by_sf.png")

gtsave(as_gt(fig102_mongo), "mongo_102_descriptive_statistics_by_sf.docx")

# extragem doar coloanele numerice
glimpse(main_df_mongo_meta)
num_vars <- main_df_mongo_meta |>
  keep(is.numeric) |>
  names()

t1 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{min}"))
  ) |>
  modify_header(stat_0 ~ "**Min**")
t1

t2 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{p25}"))) |>
  modify_header(stat_0 ~ "**Q1**")
t2 

t3 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{p50}"))) |>
  modify_header(stat_0 ~ "**Median**")
t3 

t4 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{p75}"))) |>
  modify_header(stat_0 ~ "**Q3**")
t4 

t5 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{max}"))) |>
  modify_header(stat_0 ~ "**Max**")
t5

t6 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{mean}"))) |>
  modify_header(stat_0 ~ "**Mean**")
t6

t7 <- main_df_mongo_meta |>
  keep(is.numeric) |>
  tbl_summary(
    type = list(all_of(num_vars) ~ "continuous"),          
    statistic = list(all_continuous() ~ c("{sd}"))) |>
  modify_header(stat_0 ~ "**SD**")
t7

fig203 <- tbl_merge (list(t1, t2, t3, t4, t5, t6, t7), 
                     tab_spanner = NULL) |>
  modify_spanning_header(everything() ~ NA_character_) |>
  modify_footnote(everything() ~ NA) |>
  modify_header(label ~ "**Variable**") |>
  bold_labels()

fig203


fig103 |>
  gtsummary::as_flex_table() |>
  flextable::save_as_image(path = "103 descriptive_statistics3.png")


# Export fig103
fig103_mongo %>%
  as_flex_table() %>%
  save_as_image("mongo_103_descriptive_statistics3.png")
fig103_mongo
gtsave(as_gt(fig103_mongo), "mongo_103_descriptive_statistics3.docx")


#########################################################################
##        Ib. Display the number missing values for each variable      ## 
#########################################################################

## PostgreSQL

library(tidyverse)

missing_vals_pg <- main_df_pg_meta %>%
  map_int(~ sum(is.na(.) | . == "N/A")) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate(percent_missing = round(n_missing * 100 / nrow(main_df_pg_meta), 2))

ggplot(missing_vals_pg, 
       aes(x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, "%")),
            hjust = if_else(missing_vals_pg$percent_missing > 3, 1.02, -0.03),
            vjust = 0.5, size = 3.5) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 170, 20)) +
  labs(title = "Missing Values per Column – PostgreSQL")


## MongoDB
library(tidyverse)

missing_vals_mongo <- main_df_mongo_meta %>%
  map_int(~ sum(is.na(.) | . == "N/A")) %>%
  tibble(variable = names(.), n_missing = .) %>%
  mutate(percent_missing = round(n_missing * 100 / nrow(main_df_mongo_meta), 2))

ggplot(missing_vals_mongo, 
       aes(x = variable, y = n_missing, fill = variable)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste0(percent_missing, "%")),
            hjust = if_else(missing_vals_mongo$percent_missing > 3, 1.02, -0.03),
            vjust = 0.5, size = 3.5) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 170, 20)) +
  labs(title = "Missing Values per Column – MongoDB")


#########################################################################
###               I.c Data distribution - nominal variables          ###
#########################################################################


eda_factors_pg <- main_df_pg_meta %>%
  mutate(
    scale_factor = as.character(scale_factor),
    no_of_nodes = as.character(no_of_nodes)
  ) %>%
  select(scale_factor, no_of_nodes) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(value = coalesce(value, "N/A")) %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(main_df_pg_meta), 2)) %>%
  arrange(variable, value)

View(eda_factors_pg)

eda_factors_mongo <- main_df_mongo_meta %>%
  mutate(
    scale_factor = as.character(scale_factor),
    no_of_nodes = as.character(no_of_nodes)
  ) %>%
  select(scale_factor, no_of_nodes) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(value = coalesce(value, "N/A")) %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(main_df_mongo_meta), 2)) %>%
  arrange(variable, value)

View(eda_factors_mongo)


#########################################################################
###                 Ic. Data distribution - numeric variables         ###
#########################################################################
glimpse(df_pg)
summary(df_pg)
glimpse(df_mongo)
summary(df_mongo)

# Extrage variabilele numerice și le transformă în format lung
num_variables_pg <- main_df_pg_meta %>%
  select_if(is.numeric) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Calculează numărul de valori distincte pe fiecare variabilă numerică
num_variables_distinct_values_pg <- num_variables_pg %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value), .groups = "drop")

View(num_variables_distinct_values_pg)

# Extrage variabilele numerice și le transformă în format lung
num_variables_mongo <- main_df_mongo_meta %>%
  select_if(is.numeric) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Calculează numărul de valori distincte pe fiecare variabilă numerică
num_variables_distinct_values_mongo <- num_variables_mongo %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value), .groups = "drop")

View(num_variables_distinct_values_mongo)


# for variables with less of equal 5 distinct values, 
#   display bar plots
g1_pg <- num_variables_pg %>%
  semi_join(
    num_variables_distinct_values_pg %>% filter(n_distinct_values <= 5),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_pg), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.4, fill = "#1f77b4") +
  geom_text(
    aes(label = paste0(round(percent, 0), '%'),
        vjust = if_else(n_value > 400, 1.5, -0.5)),
    size = 4.5
  ) +
  facet_wrap(~ variable, scale = "free", ncol = 2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    strip.text = element_text(size = 14)
  ) +
  xlab("") + ylab("Frequency")

x_pg <- g1_pg + patchwork::plot_layout(nrow = 1)
ggsave("112a_pg_low_cardinality_numeric_variables.pdf", plot = x_pg, device = "pdf")


g1_mongo <- num_variables_mongo %>%
  semi_join(
    num_variables_distinct_values_mongo %>% filter(n_distinct_values <= 5),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_mongo), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.4, fill = "#ff7f0e") +
  geom_text(
    aes(label = paste0(round(percent, 0), '%'),
        vjust = if_else(n_value > 400, 1.5, -0.5)),
    size = 4.5
  ) +
  facet_wrap(~ variable, scale = "free", ncol = 2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    strip.text = element_text(size = 14)
  ) +
  xlab("") + ylab("Frequency")

x_mongo <- g1_mongo + patchwork::plot_layout(nrow = 1)
ggsave("112a_mongo_low_cardinality_numeric_variables.pdf", plot = x_mongo, device = "pdf")


# for variables with 6-10 distinct values, 
#   display also bar plots
g2_pg <- num_variables_pg %>%
  semi_join(
    num_variables_distinct_values_pg %>%
      filter(n_distinct_values > 5 & n_distinct_values <= 10),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_pg), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.4, fill = "#1f77b4") +
  geom_text(
    aes(label = paste0(round(percent, 0), '%'),
        vjust = if_else(n_value > 350, 1.5, -0.5)),
    size = 3
  ) +
  facet_wrap(~ variable, scale = "free", ncol = 2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    strip.text = element_text(size = 14)
  ) +
  xlab("") + ylab("Frequency")

x2_pg <- g2_pg + patchwork::plot_layout(nrow = 1)
ggsave("112b_pg_medium_cardinality_numeric_variables.pdf", plot = x2_pg, device = "pdf")

g2_mongo <- num_variables_mongo %>%
  semi_join(
    num_variables_distinct_values_mongo %>%
      filter(n_distinct_values > 5 & n_distinct_values <= 10),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_mongo), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.4, fill = "#ff7f0e") +
  geom_text(
    aes(label = paste0(round(percent, 0), '%'),
        vjust = if_else(n_value > 350, 1.5, -0.5)),
    size = 3
  ) +
  facet_wrap(~ variable, scale = "free", ncol = 2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    strip.text = element_text(size = 14)
  ) +
  xlab("") + ylab("Frequency")

x2_mongo <- g2_mongo + patchwork::plot_layout(nrow = 1)
ggsave("112b_mongo_medium_cardinality_numeric_variables.pdf", plot = x2_mongo, device = "pdf")


# for variables with more than 10 distinct values, 
#   display histogram
# separate histogram for each numeric value; free scale
g3_pg <- num_variables_pg %>%
  filter(variable == "execution_time_ms") %>%
  ggplot(aes(x = value)) +
  geom_histogram(alpha = 0.6, bins = 30, fill = "#2C3E50") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 18)
  ) +
  xlab("") + ylab("Frequency")

x3_pg <- g3_pg + patchwork::plot_layout(nrow = 1)
ggsave("112c1_pg_high_cardinality_numeric_variables.pdf", plot = x3_pg, device = "pdf")



g3_mongo <- num_variables_mongo %>%
  filter(variable == "execution_time_ms") %>%
  ggplot(aes(x = value)) +
  geom_histogram(alpha = 0.6, bins = 30, fill = "#FF5733") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    strip.text = element_text(size = 18)
  ) +
  xlab("") + ylab("Frequency")

x3_mongo <- g3_mongo + patchwork::plot_layout(nrow = 1)
ggsave("112c_mongo_high_cardinality_numeric_variables.pdf", plot = x3_mongo, device = "pdf")



### boxplot for all numeric variables
g1_mongo <- num_variables_mongo %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "#E67E22", alpha = 0.5) +
  facet_wrap(~ variable, ncol = 3, scales = "free", strip.position = "top") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),  # ascundem complet textul de pe X (nefolosit)
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),  # mărim denumirea variabilelor
    panel.spacing = unit(1.2, "lines")  # spațiu între boxploturi
  ) +
  xlab("") + ylab("Value")

x_mongo <- g1_mongo + plot_layout(nrow = 1)
ggsave("112d_mongo_boxplots.pdf", plot = x_mongo, device = "pdf")
ggsave("112d_mongo_boxplots.png", plot = x_mongo, device = "png")

g1_pg <- num_variables_pg %>%
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "#3498DB", alpha = 0.5) +
  facet_wrap(~ variable, ncol = 4, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("") + ylab("Value") +
  theme(axis.text.x = element_blank()) +
  theme(strip.text.x = element_text(size = 10))

x_pg <- g1_pg + plot_layout(nrow = 1, byrow = FALSE)
ggsave("112d_pg_boxplots.pdf", plot = x_pg, device = "pdf")
ggsave("112d_pg_boxplots.png", plot = x_pg, device = "png")


## correlation plot
# Correlation plot - MongoDB
corrplot::corrplot(
  cor(
    main_df_mongo_meta %>%
      select_if(is.numeric),
    method = "spearman"
  ),
  method = "number",
  type = "upper",
  tl.cex = .7,
  number.cex = 0.7
)
# Exportă manual imaginea din RStudio



#########################################################################
###             III. EDA for the scoring (regression) model            ###
#########################################################################
### We'll keep only the completed queries on both scenarios

df_pg_scoring <- main_df %>%
  filter(db_system == "postgresql") %>%
  select(scale_factor, execution_time_ms, query_id) %>%
  mutate(
    scale_factor = factor(scale_factor),
    log10_duration = log10(execution_time_ms),
    db_system = factor("PostgreSQL")  # redundant dacă deja era setat, dar păstrat pentru claritate
  )

ggplot(df_pg_scoring, aes(x = execution_time_ms)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ scale_factor, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Execution Time Distribution in PostgreSQL by Scale Factor",
    x = "Execution Time (ms)",
    y = "Frequency"
  )

ggsave('C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R/pg_histogram_by_sf.png')


df_mongo_scoring <- main_df %>%
  filter(db_system == "mongodb") %>%
  filter(!is.na(execution_time_ms)) %>%
  select(scale_factor, execution_time_ms, query_id) %>%
  mutate(
    db_system = factor("MongoDB"),
    scale_factor = factor(scale_factor),
    log10_duration = log10(execution_time_ms)
  )

ggplot(df_mongo_scoring, aes(x = execution_time_ms)) +
  geom_histogram(bins = 30, fill = "#27AE60", alpha = 0.6) +
  facet_wrap(~ scale_factor, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Execution Time Distribution in MongoDB by Scale Factor",
    x = "Execution Time (ms)",
    y = "Frequency"
  )

ggsave('C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R/mongo_histogram_by_sf.png')



#########################################################################
##                IIIa. Tables with descriptive statistics             ## 
#########################################################################
# PostgreSQL subset
df_pg_scoring <- main_df %>%
  filter(!is.na(execution_time_ms)) %>%
  select(scale_factor, execution_time_ms, query_id) %>%
  mutate(
    db_system = "postgreSQL",
    duration_sec = execution_time_ms / 1000,
    log10_duration = log10(execution_time_ms)
  )

# MongoDB subset
df_mongo_scoring <- main_df %>%
  filter(!is.na(execution_time_ms)) %>%
  select(scale_factor, execution_time_ms, query_id) %>%
  mutate(
    db_system = "mongodb",
    duration_sec = execution_time_ms / 1000,
    log10_duration = log10(execution_time_ms)
  )

# Combine both
df <- bind_rows(
  df_pg_scoring,
  df_mongo_scoring
) %>%
  mutate(
    scale_factor = factor(scale_factor),
    model_db = factor(db_system)
  )

# Build Table – Figure 301
fig301 <- df %>%
  transmute(
    scale_factor = factor(paste0("SF ", scale_factor)),
    model_db,
    duration_sec,
    log10_duration
  ) %>%
  tbl_strata(
    strata = model_db,
    .tbl_fun = ~ .x %>%
      tbl_summary(by = scale_factor, missing = "no", 
                  statistic = list(all_continuous() ~ "{mean} ± {sd}")) %>%
      modify_header(label ~ "**Metric**"),
    .header = "**{strata}**, N = {n}"
  )


# Afișare + salvare
fig301
fig301 %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_image(path = "301_descriptive_statistics_duration.png")


#########################################################################
###                 IIId. Data distribution - numeric variables        ###
#########################################################################

# Variabile numerice în format lung
pg_num_variables <- main_df_pg_meta %>%
  select_if(is.numeric) %>%
  select(-scale_factor) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Număr de valori distincte
pg_num_variables_distinct_values <- pg_num_variables %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value)) %>%
  ungroup()

View(pg_num_variables_distinct_values)


# Variabile numerice în format lung
mongo_num_variables <- main_df_mongo_meta %>%
  select_if(is.numeric) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Număr de valori distincte
mongo_num_variables_distinct_values <- mongo_num_variables %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value)) %>%
  ungroup()

View(mongo_num_variables_distinct_values)


# for variables with less of equal 10 distinct values, 
#   display bar plots

# Calcul variabile numerice în format lung 
num_variables_pg <- main_df_pg_meta %>%
  select(where(is.numeric)) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Calcul cardinalitate distinctă
pg_num_variables_distinct_values <- num_variables_pg %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value)) %>%
  ungroup()

# Plot pentru variabile cu ≤ 10 valori distincte
g1_pg <- num_variables_pg %>%
  semi_join(
    pg_num_variables_distinct_values %>%
      filter(n_distinct_values <= 10),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_pg), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.5) +
  geom_text(aes(label = paste0(round(percent, 0), '%'),
                vjust = if_else(n_value > 500, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free", ncol = 3) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5),
        strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency")

x_pg <- g1_pg + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("303a_pg_low_cardinality_numeric_variables.pdf", plot = x_pg, device = "pdf")
ggsave("303a_pg_low_cardinality_numeric_variables.png", plot = x_pg, device = "png")


# Variabile numerice în format lung (fără scale_factor)
num_variables_mongo <- main_df_mongo_meta %>%
  select(where(is.numeric)) %>%
  mutate(row_num = row_number()) %>%
  pivot_longer(-row_num, names_to = "variable", values_to = "value")

# Cardinalitate distinctă
mongo_num_variables_distinct_values <- num_variables_mongo %>%
  group_by(variable) %>%
  summarise(n_distinct_values = n_distinct(value)) %>%
  ungroup()

# Plot
g1_mongo <- num_variables_mongo %>%
  semi_join(
    mongo_num_variables_distinct_values %>%
      filter(n_distinct_values <= 10),
    by = "variable"
  ) %>%
  ungroup() %>%
  group_by(variable, value) %>%
  summarise(n_value = n(), .groups = "drop") %>%
  mutate(percent = round(n_value * 100 / nrow(num_variables_mongo), 2)) %>%
  arrange(variable, value) %>%
  ggplot(aes(x = value, y = n_value)) +
  geom_col(alpha = 0.5) +
  geom_text(aes(label = paste0(round(percent, 0), '%'),
                vjust = if_else(n_value > 500, 1.5, -0.5)), size = 3) +
  facet_wrap(~ variable, scale = "free", ncol = 3) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5),
        strip.text.x = element_text(size = 10)) +
  xlab("") + ylab("frequency")

x_mongo <- g1_mongo + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("303a_mongo_low_cardinality_numeric_variables.pdf", plot = x_mongo, device = "pdf")
ggsave("303a_mongo_low_cardinality_numeric_variables.png", plot = x_mongo, device = "png")


### boxplot for all numeric variables
g1_pg <- num_variables_pg %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, ncol = 4, scales = "free") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 9)
  ) +
  xlab("") + ylab("value")

x_pg <- g1_pg + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("303c_pg_boxplots.pdf", plot = x_pg, device = "pdf")
ggsave("303c_pg_boxplots.png", plot = x_pg, device = "png")


g1_mongo <- num_variables_mongo %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, ncol = 4, scales = "free") +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 9)
  ) +
  xlab("") + ylab("value")

x_mongo <- g1_mongo + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("303c_mongo_boxplots.pdf", plot = x_mongo, device = "pdf")
ggsave("303c_mongo_boxplots.png", plot = x_mongo, device = "png")


#########################################################################
###   IIIe. The outcome: duration_sec; transformation log10(duration) ###
#########################################################################

# Adaptare pentru PostgreSQL
df_pg_scoring <- main_df_pg_meta %>%
  select(scale_factor, execution_time_ms) %>%
  rename(duration_sec = execution_time_ms)  

glimpse(df_pg_scoring)

# Boxplot simplu
g1_pg <- ggplot(df_pg_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1200, by = 50))

x_pg <- g1_pg + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("311a_pg_duration_sec.pdf", plot = x_pg,  device = "pdf") 
ggsave("311a_pg_duration_sec.png", plot = x_pg,  device = "png") 

# Facet wrap pe scale factor
ggplot(df_pg_scoring %>% mutate(scale_factor = factor(scale_factor)), 
       aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  facet_wrap(~ scale_factor, scale = "free", labeller = label_both) +
  theme(legend.position = "none") 

# Zoom pe valori mici
g_zoom_pg <- ggplot(df_pg_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                    aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ggforce::facet_zoom(ylim = c(0, 155)) 

x_zoom_pg <- g_zoom_pg + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("312b_pg_duration_sec.pdf", plot = x_zoom_pg,  device = "pdf") 
ggsave("312b_pg_duration_sec.png", plot = x_zoom_pg,  device = "png") 


# Adaptare pentru MongoDB
df_mongo_scoring <- main_df_mongo_meta %>%
  select(scale_factor, execution_time_ms) %>%
  mutate(duration_sec = execution_time_ms / 1000)

glimpse(df_mongo_scoring)

# Boxplot simplu
g1_mongo <- ggplot(df_mongo_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                   aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1200, by = 50))

x_mongo <- g1_mongo + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("311a_mongo_duration_sec.pdf", plot = x_mongo,  device = "pdf") 
ggsave("311a_mongo_duration_sec.png", plot = x_mongo,  device = "png") 

# Facet wrap pe scale factor
ggplot(df_mongo_scoring %>% mutate(scale_factor = factor(scale_factor)), 
       aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  facet_wrap(~ scale_factor, scale = "free", labeller = label_both) +
  theme(legend.position = "none") 

# Zoom pe valori mici
g_zoom_mongo <- ggplot(df_mongo_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                       aes(x = scale_factor, y = duration_sec)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ggforce::facet_zoom(ylim = c(0, 155)) 

x_zoom_mongo <- g_zoom_mongo + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("312b_mongo_duration_sec.pdf", plot = x_zoom_mongo,  device = "pdf") 
ggsave("312b_mongo_duration_sec.png", plot = x_zoom_mongo,  device = "png") 



# Pregătim datele PostgreSQL
df_pg_scoring <- main_df_pg_meta %>%
  select(scale_factor, execution_time_ms) %>%
  mutate(
    duration_sec = execution_time_ms,
    log10_duration = log10(duration_sec)
  )

# Boxplot log10_duration PostgreSQL
g1_pg <- ggplot(df_pg_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                aes(x = scale_factor, y = log10_duration)) +
  geom_boxplot() +
  theme(legend.position = "none")

x_pg <- g1_pg + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("313_pg_log10_duration.pdf", plot = x_pg,  device = "pdf") 
ggsave("313_pg_log10_duration.png", plot = x_pg,  device = "png") 

# Pregătim datele MongoDB
df_mongo_scoring <- main_df_mongo_meta %>%
  select(scale_factor, execution_time_ms) %>%
  mutate(
    duration_sec = execution_time_ms / 1000,
    log10_duration = log10(duration_sec)
  )

# Boxplot log10_duration MongoDB
g1_mongo <- ggplot(df_mongo_scoring %>% mutate(scale_factor = factor(scale_factor)), 
                   aes(x = scale_factor, y = log10_duration)) +
  geom_boxplot() +
  theme(legend.position = "none")

x_mongo <- g1_mongo + patchwork::plot_layout(nrow = 1, byrow = FALSE)
ggsave("313_mongo_log10_duration.pdf", plot = x_mongo,  device = "pdf") 
ggsave("313_mongo_log10_duration.png", plot = x_mongo,  device = "png") 
