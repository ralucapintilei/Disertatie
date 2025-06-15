#####################################################################
###                    4. Inferential Statistics                  ###
#####################################################################

options(scipen = 999)
library(readxl)
library(tidyverse)
library(scales)
library(patchwork)
library(viridis)
library(ggsci)
library(svglite)
#install.packages('ggstatsplot')
library(ggstatsplot)
library(effectsize)
#citation('ggstatsplot')


#####################################################################
###                            The data set                       ###
#####################################################################
main_dir <- 'C:/Users/pinti/OneDrive - FEAA/Desktop/Raluca-R'
setwd(main_dir)
main_df <- read_excel('main_df.xlsx', sheet = "AllResults")
glimpse(main_df)

#########################################################################
###     II. Inferential statistics for the scoring/regression model   ###
#########################################################################

glimpse(main_df)

main_df_clean <- main_df %>%
  rename_with(janitor::make_clean_names) %>%
  mutate(execution_time_ms = as.numeric(execution_time_ms)) %>%
  filter(!is.na(execution_time_ms), execution_time_ms <= 1200000)

valid_queries <- main_df_clean %>%
  group_by(query_id) %>%
  summarise(n_models = n_distinct(model)) %>%
  filter(n_models == 2)

df_scoring <- main_df_clean %>%
  semi_join(valid_queries, by = "query_id") %>%
  select(scale_factor, model, query_id, execution_time_ms, no_of_nodes) %>%  # <- adăugat no_of_nodes
  mutate(
    model = factor(model),
    scale_factor = factor(scale_factor),
    duration_sec = execution_time_ms / 1000,
    log10_duration = log10(execution_time_ms)
  )

H1 - Durata de executie difera semnificativ intre cele servere de baze de date
- cele 2 var durata de executie var numerica Continua, server bd var nominala
- verificam daca distribuia variabilei durata e normala, folosim testul shapiro-wild
- ip nula este ca ditributia e normla
shapiro.test(df_scoring$duration_sec)
- inrucat W = 0.25205, p-value < 0.00000000000000022, p e mult mai mic decat 0.05, inseamna ca ip nula e respira, prin urmare distributia u e normala
- intrucat distributia nu e normala, t asocierea dintr variabile vom folosi testul neparametric mann-whitney
H2 

ggbetweenstats(data = df_scoring, x = model, y = duration_sec, type = 'np')



-- p value e 2.72 . intrucat e mai mic, ip nula a testului mann-whitney este respinsa ( este ca cele 2 distributii sunt echivalente.), prin urmare exista dif semnificative intre cele 2 serverSocket(
 -- effect size e -0.29
 library(effectsize)
 interpret_rank_biserial(-0.29)
-- deci intensitatea efectului este medie.
 
 -- intervalu de incredere e ingust si nu intersecteaz val 0, prin urmare legatura dintre cele 2 variabile este semnificatv statistica, de intensitate medie si stabila. 
 
 
 H1a
 ggbetweenstats(data = df_scoring%>%
                filter(scale_factor == 0.01)  , x = model, y = duration_sec, type = 'np')
 H1d
 ggbetweenstats(data = df_scoring%>%
                  filter(scale_factor == 10)  , x = model, y = duration_sec, type = 'np')

 -- apoi cu no_nodes
 H2 durata ex e asociata cu nr de noduri pe ambele serverd
 
 ggbetweenstats(data = df_scoring%>%
                  mutate(no_of_nodes = factor(no_of_nodes)),
                  x = no_of_nodes, y = duration_sec, type = 'np')
 
 H2a marimea nu influenteaza pe pg
 ggbetweenstats(data = df_scoring%>%
                  mutate(no_of_nodes = factor(no_of_nodes)) %>%
                filter(model == '0 - postgresql'),
                x = no_of_nodes, y = duration_sec, type = 'np')
 
 -- mongo
 
 ggbetweenstats(data = df_scoring%>%
                  mutate(no_of_nodes = factor(no_of_nodes)) %>%
                  filter(model == '1 - mongodb'),

                      x = no_of_nodes, y = duration_sec, type = 'np')
 
 -- si pe scale factor    
 
# Afișare pentru confirmare
glimpse(df_scoring)
table(df_scoring$model)


##  Boxplot duration_sec (logica originală):

g1 <- ggplot(df_scoring, aes(x = scale_factor, y = duration_sec, fill = model)) +
  geom_boxplot(alpha = 0.6) +
  facet_grid(no_of_nodes ~ model, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distributia timpului de executie (secunde)",
       x = "Scale Factor",
       y = "Execution time (seconds)") +
  theme(legend.position = "none")

ggsave("311b_duration_sec_boxplot_by_nodes.pdf", plot = g1, width = 8, height = 6)
ggsave("311b_duration_sec_boxplot_by_nodes.png", plot = g1, width = 8, height = 6)

g2 <- ggplot(df_scoring, aes(x = scale_factor, y = log10_duration, fill = model)) +
  geom_boxplot(alpha = 0.6) +
  facet_grid(no_of_nodes ~ model, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distributia log10(timp executie)",
       x = "Scale Factor",
       y = "log10(Execution Time ms)") +
  theme(legend.position = "none")

ggsave("313b_log10_duration_boxplot_by_nodes.pdf", plot = g2, width = 8, height = 6)
ggsave("313b_log10_duration_boxplot_by_nodes.png", plot = g2, width = 8, height = 6)

g3 <- ggplot(df_scoring, aes(x = execution_time_ms, fill = model)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_grid(no_of_nodes ~ model + scale_factor, scales = "free_y") +
  theme_minimal() +
  labs(title = "Histograma timp executie (ms)",
       x = "Execution time (ms)",
       y = "Frequency") +
  theme(legend.position = "none")

ggsave("314b_histogram_duration_ms_by_nodes.pdf", plot = g3, width = 10, height = 6)
ggsave("314b_histogram_duration_ms_by_nodes.png", plot = g3, width = 10, height = 6)


#########################################################################
### IIa.RQ5:Is the query duration associated with the system?
#########################################################################
glimpse(df_scoring)
shapiro.test(df_scoring$duration_sec)

# Grupăm rulările pentru care avem rezultate în ambele sisteme
df_scoring_paired <- main_df %>%
  rename_with(janitor::make_clean_names) %>%
  mutate(
    execution_time_ms = as.numeric(execution_time_ms),
    duration_sec = execution_time_ms / 1000
  ) %>%
  filter(!is.na(execution_time_ms), execution_time_ms <= 1200000) %>%
  group_by(query_id, scale_factor, no_of_nodes, model) %>%
  summarise(duration_sec = mean(duration_sec), .groups = "drop") %>%
  group_by(query_id, scale_factor, no_of_nodes) %>%
  filter(n_distinct(model) == 2) %>%
  ungroup() %>%
  pivot_wider(
    names_from = model,
    values_from = duration_sec
  )


glimpse(df_scoring_paired)
nrow(df_scoring_paired)

names(df_scoring_paired)[names(df_scoring_paired) == "0 - postgresql"] <- "postgresql"
names(df_scoring_paired)[names(df_scoring_paired) == "1 - mongodb"] <- "mongodb"

# Teste de normalitate
shapiro_pg <- shapiro.test(df_scoring_paired$postgresql)
shapiro_mongo <- shapiro.test(df_scoring_paired$mongodb)

# T-test (pentru caz normal)
t_test <- t.test(df_scoring_paired$postgresql,
                 df_scoring_paired$mongodb,
                 paired = TRUE)

# Wilcoxon test (non-parametric)
wilcox <- wilcox.test(df_scoring_paired$postgresql,
                      df_scoring_paired$mongodb,
                      paired = TRUE,
                      conf.int = TRUE)

shapiro_pg
shapiro_mongo
t_test
wilcox

-------------------------------------------------------------------

  df_scoring_paired2_3n <- main_df_clean %>%
  rename_with(janitor::make_clean_names) %>%
  filter(no_of_nodes == 3) %>%
  mutate(
    execution_time_ms = as.numeric(execution_time_ms),
    log10_duration = log10(execution_time_ms)
  ) %>%
  filter(!is.na(execution_time_ms), execution_time_ms <= 1200000) %>%
  group_by(query_id, model, scale_factor) %>%
  summarise(
    execution_time_ms = mean(execution_time_ms),
    log10_duration = mean(log10_duration),
    .groups = "drop"
  ) %>%
  group_by(query_id, scale_factor) %>%
  filter(n_distinct(model) == 2) %>%
  ungroup()

df_scoring_paired2_6n <- main_df_clean %>%
  rename_with(janitor::make_clean_names) %>%
  filter(no_of_nodes == 6) %>%
  mutate(
    execution_time_ms = as.numeric(execution_time_ms),
    log10_duration = log10(execution_time_ms)
  ) %>%
  filter(!is.na(execution_time_ms), execution_time_ms <= 1200000) %>%
  group_by(query_id, model, scale_factor) %>%
  summarise(
    execution_time_ms = mean(execution_time_ms),
    log10_duration = mean(log10_duration),
    .groups = "drop"
  ) %>%
  group_by(query_id, scale_factor) %>%
  filter(n_distinct(model) == 2) %>%
  ungroup()

# Separăm valorile pe model
df_pg_3n <- df_scoring_paired2_3n %>%
  filter(model == "0 - postgresql") %>%
  arrange(query_id, scale_factor)

df_mongo_3n <- df_scoring_paired2_3n %>%
  filter(model == "1 - mongodb") %>%
  arrange(query_id, scale_factor)

# Teste statistice
shapiro_pg_3n <- shapiro.test(df_pg_3n$log10_duration)
shapiro_mongo_3n <- shapiro.test(df_mongo_3n$log10_duration)

t_test_3n <- t.test(df_pg_3n$log10_duration, df_mongo_3n$log10_duration, paired = TRUE)
wilcox_3n <- wilcox.test(df_pg_3n$log10_duration, df_mongo_3n$log10_duration, paired = TRUE, conf.int = TRUE)

shapiro_pg_3n
shapiro_mongo_3n
t_test_3n
wilcox_3n

df_pg_6n <- df_scoring_paired2_6n %>% filter(model == "0 - postgresql") %>% arrange(query_id, scale_factor)
df_mongo_6n <- df_scoring_paired2_6n %>% filter(model == "1 - mongodb") %>% arrange(query_id, scale_factor)

stopifnot(identical(df_pg_6n$query_id, df_mongo_6n$query_id))
stopifnot(identical(df_pg_6n$scale_factor, df_mongo_6n$scale_factor))

shapiro_pg_6n <- shapiro.test(df_pg_6n$log10_duration)
shapiro_mongo_6n <- shapiro.test(df_mongo_6n$log10_duration)

t_test_6n <- t.test(df_pg_6n$log10_duration, df_mongo_6n$log10_duration, paired = TRUE)
wilcox_6n <- wilcox.test(df_pg_6n$log10_duration, df_mongo_6n$log10_duration, paired = TRUE, conf.int = TRUE)

# Afișare
shapiro_pg_6n
shapiro_mongo_6n
t_test_6n
wilcox_6n


library(ggstatsplot)

# Pregătim datele în format lung pentru ggwithinstats
df_plot_3n <- bind_rows(
  df_pg_3n %>% mutate(model = "PostgreSQL"),
  df_mongo_3n %>% mutate(model = "MongoDB")
)

df_plot_3n$model <- factor(df_plot_3n$model, levels = c("PostgreSQL", "MongoDB"))

# Adăugăm un identificator comun pentru perechi (query_id + scale_factor)
df_plot_3n$pair_id <- paste0(df_plot_3n$query_id, "_", df_plot_3n$scale_factor)

# Vizualizare cu ggwithinstats pe log10_duration
g1 <- ggwithinstats(
  data = df_plot_3n,
  x = model,
  y = log10_duration,
  type = "np",  # non-parametric (Wilcoxon)
  pair_id = pair_id,
  pairwise.comparisons = TRUE
) +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_viridis_d()

# Salvare
ggsave("02_association_log10_duration_3n.pdf", plot = g1, width = 8, height = 5)


df_plot_6n <- bind_rows(
  df_pg_6n %>% mutate(model = "PostgreSQL"),
  df_mongo_6n %>% mutate(model = "MongoDB")
) %>%
  mutate(model = factor(model, levels = c("PostgreSQL", "MongoDB")),
         pair_id = paste0(query_id, "_", scale_factor))

g2 <- ggwithinstats(
  data = df_plot_6n,
  x = model,
  y = log10_duration,
  type = "np",
  pair_id = pair_id,
  pairwise.comparisons = TRUE
) +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_viridis_d()

ggsave("02_association_log10_duration_6n.pdf", plot = g2, width = 8, height = 5)



#########################################################################
### IIb. RQ6: Is the query duration associated with the number of
###                            inner joins? (PostgreSQL)
#########################################################################
shapiro.test(df_pg$join_uri)
shapiro.test(df_pg$elapsed)

g1 <- ggscatterstats(
  data = df_pg, 
  x = join_uri, 
  y = elapsed,
  type = 'np'
)
g1

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)
ggsave("03 Association duration vs f__inner_j.pdf", plot = x,  device = "pdf")


g2 <- ggscatterstats(
  data = df_pg,
  x = clauze_where,
  y = elapsed,
  type = 'np'
) +
  labs(title = "Correlation: clauze_where vs elapsed")

ggsave("03a_clauze_where_vs_elapsed.pdf", plot = g2, width = 7, height = 5)
ggsave("03a_clauze_where_vs_elapsed.png", plot = g2, width = 7, height = 5)

g3 <- ggscatterstats(
  data = df_pg,
  x = clauze_group_by,
  y = elapsed,
  type = 'np'
) +
  labs(title = "Correlation: clauze_group_by vs elapsed")

ggsave("03a_clauze_group_by_vs_elapsed.pdf", plot = g3, width = 7, height = 5)
ggsave("03a_clauze_group_by_vs_elapsed.png", plot = g3, width = 7, height = 5)

g4 <- ggscatterstats(
  data = df_pg,
  x = clauze_having,
  y = elapsed,
  type = 'np'
) +
  labs(title = "Correlation: clauze_having vs elapsed")

ggsave("03a_clauze_having_vs_elapsed.pdf", plot = g4, width = 7, height = 5)
ggsave("03a_clauze_having_vs_elapsed.png", plot = g4, width = 7, height = 5)

g5 <- ggscatterstats(
  data = df_pg,
  x = subinterogari,
  y = elapsed,
  type = 'np'
) +
  labs(title = "Correlation: subinterogari vs elapsed")

ggsave("03a_subinterogari_vs_elapsed.pdf", plot = g5, width = 7, height = 5)
ggsave("03a_subinterogari_vs_elapsed.png", plot = g5, width = 7, height = 5)

g6 <- ggscatterstats(
  data = df_pg,
  x = distinct,
  y = elapsed,
  type = 'np'
) +
  labs(title = "Correlation: distinct vs elapsed")

ggsave("03a_distinct_vs_elapsed.pdf", plot = g6, width = 7, height = 5)
ggsave("03a_distinct_vs_elapsed.png", plot = g6, width = 7, height = 5)
----------------------------------------------------------------------- 
  
  g1 <- ggscatterstats(
    data = df_mongo,
    x = subconsultari_lookup_pipeline,
    y = execution_time_ms,
    type = "np"
  ) +
  labs(title = "MongoDB: subconsultari_lookup_pipeline vs execution_time_ms")

ggsave("04a_mongo_subconsultari_lookup_pipeline.pdf", g1, width = 7, height = 5)
ggsave("04a_mongo_subconsultari_lookup_pipeline.png", g1, width = 7, height = 5)

g2 <- ggscatterstats(
  data = df_mongo,
  x = grupari_group,
  y = execution_time_ms,
  type = "np"
) +
  labs(title = "MongoDB: grupari_group vs execution_time_ms")

ggsave("04b_mongo_grupari_group.pdf", g2, width = 7, height = 5)
ggsave("04b_mongo_grupari_group.png", g2, width = 7, height = 5)

g3 <- ggscatterstats(
  data = df_mongo,
  x = filtre_match,
  y = execution_time_ms,
  type = "np"
) +
  labs(title = "MongoDB: filtre_match vs execution_time_ms")

ggsave("04c_mongo_filtre_match.pdf", g3, width = 7, height = 5)
ggsave("04c_mongo_filtre_match.png", g3, width = 7, height = 5)

g4 <- ggscatterstats(
  data = df_mongo,
  x = nr_join_uri_lookup,
  y = execution_time_ms,
  type = "np"
) +
  labs(title = "MongoDB: nr_join_uri_lookup vs execution_time_ms")

ggsave("04d_mongo_nr_join_uri_lookup.pdf", g4, width = 7, height = 5)
ggsave("04d_mongo_nr_join_uri_lookup.png", g4, width = 7, height = 5)

g5 <- ggscatterstats(
  data = df_mongo,
  x = filtre_in_clauza_where,
  y = execution_time_ms,
  type = "np"
) +
  labs(title = "MongoDB: filtre_in_clauza_where vs execution_time_ms")

ggsave("04e_mongo_filtre_in_clauza_where.pdf", g5, width = 7, height = 5)
ggsave("04e_mongo_filtre_in_clauza_where.png", g5, width = 7, height = 5)

g6 <- ggscatterstats(
  data = df_mongo,
  x = nr_subconsultari_aggregate,
  y = execution_time_ms,
  type = "np"
) +
  labs(title = "MongoDB: nr_subconsultari_aggregate vs execution_time_ms")

ggsave("04f_mongo_nr_subconsultari_aggregate.pdf", g6, width = 7, height = 5)
ggsave("04f_mongo_nr_subconsultari_aggregate.png", g6, width = 7, height = 5)


#########################################################################
###  IIc. RQ7 Does the the association between the duration and       ###
###            the system vary among scale factors?        ###
#########################################################################


g1 <- ggplot(
  df_scoring %>% 
    mutate(
      scale_factor = factor(scale_factor),
      no_of_nodes = factor(no_of_nodes),
      model = factor(model, labels = c("PostgreSQL", "MongoDB"))
    ), 
  aes(x = model, y = log10_duration, fill = model)
) +
  geom_boxplot(alpha = 0.6) +
  facet_grid(no_of_nodes ~ scale_factor, labeller = label_both, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Distributia log10(durata) pe sistem, in functie de scale factor si numar noduri",
    x = "System",
    y = "log10(Duration in ms)"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

ggsave("03b_log10_duration_by_sf_and_nodes.pdf", plot = g1, width = 10, height = 6)
ggsave("03b_log10_duration_by_sf_and_nodes.png", plot = g1, width = 10, height = 6)


g1 <- ggplot(
  df_scoring %>% 
    mutate(
      scale_factor = factor(scale_factor),
      no_of_nodes = factor(no_of_nodes)
    ), 
  aes(x = scale_factor, y = log10_duration, fill = scale_factor)
) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ no_of_nodes, labeller = label_both, scales = "free_y") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5)) +
  labs(
    title = "Distributia log10(durata) in functie de Scale Factor și Numar Noduri",
    x = "Scale Factor",
    y = "log10(Duration ms)"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 11)
  )

x <- g1 + plot_layout(nrow = 1, byrow = FALSE)

ggsave("04b_log10_duration_by_sf_and_nodes.pdf", plot = x, device = "pdf", width = 8, height = 5)
ggsave("04b_log10_duration_by_sf_and_nodes.png", plot = x, device = "png", width = 8, height = 5)

-----------------------------------------------------------------

### duration_sec
library(ggstatsplot)
library(patchwork)

# 1. Pentru 3 noduri
df_3nodes <- df_scoring %>% filter(no_of_nodes == 3)

g1_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 0.01),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 0.01, Nodes: 3"
)

g2_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 0.1),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 0.1, Nodes: 3"
)

g3_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 1),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 1, Nodes: 3"
)

g4_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 10),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 10, Nodes: 3"
)

x_3 <- g1_3 + plot_spacer() + g2_3 + plot_spacer() + g3_3 + plot_spacer() + g4_3 +
  plot_layout(nrow = 2)

ggsave("05_duration_sec_vs_model_sf_nodes3.pdf", plot = x_3, width = 25, height = 20, units = "cm")
ggsave("05_duration_sec_vs_model_sf_nodes3.png", plot = x_3, width = 25, height = 20, units = "cm")


# 2. Pentru 6 noduri
df_6nodes <- df_scoring %>% filter(no_of_nodes == 6)

g1_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 0.01),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 0.01, Nodes: 6"
)

g2_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 0.1),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 0.1, Nodes: 6"
)

g3_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 1),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 1, Nodes: 6"
)

g4_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 10),
  x = model,
  y = duration_sec,
  type = "np",
  title = "SF: 10, Nodes: 6"
)

x_6 <- g1_6 + plot_spacer() + g2_6 + plot_spacer() + g3_6 + plot_spacer() + g4_6 +
  plot_layout(nrow = 2)

ggsave("05_duration_sec_vs_model_sf_nodes6.pdf", plot = x_6, width = 25, height = 20, units = "cm")
ggsave("05_duration_sec_vs_model_sf_nodes6.png", plot = x_6, width = 25, height = 20, units = "cm")

------------------------------------------------------------------------

### log10_duration
# 2. Pentru 6 noduri
  df_3nodes <- df_scoring %>% filter(no_of_nodes == 3)

g1_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 0.01),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 0.01, Nodes: 3"
)

g2_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 0.1),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 0.1, Nodes: 3"
)

g3_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 1),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 1, Nodes: 3"
)

g4_3 <- ggbetweenstats(
  data = df_3nodes %>% filter(scale_factor == 10),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 10, Nodes: 3"
)

x_3 <- g1_3 + plot_spacer() + g2_3 + plot_spacer() + g3_3 + plot_spacer() + g4_3 +
  plot_layout(nrow = 2)

ggsave("06_log10_duration_vs_model_sf_nodes3.pdf", plot = x_3, width = 25, height = 20, units = "cm")
ggsave("06_log10_duration_vs_model_sf_nodes3.png", plot = x_3, width = 25, height = 20, units = "cm")


df_6nodes <- df_scoring %>% filter(no_of_nodes == 6)

g1_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 0.01),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 0.01, Nodes: 6"
)

g2_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 0.1),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 0.1, Nodes: 6"
)

g3_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 1),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 1, Nodes: 6"
)

g4_6 <- ggbetweenstats(
  data = df_6nodes %>% filter(scale_factor == 10),
  x = model,
  y = log10_duration,
  type = "np",
  title = "SF: 10, Nodes: 6"
)

x_6 <- g1_6 + plot_spacer() + g2_6 + plot_spacer() + g3_6 + plot_spacer() + g4_6 +
  plot_layout(nrow = 2)

ggsave("06_log10_duration_vs_model_sf_nodes6.pdf", plot = x_6, width = 25, height = 20, units = "cm")
ggsave("06_log10_duration_vs_model_sf_nodes6.png", plot = x_6, width = 25, height = 20, units = "cm")

