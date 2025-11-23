# --- Phase 6: The Complete Series Analysis ---

# 1. Model 1: Hit Predictor (Binary)
# Target: ever_top10_global
rf_data_series_hit <- df_series %>%
  select(ever_top10_global, starts_with("Genre_"), starts_with("Keyword_"), 
         cast_popularity_max, episode_count_s1, episode_runtime_avg_s1, is_ip_adaptation,
         starts_with("release_cadence.")) %>%
  mutate(ever_top10_global = as.factor(ever_top10_global)) %>%
  na.omit()

set.seed(123)
rf_model_series_hit <- randomForest(ever_top10_global ~ ., data = rf_data_series_hit, importance = TRUE, ntree = 500)

# 2. Model 2: Buzz Predictor (Wiki Views)
# Target: wiki_views_28d_log
rf_data_series_buzz <- df_series %>%
  select(wiki_views_28d_log, starts_with("Genre_"), starts_with("Keyword_"), 
         cast_popularity_max, episode_count_s1, episode_runtime_avg_s1, is_ip_adaptation,
         starts_with("release_cadence.")) %>%
  filter(!is.na(wiki_views_28d_log)) %>%
  na.omit()

set.seed(123)
rf_model_series_buzz <- randomForest(wiki_views_28d_log ~ ., data = rf_data_series_buzz, importance = TRUE, ntree = 500)

# 3. Model 3: Engagement Predictor (Votes)
# Target: log_vote_count (derived from tmdb_vote_count)
rf_data_series_eng <- df_series %>%
  select(tmdb_vote_count, starts_with("Genre_"), starts_with("Keyword_"), 
         cast_popularity_max, episode_count_s1, episode_runtime_avg_s1, is_ip_adaptation,
         starts_with("release_cadence.")) %>%
  mutate(log_vote_count = log1p(tmdb_vote_count)) %>%
  select(-tmdb_vote_count) %>%
  na.omit()

set.seed(123)
rf_model_series_eng <- randomForest(log_vote_count ~ ., data = rf_data_series_eng, importance = TRUE, ntree = 500)

# 4. Model 4: Quality Predictor (Rating)
# Target: imdb_rating_used
rf_data_series_qual <- df_series %>%
  select(imdb_rating_used, starts_with("Genre_"), starts_with("Keyword_"), 
         cast_popularity_max, episode_count_s1, episode_runtime_avg_s1, is_ip_adaptation,
         starts_with("release_cadence.")) %>%
  filter(!is.na(imdb_rating_used)) %>%
  na.omit()

set.seed(123)
rf_model_series_qual <- randomForest(imdb_rating_used ~ ., data = rf_data_series_qual, importance = TRUE, ntree = 500)

# 5. Build the "Series Driver Matrix"
# We extract importance scores from all 4 models and join them
imp_hit_s <- as.data.frame(importance(rf_model_series_hit)) %>% 
  mutate(Feature = rownames(.), Rank_Hit = rank(desc(MeanDecreaseGini))) %>% select(Feature, Rank_Hit)

imp_buzz_s <- as.data.frame(importance(rf_model_series_buzz)) %>% 
  mutate(Feature = rownames(.), Rank_Buzz = rank(desc(`%IncMSE`))) %>% select(Feature, Rank_Buzz)

imp_eng_s <- as.data.frame(importance(rf_model_series_eng)) %>% 
  mutate(Feature = rownames(.), Rank_Eng = rank(desc(`%IncMSE`))) %>% select(Feature, Rank_Eng)

imp_qual_s <- as.data.frame(importance(rf_model_series_qual)) %>% 
  mutate(Feature = rownames(.), Rank_Qual = rank(desc(`%IncMSE`))) %>% select(Feature, Rank_Qual)

driver_matrix_series <- imp_hit_s %>%
  full_join(imp_buzz_s, by = "Feature") %>%
  full_join(imp_eng_s, by = "Feature") %>%
  full_join(imp_qual_s, by = "Feature") %>%
  # Keep Top 10 features from ANY model
  filter(Rank_Hit <= 10 | Rank_Buzz <= 10 | Rank_Eng <= 10 | Rank_Qual <= 10) %>%
  arrange(Rank_Hit)

# 6. Build the "Series Cluster Success" Table
cluster_success_series <- df_series %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    Hit_Rate = round(mean(as.numeric(as.character(ever_top10_global))) * 100, 1),
    Avg_Buzz = round(mean(wiki_views_28d_log, na.rm = TRUE), 1),
    Avg_Passion = round(mean(log(tmdb_vote_count + 1), na.rm = TRUE), 1),
    Avg_Quality = round(mean(imdb_rating_used, na.rm = TRUE), 1)
  ) %>%
  # Join names so we know what we are looking at
  left_join(series_profile %>% select(cluster, Top_Genre, Top_Keyword), by = "cluster") %>%
  arrange(desc(Hit_Rate))

# 7. Print Results
print("--- Series Driver Matrix ---")
print(as.data.frame(driver_matrix_series))

print("--- Series Cluster Success ---")
print(as.data.frame(cluster_success_series))



#______________________________












# --- Phase 6: Generating Missing Series Charts (FIXED) ---

# 1. Define a robust function to extract and standardize the score name
get_importance_table <- function(model, metric_type) {
  imp_df <- as.data.frame(importance(model))
  imp_df$Feature <- rownames(imp_df)
  
  if (metric_type == "Gini") {
    # For Hit model (Classification)
    imp_df$Importance <- imp_df$MeanDecreaseGini
  } else {
    # For Regression models (Buzz, Eng, Qual)
    imp_df$Importance <- imp_df$`%IncMSE`
  }
  return(imp_df)
}

# 2. Extract and Prepare the Importance DataFrames (Assuming models are in memory)
hit_imp_data <- get_importance_table(rf_model_series_hit, "Gini")
buzz_imp_data <- get_importance_table(rf_model_series_buzz, "MSE")
eng_imp_data <- get_importance_table(rf_model_series_eng, "MSE")
qual_imp_data <- get_importance_table(rf_model_series_qual, "MSE")


# 3. Plot Hit Predictor (ever_top10_global)
top_hit_s <- hit_imp_data %>% arrange(desc(Importance)) %>% head(15)

print("Plotting Series Hit Drivers...")
ggplot(top_hit_s, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Series: Drivers of HITS (Top 10 Global)",
       x = "Feature", y = "Importance (Mean Decrease Gini)") +
  theme_minimal()

# 4. Plot Buzz Predictor (wiki_views_28d_log)
top_buzz_s <- buzz_imp_data %>% arrange(desc(Importance)) %>% head(15)

print("Plotting Series Buzz Drivers...")
ggplot(top_buzz_s, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Series: Drivers of BUZZ (Wiki Views)",
       x = "Feature", y = "Importance (% Inc MSE)") +
  theme_minimal()

# 5. Plot Engagement Predictor (tmdb_vote_count)
top_eng_s <- eng_imp_data %>% arrange(desc(Importance)) %>% head(15)

print("Plotting Series Engagement Drivers...")
ggplot(top_eng_s, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Series: Drivers of ENGAGEMENT (Votes)",
       x = "Feature", y = "Importance (% Inc MSE)") +
  theme_minimal()

# 6. Plot Quality Predictor (imdb_rating_used)
top_qual_s <- qual_imp_data %>% arrange(desc(Importance)) %>% head(15)

print("Plotting Series Quality Drivers...")
ggplot(top_qual_s, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Series: Drivers of QUALITY (IMDb Rating)",
       x = "Feature", y = "Importance (% Inc MSE)") +
  theme_minimal()

