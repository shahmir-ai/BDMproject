# 1. Load the necessary library
# If you don't have it installed, run: install.packages("tidyverse")
library(tidyverse)

# 2. Load the dataset
# We use read_csv (from tidyverse) because it is smarter at guessing data types than read.csv
df <- read_csv("Netflix_full_data.csv")

# 3. Inspect the data
# This shows us the dimensions (rows/columns) and the data types
glimpse(df)

# 4. Check for duplicate rows (just a sanity check)
sum(duplicated(df))

#__________


# 1. Filter for English Language Only
df_en <- df %>% 
  filter(original_language == "en")

print(paste("Total English Titles:", nrow(df_en)))

# 2. Split into Films and Series
df_films <- df_en %>% 
  filter(type == "film")

df_series <- df_en %>% 
  filter(type == "series")

# 3. Clean up columns (Drop columns that don't apply to each type)
# For Films: Drop episode-related columns and cadence
df_films <- df_films %>% 
  select(-c(episode_count_s1, episode_runtime_avg_s1, release_cadence))

# For Series: Drop runtime_min (since we use episode runtime)
df_series <- df_series %>% 
  select(-runtime_min)

# 4. Check the split
print(paste("Films:", nrow(df_films)))
print(paste("Series:", nrow(df_series)))

#________________



# 1. Define a helper function to process genres
process_genres <- function(data) {
  data %>%
    select(tmdb_id, genres_tmdb) %>%
    # Split the comma-separated string into multiple rows
    separate_rows(genres_tmdb, sep = ",") %>%
    # Remove any leading/trailing whitespace (just in case)
    mutate(genres_tmdb = trimws(genres_tmdb)) %>%
    # Create a '1' for presence
    mutate(value = 1) %>%
    # Pivot wide: Create columns like "Genre_Action", filling missing with 0
    pivot_wider(names_from = genres_tmdb, 
                values_from = value, 
                values_fill = 0,
                names_prefix = "Genre_")
}

# 2. Apply it to Films
genre_dummies_films <- process_genres(df_films)
# Join the new genre columns back to the main film dataset
df_films <- left_join(df_films, genre_dummies_films, by = "tmdb_id")

# 3. Apply it to Series
genre_dummies_series <- process_genres(df_series)
# Join the new genre columns back to the main series dataset
df_series <- left_join(df_series, genre_dummies_series, by = "tmdb_id")

# 4. Check the result (Look at the new columns)
# We'll check the first few column names to verify they start with "Genre_"
print("New Film Columns:")
print(head(names(df_films %>% select(starts_with("Genre_")))))

print("New Series Columns:")
print(head(names(df_series %>% select(starts_with("Genre_")))))

#________________




# 1. Define a helper function for Top-N Keywords
process_keywords <- function(data, top_n = 50) {
  # Step A: Find the top N most frequent keywords
  top_keywords <- data %>%
    select(tmdb_id, keywords) %>%
    separate_rows(keywords, sep = ",") %>%
    mutate(keywords = trimws(keywords)) %>%
    filter(keywords != "") %>%
    count(keywords, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    pull(keywords)
  
  # Step B: Filter the data for ONLY those keywords and Pivot
  data %>%
    select(tmdb_id, keywords) %>%
    separate_rows(keywords, sep = ",") %>%
    mutate(keywords = trimws(keywords)) %>%
    # KEEP only if it's in our top list
    filter(keywords %in% top_keywords) %>%
    mutate(value = 1) %>%
    # Make the column names friendly (e.g., "Keyword_high_school")
    mutate(keywords = paste0("Keyword_", str_replace_all(keywords, " ", "_"))) %>%
    distinct(tmdb_id, keywords, .keep_all = TRUE) %>%
    pivot_wider(names_from = keywords, 
                values_from = value, 
                values_fill = 0)
}

# 2. Apply to Films (Top 50)
keyword_dummies_films <- process_keywords(df_films, top_n = 50)
df_films <- left_join(df_films, keyword_dummies_films, by = "tmdb_id")

# 3. Apply to Series (Top 50)
keyword_dummies_series <- process_keywords(df_series, top_n = 50)
df_series <- left_join(df_series, keyword_dummies_series, by = "tmdb_id")

# 4. Fill NAs with 0 (for titles that had NO top keywords)
# We use 'mutate(across...)' to replace NA with 0 for all new keyword columns
df_films <- df_films %>% 
  mutate(across(starts_with("Keyword_"), ~replace_na(., 0)))

df_series <- df_series %>% 
  mutate(across(starts_with("Keyword_"), ~replace_na(., 0)))

# 5. Check the results
print("Example Film Keywords:")
print(head(names(df_films %>% select(starts_with("Keyword_")))))

print("Example Series Keywords:")
print(head(names(df_series %>% select(starts_with("Keyword_")))))




#________________









# 1. Install caret (if you don't have it)
# install.packages("caret")
library(caret)

# --- 2. Clean up SERIES data ---

# Impute (fill) NAs for release_cadence
# We assume 'NA' (which happened for films, but just in case) means 'binge'
df_series$release_cadence <- replace_na(df_series$release_cadence, "binge")

# Impute NAs for numeric predictors (e.g., crew_popularity_max)
# We fill with 0, assuming NA means "no popularity" or "not applicable"
df_series <- df_series %>%
  mutate(crew_popularity_max = replace_na(crew_popularity_max, 0))

# One-Hot Encode 'release_cadence' (the last categorical column)
# This creates columns like 'release_cadence.binge', 'release_cadence.weekly'
cadence_dummies <- model.matrix(~ release_cadence - 1, data = df_series)
df_series <- bind_cols(df_series, as.data.frame(cadence_dummies))

# --- 3. Clean up FILMS data ---

# Impute NAs for numeric predictors
df_films <- df_films %>%
  mutate(crew_popularity_max = replace_na(crew_popularity_max, 0))


# --- 4. Create the final "Clustering Matrix" for FILMS ---

# A) Identify all predictor columns (X variables)
# We select only the columns we built: scaled numerics, genres, keywords, and flags
film_predictors <- df_films %>%
  select(
    starts_with("Genre_"), 
    starts_with("Keyword_"),
    cast_popularity_max, 
    cast_popularity_mean, 
    crew_popularity_max, 
    runtime_min, 
    is_ip_adaptation,
    has_keywords
  )

# B) Identify just the numeric columns that need scaling
film_numeric_cols <- c(
  "cast_popularity_max", 
  "cast_popularity_mean", 
  "crew_popularity_max", 
  "runtime_min"
)

# C) Apply the scaling
# 'preProcess' creates the "recipe" (center to mean, scale by std dev)
scaler_films <- preProcess(film_predictors, method = c("center", "scale"), 
                           # This line is important!
                           # It tells the scaler to ONLY scale the columns in our list
                           # and to ignore all the 0/1 dummies for genres/keywords.
                           subset = film_numeric_cols)

# 'predict' applies that recipe to our data
films_scaled <- predict(scaler_films, film_predictors)

# --- 5. Create the final "Clustering Matrix" for SERIES ---

# A) Identify all predictor columns
series_predictors <- df_series %>%
  select(
    starts_with("Genre_"),
    starts_with("Keyword_"),
    starts_with("release_cadence."),
    cast_popularity_max,
    cast_popularity_mean,
    crew_popularity_max,
    episode_count_s1,
    episode_runtime_avg_s1,
    is_ip_adaptation,
    has_keywords
  )

# B) Identify numeric columns to scale
series_numeric_cols <- c(
  "cast_popularity_max", 
  "cast_popularity_mean", 
  "crew_popularity_max", 
  "episode_count_s1", 
  "episode_runtime_avg_s1"
)

# C) Apply the scaling
scaler_series <- preProcess(series_predictors, method = c("center", "scale"),
                            subset = series_numeric_cols)

series_scaled <- predict(scaler_series, series_predictors)


# --- 6. Verification ---
print("--- Film Scaled Data Summary (Check for Mean ~ 0) ---")
summary(films_scaled[, film_numeric_cols])

print("--- Series Scaled Data Summary (Check for Mean ~ 0) ---")
summary(series_scaled[, series_numeric_cols])


#________________







# --- Phase 2: Clustering - Step 1: PCA ---

# 1. Run PCA on Films
# prcomp performs the PCA. 
# scale. = FALSE because we already scaled the data manually in the previous step.
pca_films <- prcomp(films_scaled, center = FALSE, scale. = FALSE)

# 2. Run PCA on Series
pca_series <- prcomp(series_scaled, center = FALSE, scale. = FALSE)

# 3. Visualize the Variance (The Scree Plot)
# We want to see how many components we need to keep.

# Calculate variance explained for Films
var_explained_films <- pca_films$sdev^2 / sum(pca_films$sdev^2)
# Create a quick data frame for plotting
scree_data_films <- data.frame(
  PC = 1:length(var_explained_films),
  Variance = var_explained_films,
  Cumulative = cumsum(var_explained_films)
)

# Plot Variance Explained (Films)
# Look for the "Elbow" where the bars flatten out
ggplot(scree_data_films[1:20,], aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = Variance), color = "red", size = 1) +
  geom_point(color = "red") +
  labs(title = "Scree Plot: Films (Top 20 PCs)", 
       subtitle = "Bars = Individual Variance, Red Line = Trend",
       y = "Proportion of Variance Explained") +
  theme_minimal()

# 4. Check Cumulative Variance
# This tells us: "If we keep X components, we explain Y% of the data."
print("--- Cumulative Variance (Films) ---")
print(head(scree_data_films$Cumulative, 20))

# 5. Repeat Check for Series
var_explained_series <- pca_series$sdev^2 / sum(pca_series$sdev^2)
print("--- Cumulative Variance (Series) ---")
print(head(cumsum(var_explained_series), 20))

# --- Create the Scree Plot for SERIES ---

# 1. Create the data frame for plotting
scree_data_series <- data.frame(
  PC = 1:length(var_explained_series),
  Variance = var_explained_series,
  Cumulative = cumsum(var_explained_series)
)

# 2. Plot Variance Explained (Series)
ggplot(scree_data_series[1:20,], aes(x = PC, y = Variance)) +
  geom_col(fill = "darkgreen") + # Different color to distinguish from Films
  geom_line(aes(y = Variance), color = "black", size = 1) +
  geom_point(color = "black") +
  labs(title = "Scree Plot: Series (Top 20 PCs)", 
       subtitle = "Bars = Individual Variance, Line = Trend",
       y = "Proportion of Variance Explained") +
  theme_minimal()


#________________










# --- Phase 2: Clustering - Step 2: Choosing K (Elbow Method) ---

# 1. Prepare the PCA Data
# We select the top components based on our decision
films_pca_data <- pca_films$x[, 1:15]   # Top 15 PCs for Films
series_pca_data <- pca_series$x[, 1:13] # Top 13 PCs for Series

# 2. Define a helper function to calculate WCSS (Error)
calculate_wcss <- function(data, max_k = 15) {
  wcss <- numeric(max_k)
  # Loop from k=1 to max_k
  for (k in 1:max_k) {
    # Run KMeans
    km <- kmeans(data, centers = k, nstart = 25)
    # Store the total within-cluster sum of squares
    wcss[k] <- km$tot.withinss
  }
  return(wcss)
}

# 3. Calculate WCSS for Films
set.seed(123) # For reproducibility
wcss_films <- calculate_wcss(films_pca_data)

# 4. Calculate WCSS for Series
set.seed(123)
wcss_series <- calculate_wcss(series_pca_data)

# 5. Plot the Elbow for Films
elbow_data_films <- data.frame(K = 1:15, WCSS = wcss_films)

ggplot(elbow_data_films, aes(x = K, y = WCSS)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 1:15) +
  labs(title = "Elbow Method: Films", 
       subtitle = "Look for the 'kink' in the line",
       x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

# 6. Plot the Elbow for Series
elbow_data_series <- data.frame(K = 1:15, WCSS = wcss_series)

ggplot(elbow_data_series, aes(x = K, y = WCSS)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_x_continuous(breaks = 1:15) +
  labs(title = "Elbow Method: Series", 
       subtitle = "Look for the 'kink' in the line",
       x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()



#________________







# --- Phase 2: Clustering - Step 3: Silhouette Analysis ---

# 1. Load the 'cluster' library for silhouette function
library(cluster)

# 2. Define a helper function for Silhouette Score
calculate_avg_silhouette <- function(data, max_k = 10) {
  sil_width <- numeric(max_k)
  # Start from k=2 (Silhouette needs at least 2 clusters)
  for (k in 2:max_k) {
    km <- kmeans(data, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist(data))
    sil_width[k] <- mean(ss[, 3]) # Column 3 holds the width values
  }
  return(sil_width)
}

# 3. Calculate Silhouette for Films
# We limit to k=10 because >10 clusters is usually too hard to explain anyway
sil_films <- calculate_avg_silhouette(films_pca_data, max_k = 10)

# 4. Calculate Silhouette for Series
sil_series <- calculate_avg_silhouette(series_pca_data, max_k = 10)

# 5. Plot Silhouette for Films
sil_data_films <- data.frame(K = 2:10, Silhouette = sil_films[2:10])

ggplot(sil_data_films, aes(x = K, y = Silhouette)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Silhouette Score: Films", 
       subtitle = "Higher is Better",
       x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  theme_minimal()

# 6. Plot Silhouette for Series
sil_data_series <- data.frame(K = 2:10, Silhouette = sil_series[2:10])

ggplot(sil_data_series, aes(x = K, y = Silhouette)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Silhouette Score: Series", 
       subtitle = "Higher is Better",
       x = "Number of Clusters (k)", y = "Average Silhouette Width") +
  theme_minimal()

#________________








# --- Phase 2: Clustering - Step 4: Final Clustering & Assignment ---

# 1. Run Final K-Means for Films (k=9)
set.seed(123)
final_km_films <- kmeans(films_pca_data, centers = 9, nstart = 25)

# 2. Run Final K-Means for Series (k=5)
set.seed(123)
final_km_series <- kmeans(series_pca_data, centers = 5, nstart = 25)

# 3. Assign Cluster IDs back to the original dataframes
# We convert to 'factor' because Cluster ID is a category, not a number (Cluster 1 < Cluster 2 is meaningless)
df_films$cluster <- as.factor(final_km_films$cluster)
df_series$cluster <- as.factor(final_km_series$cluster)

# 4. Check the counts (How many movies in each cluster?)
print("--- Film Cluster Sizes ---")
print(table(df_films$cluster))

print("--- Series Cluster Sizes ---")
print(table(df_series$cluster))



#________________
































# --- Phase 3: Profiling - Step 1: Generate Cluster Reports (Fixed) ---

get_cluster_profile <- function(data) {
  
  # 1. Calculate Basic Numeric Averages (Who/How?)
  stats <- data %>%
    group_by(cluster) %>%
    summarise(
      Count = n(),
      across(any_of(c("runtime_min", "episode_count_s1", "cast_popularity_max", "imdb_rating_used")), 
             \(x) round(mean(x, na.rm = TRUE), 1))
    )
  
  # 2. Find the Top Genre (What?)
  # We calculate the average score for every genre column, then pick the winner
  genre_stats <- data %>%
    group_by(cluster) %>%
    # Calculate mean for all genre columns
    summarise(across(starts_with("Genre_"), mean)) %>%
    # Flip the table to make it easy to find the max
    pivot_longer(cols = starts_with("Genre_"), names_to = "Genre", values_to = "Pct") %>%
    group_by(cluster) %>%
    # Pick the #1 genre for each cluster
    slice_max(Pct, n = 1, with_ties = FALSE) %>%
    mutate(
      Top_Genre = str_remove(Genre, "Genre_"),
      Genre_Pct = round(Pct * 100, 0)
    ) %>%
    select(cluster, Top_Genre, Genre_Pct)
  
  # 3. Find the Top Keyword (About What?)
  keyword_stats <- data %>%
    group_by(cluster) %>%
    summarise(across(starts_with("Keyword_"), mean)) %>%
    pivot_longer(cols = starts_with("Keyword_"), names_to = "Keyword", values_to = "Pct") %>%
    group_by(cluster) %>%
    slice_max(Pct, n = 1, with_ties = FALSE) %>%
    mutate(
      Top_Keyword = str_remove(Keyword, "Keyword_"),
      Keyword_Pct = round(Pct * 100, 0)
    ) %>%
    select(cluster, Top_Keyword, Keyword_Pct)
  
  # 4. Combine everything into one clean table
  final_profile <- stats %>%
    left_join(genre_stats, by = "cluster") %>%
    left_join(keyword_stats, by = "cluster")
  
  return(final_profile)
}

# 1. Generate Profile for Films
print("--- Film Cluster Profiles ---")
film_profile <- get_cluster_profile(df_films)
print(film_profile)

# 2. Generate Profile for Series
print("--- Series Cluster Profiles ---")
series_profile <- get_cluster_profile(df_series)
print(series_profile)



#________________






























# --- Phase 4: Prediction (Random Forest) - FIXED ---

library(randomForest)
library(tidyverse) # Ensure this is loaded for piping (%>%)

# 1. Sanitize Column Names (The Fix)
# This replaces "Science Fiction" with "Science.Fiction" so R doesn't crash
names(df_films) <- make.names(names(df_films))
names(df_series) <- make.names(names(df_series))

# 2. Prepare the Data for Modelling (FILMS)
# Note: We use 'starts_with' which will automatically pick up the new names
rf_data_films <- df_films %>%
  select(
    ever_top10_global,       
    starts_with("Genre_"),   
    starts_with("Keyword_"), 
    cast_popularity_max,     
    runtime_min,             
    is_ip_adaptation         
  ) %>%
  # Ensure the target is a Factor (0/1 category)
  mutate(ever_top10_global = as.factor(ever_top10_global)) %>%
  na.omit()

# 3. Train the Random Forest Model
set.seed(123)
rf_model_films <- randomForest(ever_top10_global ~ ., data = rf_data_films, importance = TRUE, ntree = 500)

# 4. View Model Accuracy
print("--- Film Model Accuracy ---")
print(rf_model_films)

# 5. Extract & Plot Feature Importance
importance_df <- as.data.frame(importance(rf_model_films))
importance_df$Feature <- rownames(importance_df)

# Sort by "MeanDecreaseGini" (Importance)
top_features <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(15)

# Plot the Top 15 Drivers
ggplot(top_features, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Drivers of Success (Films)",
       subtitle = "Predicting 'Ever Top 10 Global'",
       x = "Feature", y = "Importance (Mean Decrease Gini)") +
  theme_minimal()


#________________


# Compare Runtimes for Hits (1) vs. Misses (0)
df_films %>%
  group_by(ever_top10_global) %>%
  summarise(
    count = n(),
    min_runtime = min(runtime_min, na.rm = TRUE),
    avg_runtime = mean(runtime_min, na.rm = TRUE),
    max_runtime = max(runtime_min, na.rm = TRUE)
  )



#________________



# --- Phase 4: Prediction (Model 2 - The "Buzz" Meter) ---

# 1. Prepare Data for Wiki Views Prediction (FILMS)
# Target: wiki_views_28d_log (Continuous)
rf_data_wiki <- df_films %>%
  select(
    wiki_views_28d_log,      # The Target (Log of Pageviews)
    starts_with("Genre_"),
    starts_with("Keyword_"),
    cast_popularity_max,
    runtime_min,
    is_ip_adaptation,
    original_language        # Language matters for Wiki views!
  ) %>%
  # Remove rows where we don't have wiki data (using the log column handles the NAs)
  filter(!is.na(wiki_views_28d_log)) %>%
  na.omit()

# 2. Train Random Forest Regressor
set.seed(123)
# ntree=500 is standard. importance=TRUE lets us see the drivers.
rf_model_wiki <- randomForest(wiki_views_28d_log ~ ., data = rf_data_wiki, importance = TRUE, ntree = 500)

# 3. Model Performance (R-Squared)
# In Regression, "Variance Explained" is our Accuracy score.
print("--- Wiki Views Model Performance ---")
print(rf_model_wiki)

# 4. Plot Feature Importance (What drives Buzz?)
importance_wiki <- as.data.frame(importance(rf_model_wiki))
importance_wiki$Feature <- rownames(importance_wiki)

top_drivers_wiki <- importance_wiki %>%
  arrange(desc(`%IncMSE`)) %>% # %IncMSE is the standard metric for Regression RF
  head(15)

ggplot(top_drivers_wiki, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "purple") + # Purple for "Buzz"
  coord_flip() +
  labs(title = "What Drives 'Buzz'? (Wiki Views)",
       subtitle = "Top Predictors of Search Volume",
       x = "Feature", y = "Importance (% Increase in MSE)") +
  theme_minimal()


#________________























# --- Phase 4: Prediction (Model 3 - The "Engagement" Engine) ---

# 1. Prepare Data for Engagement Prediction (FILMS)
# Target: tmdb_vote_count (Continuous)
# Note: Vote counts are also exponential, so we will predict the LOG of votes
# to stop massive blockbusters from skewing the whole model.

rf_data_eng <- df_films %>%
  select(
    tmdb_vote_count,         # The Target (Raw)
    starts_with("Genre_"),
    starts_with("Keyword_"),
    cast_popularity_max,
    runtime_min,
    is_ip_adaptation,
    original_language
  ) %>%
  # Create Log Vote Count (Log1p adds 1 to handle 0s safely)
  mutate(log_vote_count = log1p(tmdb_vote_count)) %>%
  select(-tmdb_vote_count) %>% # Drop the raw target
  na.omit()

# 2. Train Random Forest Regressor
set.seed(123)
rf_model_eng <- randomForest(log_vote_count ~ ., data = rf_data_eng, importance = TRUE, ntree = 500)

# 3. Model Performance
print("--- Engagement Model Performance ---")
print(rf_model_eng)

# 4. Plot Feature Importance
importance_eng <- as.data.frame(importance(rf_model_eng))
importance_eng$Feature <- rownames(importance_eng)

top_drivers_eng <- importance_eng %>%
  arrange(desc(`%IncMSE`)) %>%
  head(15)

ggplot(top_drivers_eng, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "orange") + # Orange for "Engagement"
  coord_flip() +
  labs(title = "What Drives 'Engagement'? (Vote Counts)",
       subtitle = "Top Predictors of Audience Passion",
       x = "Feature", y = "Importance (% Increase in MSE)") +
  theme_minimal()
#________________













# --- Phase 4: Prediction (Model 3 - The "Engagement" Engine) ---

# 1. Prepare Data for Engagement Prediction (FILMS)
# Target: tmdb_vote_count (Continuous)
# Note: Vote counts are also exponential, so we will predict the LOG of votes
# to stop massive blockbusters from skewing the whole model.

rf_data_eng <- df_films %>%
  select(
    tmdb_vote_count,         # The Target (Raw)
    starts_with("Genre_"),
    starts_with("Keyword_"),
    cast_popularity_max,
    runtime_min,
    is_ip_adaptation,
    original_language
  ) %>%
  # Create Log Vote Count (Log1p adds 1 to handle 0s safely)
  mutate(log_vote_count = log1p(tmdb_vote_count)) %>%
  select(-tmdb_vote_count) %>% # Drop the raw target
  na.omit()

# 2. Train Random Forest Regressor
set.seed(123)
rf_model_eng <- randomForest(log_vote_count ~ ., data = rf_data_eng, importance = TRUE, ntree = 500)

# 3. Model Performance
print("--- Engagement Model Performance ---")
print(rf_model_eng)

# 4. Plot Feature Importance
importance_eng <- as.data.frame(importance(rf_model_eng))
importance_eng$Feature <- rownames(importance_eng)

top_drivers_eng <- importance_eng %>%
  arrange(desc(`%IncMSE`)) %>%
  head(15)

ggplot(top_drivers_eng, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "orange") + # Orange for "Engagement"
  coord_flip() +
  labs(title = "What Drives 'Engagement'? (Vote Counts)",
       subtitle = "Top Predictors of Audience Passion",
       x = "Feature", y = "Importance (% Increase in MSE)") +
  theme_minimal()

#________________















# --- Phase 4: Prediction (Model 4 - The "Critics' Choice") ---

# 1. Prepare Data for Rating Prediction (FILMS)
# Target: imdb_rating_used (Continuous, 1-10)
rf_data_qual <- df_films %>%
  select(
    imdb_rating_used,        # The Target (Quality)
    starts_with("Genre_"),
    starts_with("Keyword_"),
    cast_popularity_max,
    runtime_min,
    is_ip_adaptation,
    original_language
  ) %>%
  # Filter out rows where we don't have a rating
  filter(!is.na(imdb_rating_used)) %>%
  na.omit()

# 2. Train Random Forest Regressor
set.seed(123)
rf_model_qual <- randomForest(imdb_rating_used ~ ., data = rf_data_qual, importance = TRUE, ntree = 500)

# 3. Model Performance
print("--- Quality Model Performance ---")
print(rf_model_qual)

# 4. Plot Feature Importance
importance_qual <- as.data.frame(importance(rf_model_qual))
importance_qual$Feature <- rownames(importance_qual)

top_drivers_qual <- importance_qual %>%
  arrange(desc(`%IncMSE`)) %>%
  head(15)

ggplot(top_drivers_qual, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "darkred") + # Red for "Critics/Quality"
  coord_flip() +
  labs(title = "What Drives 'Quality'? (IMDb Rating)",
       subtitle = "Top Predictors of Critical Acclaim",
       x = "Feature", y = "Importance (% Increase in MSE)") +
  theme_minimal()
#________________






# --- Phase 5: The Grand Finale (4-Way Driver Matrix) ---

# 1. Get the Quality Drivers
drivers_qual <- importance_qual %>%
  select(Feature, Importance_Qual = `%IncMSE`) %>%
  arrange(desc(Importance_Qual)) %>%
  mutate(Rank_Qual = row_number())

# 2. Join ALL 4 Models
# (Assumes you still have drivers_hit, drivers_buzz, drivers_eng in memory)
driver_matrix_4way <- drivers_hit %>%
  full_join(drivers_buzz, by = "Feature") %>%
  full_join(drivers_eng, by = "Feature") %>%
  full_join(drivers_qual, by = "Feature") %>%
  select(Feature, Rank_Hit, Rank_Buzz, Rank_Eng, Rank_Qual) %>%
  arrange(Rank_Hit)

# 3. Filter to Top Features
top_drivers_4way <- driver_matrix_4way %>%
  filter(Rank_Hit <= 15 | Rank_Buzz <= 15 | Rank_Eng <= 15 | Rank_Qual <= 15)

# 4. Print
print("--- The 4-Way Driver Matrix ---")
print(as.data.frame(top_drivers_4way))

# 5. Save
write_csv(top_drivers_4way, "Driver_Matrix_Results_4Way.csv")




#________________






# --- Phase 4.5: Cluster Success (4-Way) ---

cluster_success_4way <- df_films %>%
  group_by(cluster) %>%
  summarise(
    Count = n(),
    Hit_Rate = round(mean(as.numeric(as.character(ever_top10_global))) * 100, 1),
    Avg_Buzz = round(mean(wiki_views_28d_log, na.rm = TRUE), 1),
    Avg_Passion = round(mean(log(tmdb_vote_count + 1), na.rm = TRUE), 1),
    # NEW: Average Quality
    Avg_Quality = round(mean(imdb_rating_used, na.rm = TRUE), 1)
  ) %>%
  left_join(film_profile %>% select(cluster, Top_Genre, Top_Keyword), by = "cluster") %>%
  arrange(desc(Hit_Rate))

print("--- Cluster Success (4 Dimensions) ---")
print(as.data.frame(cluster_success_4way))
#________________


