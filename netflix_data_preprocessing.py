"""
NETFLIX CONTENT STRATEGY - DATA ACQUISITION PIPELINE
====================================================
This script consolidates the multi-step process used to build the 
'Netflix_full_data.csv' dataset.

It integrates data from:
1. Wikipedia (Base Catalog of Netflix Originals)
2. TMDb API (Metadata, Cast, Runtime, Plot)
3. IMDb Datasets (Ratings, Votes, Keywords)
4. Google Trends (Search Volume/Buzz)
5. Netflix Global Top 10 (Success Labels)

Author: [Your Name]
Course: Data Mining
"""

import pandas as pd
import requests
import time
import json
import re
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta
from urllib.parse import quote, unquote, urlparse

# --- CONFIGURATION ---
DATA_DIR = Path("data")
RAW_DIR = DATA_DIR / "raw"
INTERIM_DIR = DATA_DIR / "interim"
PROCESSED_DIR = DATA_DIR / "processed"

# Create directories if they don't exist
for d in [DATA_DIR, RAW_DIR, INTERIM_DIR, PROCESSED_DIR]:
    d.mkdir(parents=True, exist_ok=True)

# API KEYS (Placeholders)
TMDB_API_KEY = "YOUR_TMDB_KEY"
OMDB_API_KEY = "YOUR_OMDB_KEY"

# ==============================================================================
# PHASE 1: CATALOG CREATION (WIKIPEDIA)
# ==============================================================================
def build_wiki_catalog():
    """
    Scrapes Wikipedia 'List of Netflix original films/series' to build 
    the base universe of titles and premiere dates.
    """
    print("--- Phase 1: Building Catalog from Wikipedia ---")
    
    # (Simplified logic from src/01_build_wiki_catalog_v2.py)
    # In the actual execution, this used BeautifulSoup to parse tables 
    # from cached HTML files of Wikipedia lists (2015-2025).
    
    # Logic summary:
    # 1. Load HTML pages for Series and Films (year by year).
    # 2. Parse tables using pandas.read_html.
    # 3. Normalize columns (Title, Genre, Premiere).
    # 4. Filter for 'Feature Films' and major Series categories (Drama, Comedy).
    # 5. Export to catalog.csv.
    
    print("... Catalog built successfully.")

# ==============================================================================
# PHASE 2: METADATA ENRICHMENT (TMDb)
# ==============================================================================
def enrich_with_tmdb():
    """
    Enhances the catalog with rich metadata using the TMDb API.
    Crucial Step: Enforces Season 1 logic for Series to prevent leakage.
    """
    print("--- Phase 2: Enriching with TMDb Metadata ---")
    
    # (Logic from src/02c_expand_features.py)
    
    # 1. Search TMDb API by Title + Year to get unique tmdb_id.
    # 2. Fetch Details:
    #    - Films: Runtime, Budget, Revenue.
    #    - Series: Episode Count, Average Episode Runtime.
    # 3. Fetch Credits (Cast/Crew):
    #    - Films: Top billed cast popularity.
    #    - Series: STRICTLY Season 1 Cast/Crew to avoid future data leakage.
    # 4. Calculate 'Star Power' metrics (Max Cast Popularity, Mean Cast Popularity).
    
    print("... TMDb enrichment complete (Season 1 logic applied).")

# ==============================================================================
# PHASE 3: KEYWORDS & RATINGS (IMDb)
# ==============================================================================
def enrich_with_imdb():
    """
    Adds content keywords and quality ratings using IMDb datasets.
    """
    print("--- Phase 3: IMDb Keywords and Ratings ---")
    
    # (Logic from src/02g_merge_imdb_keywords.py and src/06c_fill_imdb.py)
    
    # A. KEYWORDS
    # 1. Map TMDb ID -> IMDb ID.
    # 2. Scrape Top 5 'Relevance' Keywords from IMDb title pages.
    #    (Used Apify/Cheerio for robust scraping).
    
    # B. RATINGS & VOTES
    # 1. Download IMDb public datasets (title.ratings.tsv.gz).
    # 2. Films: Map directly to imdb_rating.
    # 3. Series: Aggregate ratings from ALL Season 1 episodes to create 
    #    a 'Season 1 Quality Score' (avoiding series-level average).
    
    print("... IMDb data merged.")

# ==============================================================================
# PHASE 4: BUZZ METRICS (GOOGLE TRENDS & WIKIPEDIA)
# ==============================================================================
def add_buzz_metrics():
    """
    Calculates 'Buzz' using search volume and pageviews.
    """
    print("--- Phase 4: Calculating Buzz Metrics ---")
    
    # (Logic from src/03a_wiki_pageviews.py and src/04_google_trends.py)
    
    # A. WIKIPEDIA VIEWS
    # 1. Identify correct Wikipedia article title.
    # 2. Query Wikimedia API for daily views.
    # 3. Sum views for the first 28 Days post-premiere.
    
    # B. GOOGLE TRENDS
    # 1. Query pytrends for the Title + "Netflix".
    # 2. Extract search interest for the [Premiere, Premiere + 28 Days] window.
    # 3. Create binary flag 'has_trend_signal' due to data sparsity.
    
    print("... Buzz metrics calculated.")

# ==============================================================================
# PHASE 5: SUCCESS LABELS (NETFLIX TOP 10)
# ==============================================================================
def add_success_labels():
    """
    Merges the target variable: Did the title hit the Global Top 10?
    """
    print("--- Phase 5: Merging Success Labels ---")
    
    # (Logic from src/07_build_netflix_top10_labels.py)
    
    # 1. Load weekly Netflix Global Top 10 Excel file.
    # 2. Filter for weeks corresponding to the title's release window.
    # 3. Create Target Variables:
    #    - ever_top10_global (Binary: 1/0)
    #    - weeks_top10_global (Count)
    
    print("... Success labels merged.")

# ==============================================================================
# MAIN EXECUTION FLOW
# ==============================================================================
if __name__ == "__main__":
    print("Starting Netflix Data Pipeline...")
    
    # In a real run, these functions would process and pass DataFrames.
    # This script serves as the architectural documentation of the process.
    
    build_wiki_catalog()
    enrich_with_tmdb()
    enrich_with_imdb()
    add_buzz_metrics()
    add_success_labels()
    
    print("\nPIPELINE COMPLETE.")
    print(f"Final Dataset ready at: {PROCESSED_DIR}/Netflix_full_data.csv")