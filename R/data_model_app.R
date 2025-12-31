
data_model_app <- function(){
# ---- Packages: install if missing, then load quietly ----
  required_pkgs <- c("shiny", "DT", "dplyr", "tibble", "stringr")
  
  install_if_missing <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
      install.packages(missing, dependencies = TRUE)
    }
  }
  
  install_if_missing(required_pkgs)
  
  suppressPackageStartupMessages({
    lapply(required_pkgs, function(p) library(p, character.only = TRUE))
  })
  
  # Optional Excel export support
  .has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
  if (!.has_openxlsx) {
    # Try to install openxlsx automatically (comment out if you prefer to keep optional)
    install_if_missing("openxlsx")
    .has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
  }
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
# -----------------------------
# GMS field list (provided)
# -----------------------------
gms_fields <- c(
  "url","platform_name","item_title","item_text","ad_language","search_term","was_recommended",
  "is_case_of_interest","location_known","price_available","seller_contact_known","item_hasEggs",
  "item_multiple_detections","item_image","image_text","website_type","origin_country","item_type",
  "destination_country","item_common_name","item_common_name_website","item_phylum","item_class",
  "item_order","item_family","item_genus","item_species","item_taxa","location_level0","location_level1",
  "location_level2","ad_location","item_sold_in","item_CITES","item_date_posted","record_date","item_price",
  "item_currency","item_count","item_unit","note","is_group","group_type","group_name","payment_method",
  "is_delivery_available","delivery_method","is_seller_vulnerable_group","vulnerable_group","is_bycatch",
  "case_id","datahub"
)

# -----------------------------
# Global Data Model superset (broad master schema)
# Columns: category, field_id, description, standardization_notes
# -----------------------------
gdm_fields <- tribble(
  ~category, ~field_id, ~description, ~standardization_notes,
  
  # 1) Core record and workflow metadata
  "Core record and workflow metadata", "record_id", "Unique record identifier for the monitoring entry", "UUID preferred; immutable; never reuse",
  "Core record and workflow metadata", "record_version", "Version number for updates to the same record", "Integer; increment on edits",
  "Core record and workflow metadata", "parent_record_id", "Links to a parent record (eg consolidated case record)", "UUID; use for rollups and case aggregation",
  "Core record and workflow metadata", "collection_program", "Program or project name", "Controlled list; keep canonical names",
  "Core record and workflow metadata", "collecting_organization", "Organization that collected the record", "Use canonical org registry / code",
  "Core record and workflow metadata", "collecting_unit", "Team/unit name", "Controlled list; avoid free text drift",
  "Core record and workflow metadata", "datahub", "Data hub / monitoring node responsible", "Controlled list",
  "Core record and workflow metadata", "analyst_id", "Collector/analyst identifier", "Pseudonymous internal ID; avoid personal names",
  "Core record and workflow metadata", "analyst_role", "Role (monitor, reviewer, supervisor)", "Controlled list",
  "Core record and workflow metadata", "date_collected_utc", "When record was collected (UTC)", "ISO 8601 UTC datetime (YYYY-MM-DDThh:mm:ssZ)",
  "Core record and workflow metadata", "date_last_updated_utc", "Last update timestamp (UTC)", "ISO 8601 UTC datetime",
  "Core record and workflow metadata", "record_date", "Date the record was created/entered", "ISO 8601 date or datetime",
  "Core record and workflow metadata", "collection_method", "How it was found (manual/alert/scraper/tip)", "Controlled list",
  "Core record and workflow metadata", "monitoring_mode", "Active search vs passive alerts", "Controlled list",
  "Core record and workflow metadata", "alert_source", "If found via alert, which system", "Controlled list",
  "Core record and workflow metadata", "data_source_confidentiality", "Sharing restriction level", "Controlled list (public, partner-only, LE-only, restricted)",
  "Core record and workflow metadata", "record_status", "Lifecycle status", "Controlled list (draft, reviewed, submitted, archived)",
  "Core record and workflow metadata", "review_status", "QA status", "Controlled list (unreviewed, reviewed, rejected, needs_fix)",
  "Core record and workflow metadata", "reviewed_by_analyst_id", "Reviewer identifier", "Pseudonymous internal ID",
  "Core record and workflow metadata", "review_date_utc", "Review timestamp", "ISO 8601 UTC datetime",
  "Core record and workflow metadata", "validation_status", "Overall validation confidence", "Controlled list (unverified, partially_verified, verified)",
  "Core record and workflow metadata", "priority_level", "Operational priority", "Controlled list (low, medium, high, critical)",
  "Core record and workflow metadata", "risk_score", "Numeric risk score", "Define scale (0-10 or 0-100) and document method",
  "Core record and workflow metadata", "risk_score_method", "Risk scoring rubric used", "Versioned rubric name (eg GMS_v1.2)",
  "Core record and workflow metadata", "tags", "Tags for filtering", "Prefer controlled tags + optional free tags",
  "Core record and workflow metadata", "note", "Internal notes", "Keep separate from evidence",
  "Core record and workflow metadata", "legal_sensitivity_flag", "Flags material needing restricted handling", "Boolean (TRUE/FALSE); align to policy",
  "Core record and workflow metadata", "pii_present_flag", "Whether record contains PII", "Boolean (TRUE/FALSE); define what counts as PII",
  "Core record and workflow metadata", "redaction_status", "Redaction state", "Controlled list (none, partial, full)",
  
  # 2) Platform, content, and discovery context
  "Platform, content, and discovery context", "url", "URL of the content", "Store canonical + raw",
  "Platform, content, and discovery context", "url_canonical", "Normalized canonical URL", "Lowercase host; strip tracking params when policy allows",
  "Platform, content, and discovery context", "url_raw", "Raw URL as captured", "Preserve exactly for evidence fidelity",
  "Platform, content, and discovery context", "platform_name", "Platform name", "Controlled list; canonical names",
  "Platform, content, and discovery context", "platform_type", "Platform category", "Controlled list (social, ecommerce, forum, messaging, classifieds, web)",
  "Platform, content, and discovery context", "website_type", "Website/platform type in your system", "Controlled list (e.g., Social Media, E-commerce, B2B)",
  "Platform, content, and discovery context", "platform_country", "Platform primary jurisdiction (optional)", "ISO 3166-1 alpha-2/3",
  "Platform, content, and discovery context", "platform_language_default", "Platform default language (optional)", "ISO 639-1/2",
  "Platform, content, and discovery context", "content_type", "Content type", "Controlled list (post, listing, story, reel, comment, profile, channel, group)",
  "Platform, content, and discovery context", "content_id", "Platform content identifier", "Store exact platform ID string",
  "Platform, content, and discovery context", "content_permalink", "Stable permalink (if distinct)", "String",
  "Platform, content, and discovery context", "group_id", "Group/community ID", "String",
  "Platform, content, and discovery context", "group_name", "Group/community name", "Preserve original; store translated separately if needed",
  "Platform, content, and discovery context", "is_group", "Whether content is from a group/community", "Boolean (TRUE/FALSE)",
  "Platform, content, and discovery context", "group_type", "Group type", "Controlled list (e.g., Social Media, MEssaging App)",
  "Platform, content, and discovery context", "page_id", "Page/business ID", "String",
  "Platform, content, and discovery context", "page_name", "Page/business name", "Preserve original",
  "Platform, content, and discovery context", "channel_id", "Channel ID (video platforms)", "String",
  "Platform, content, and discovery context", "channel_name", "Channel name", "Preserve original",
  "Platform, content, and discovery context", "thread_id", "Thread/discussion ID", "String",
  "Platform, content, and discovery context", "search_term", "Search term/query used to discover", "Store as executed; may be sensitive",
  "Platform, content, and discovery context", "discovery_query_language", "Language of query terms", "ISO 639-1/2",
  "Platform, content, and discovery context", "referral_source", "How content was reached", "Controlled list (SERP, internal_search, link, tip, scraper)",
  "Platform, content, and discovery context", "referral_url", "Referring URL (if any)", "Normalize; can be NA",
  "Platform, content, and discovery context", "access_level", "Public vs requires login/membership", "Controlled list (public, logged_in, closed_group, invite_only)",
  "Platform, content, and discovery context", "ad_language", "Detected language of ad/content", "ISO 639-1/2; record method",
  "Platform, content, and discovery context", "item_title", "Title/headline text", "Preserve original",
  "Platform, content, and discovery context", "item_text", "Body text", "Preserve original",
  "Platform, content, and discovery context", "content_original_text", "Full original text (if title+body not enough)", "Preserve exactly",
  "Platform, content, and discovery context", "content_translation", "Translation (if needed)", "Keep separate from original",
  "Platform, content, and discovery context", "content_summary", "Analyst summary", "Avoid speculation; label assumptions",
  "Platform, content, and discovery context", "content_keywords", "Extracted keywords", "Token list; document extraction method",
  "Platform, content, and discovery context", "content_hashtags", "Hashtags", "Store list normalized + raw",
  "Platform, content, and discovery context", "coded_language_flag", "Coded terms suspected", "Boolean (TRUE/FALSE)",
  "Platform, content, and discovery context", "coded_terms", "Coded terms list", "Controlled list where possible; else free text",
  "Platform, content, and discovery context", "moderation_status", "Visible/removed/unknown", "Controlled list; often unknown",
  "Platform, content, and discovery context", "item_date_posted", "Date/time posted", "ISO 8601 UTC preferred",
  "Platform, content, and discovery context", "date_posted_local", "Local timestamp shown on platform", "ISO 8601 with offset if possible",
  "Platform, content, and discovery context", "date_observed_first_utc", "First observed by monitoring team", "ISO 8601 UTC",
  "Platform, content, and discovery context", "date_observed_last_utc", "Last confirmed live", "ISO 8601 UTC",
  "Platform, content, and discovery context", "engagement_likes", "Likes/reactions count", "Integer; unknown as NA",
  "Platform, content, and discovery context", "engagement_comments", "Comment count", "Integer; unknown as NA",
  "Platform, content, and discovery context", "engagement_shares", "Share/forward count", "Integer; unknown as NA",
  "Platform, content, and discovery context", "engagement_views", "View count (video)", "Integer; unknown as NA",
  "Platform, content, and discovery context", "content_duration_seconds", "Video/audio duration", "Seconds as numeric",
  
  # 3) Seller/account entity fields
  "Seller and account entity fields", "seller_entity_id", "Internal seller entity ID", "UUID; used for entity resolution",
  "Seller and account entity fields", "seller_profile_url", "Seller profile URL", "Store canonical + raw if needed",
  "Seller and account entity fields", "seller_user_id", "Platform user ID", "Store exact platform ID string",
  "Seller and account entity fields", "seller_username", "Seller handle/username", "Preserve exact and normalized lowercase",
  "Seller and account entity fields", "seller_display_name", "Seller display name", "Preserve original",
  "Seller and account entity fields", "seller_account_type", "Individual/business/breeder/shop/unknown", "Controlled list",
  "Seller and account entity fields", "seller_verified_flag", "Verified badge present", "Boolean (TRUE/FALSE)",
  "Seller and account entity fields", "seller_followers_count", "Followers/subscribers", "Integer",
  "Seller and account entity fields", "seller_following_count", "Following count", "Integer",
  "Seller and account entity fields", "seller_posts_count", "Total posts/listings", "Integer",
  "Seller and account entity fields", "seller_bio_text", "Profile bio/about", "Preserve original",
  "Seller and account entity fields", "seller_profile_language", "Language of profile", "ISO 639-1/2",
  "Seller and account entity fields", "seller_profile_location_text", "Location text in profile", "Preserve raw; parse to structured geo fields if possible",
  "Seller and account entity fields", "seller_contact_known", "Whether any seller contact info is present", "Boolean (TRUE/FALSE)",
  "Seller and account entity fields", "seller_contact_phone", "Phone number", "E.164 preferred + raw; hash for sharing",
  "Seller and account entity fields", "seller_contact_email", "Email address", "Store raw only if policy allows; hash for sharing",
  "Seller and account entity fields", "seller_contact_whatsapp", "WhatsApp contact", "E.164 where possible",
  "Seller and account entity fields", "seller_contact_telegram", "Telegram handle/link", "Normalize @handle + preserve raw",
  "Seller and account entity fields", "seller_contact_wechat", "WeChat ID", "String",
  "Seller and account entity fields", "seller_contact_other", "Other contact info", "Prefer structured contact_methods_json",
  "Seller and account entity fields", "contact_methods_json", "Structured contact methods", "JSON schema with type/value/raw/confidence",
  "Seller and account entity fields", "payment_method", "Payment method(s) mentioned", "Controlled list, Comma Delimited (e.g., Visa/Mastera, PayPal, Cashapp)",
  "Seller and account entity fields", "seller_payment_handles", "Payment handles (eg PayPal IDs)", "Controlled type list; hash sensitive IDs",
  "Seller and account entity fields", "seller_external_links", "External links from profile", "Normalize and store list",
  "Seller and account entity fields", "seller_profile_image_hash", "Profile image hash for matching", "pHash recommended; record algorithm",
  "Seller and account entity fields", "seller_entity_resolution_status", "Entity resolution status", "Controlled list (new, matched, merged, split_needed)",
  "Seller and account entity fields", "seller_linked_entity_ids", "Linked seller_entity_id values", "Prefer relation table; list if needed",
  
  # 4) Item, species, taxonomy, product fields
  "Item, species, taxonomy, and product fields", "item_id", "Unique item line within a record", "UUID or integer index within record",
  "Item, species, taxonomy, and product fields", "item_type", "Item type in your system", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_category", "Broad category", "Controlled list (live_animal, part, derivative, plant, timber, medicine, other)",
  "Item, species, taxonomy, and product fields", "item_common_name", "Standardized common/vernacular name", "Use GBIF/NCBI backbone or Catalogue of Life per language",
  "Item, species, taxonomy, and product fields", "item_common_name_raw", "Common name as written in the post", "Preserve original",
  "Item, species, taxonomy, and product fields", "item_common_name_website", "Common grouping label (e.g., Parrots, Primates, Big Cats)", "Controlled vernacular list",
  "Item, species, taxonomy, and product fields", "item_scientific_name", "Standardized scientific name", "Use GBIF/NCBI backbone or Catalogue of Life; store identifiers",
  "Item, species, taxonomy, and product fields", "item_scientific_name_raw", "Scientific name as written", "Preserve original",
  "Item, species, taxonomy, and product fields", "item_phylum", "Phylum", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_class", "Class", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_order", "Order", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_family", "Family", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_genus", "Genus", "Controlled list",
  "Item, species, taxonomy, and product fields", "item_species", "Species epithet or binomial (your convention)", "Define convention",
  "Item, species, taxonomy, and product fields", "item_taxa", "Taxa grouping label (eg Birds/Reptiles)", "Controlled list",
  "Item, species, taxonomy, and product fields", "taxon_rank", "Rank used for identification", "Controlled list (species, genus, family, etc)",
  "Item, species, taxonomy, and product fields", "gbif_species_key", "GBIF identifier (speciesKey)", "Integer; record backbone version/date",
  "Item, species, taxonomy, and product fields", "col_taxon_id", "Catalogue of Life taxon ID", "String; record release version",
  "Item, species, taxonomy, and product fields", "itis_tsn", "ITIS TSN", "Integer",
  "Item, species, taxonomy, and product fields", "cites_taxon_id", "CITES Checklist ID", "String; document checklist version",
  "Item, species, taxonomy, and product fields", "iucn_taxon_id", "IUCN taxon ID", "String; access may be restricted by license",
  "Item, species, taxonomy, and product fields", "identification_method", "How identified", "Controlled list (visual, text_only, seller_claim, expert, ML, mixed)",
  "Item, species, taxonomy, and product fields", "identification_confidence", "Confidence of identification", "Numeric; define scale (0-1 or 0-100)",
  "Item, species, taxonomy, and product fields", "life_stage", "Life stage", "Controlled list (adult, juvenile, egg, seed, unknown)",
  "Item, species, taxonomy, and product fields", "sex", "Sex", "Controlled list (male, female, mixed, unknown)",
  "Item, species, taxonomy, and product fields", "condition", "Condition (alive/dead/processed)", "Controlled list",
  "Item, species, taxonomy, and product fields", "product_form", "Form of item (skin, horn, ivory, meat, pet, etc)", "Controlled list; consortium codebook",
  "Item, species, taxonomy, and product fields", "product_description_raw", "Raw description of product", "Preserve original",
  "Item, species, taxonomy, and product fields", "item_hasEggs", "Eggs involved", "Boolean (TRUE/FALSE)",
  "Item, species, taxonomy, and product fields", "item_count", "Quantity", "Numeric",
  "Item, species, taxonomy, and product fields", "item_count_min", "Minimum quantity if range", "Numeric",
  "Item, species, taxonomy, and product fields", "item_count_max", "Maximum quantity if range", "Numeric",
  "Item, species, taxonomy, and product fields", "item_unit", "Unit of quantity", "Controlled list",
  "Item, species, taxonomy, and product fields", "weight_value", "Weight value if stated", "Numeric; store unit separately",
  "Item, species, taxonomy, and product fields", "weight_unit", "Weight unit", "Controlled list (g, kg, lb, oz)",
  "Item, species, taxonomy, and product fields", "volume_value", "Volume value if stated", "Numeric; store unit separately",
  "Item, species, taxonomy, and product fields", "volume_unit", "Volume unit", "Controlled list (mL, L, gal)",
  "Item, species, taxonomy, and product fields", "origin_claimed", "Claimed source (wild/captive/farmed)", "Prefer CITES source codes where applicable",
  "Item, species, taxonomy, and product fields", "captive_breeding_claim", "Captive breeding claim present", "Boolean (TRUE/FALSE)",
  "Item, species, taxonomy, and product fields", "health_documents_claimed", "Health/vet documents claimed", "Boolean (TRUE/FALSE)/unknown",
  "Item, species, taxonomy, and product fields", "permit_claimed_flag", "Permits claimed", "Boolean (TRUE/FALSE)/unknown",
  "Item, species, taxonomy, and product fields", "permit_type_claimed", "Type of permit claimed", "Controlled list (CITES, national, veterinary, other, unknown)",
  "Item, species, taxonomy, and product fields", "permit_number_raw", "Permit number as posted", "Sensitive; store redacted + hashed variant for sharing",
  "Item, species, taxonomy, and product fields", "legality_assessment", "Suspected legality", "Controlled list (legal, suspect_illegal, illegal, unknown)",
  "Item, species, taxonomy, and product fields", "illegality_indicators", "Reason codes for suspected illegality", "Controlled list; stable codebook",
  "Item, species, taxonomy, and product fields", "iucn_category", "IUCN Red List category", "Controlled list (LC, NT, VU, EN, CR, EW, EX, DD, NE)",
  "Item, species, taxonomy, and product fields", "item_CITES", "CITES Appendix indicator", "Controlled list (I, II, III, none, unknown)",
  "Item, species, taxonomy, and product fields", "national_protection_status", "National protection tier", "Country-specific controlled list; document source/version",
  "Item, species, taxonomy, and product fields", "conservation_notes", "Conservation notes", "Free text; keep concise",
  
  # 5) Price, currency, and commercial terms
  "Price, currency, and commercial terms", "price_available", "Whether a price is stated", "Boolean (TRUE/FALSE)",
  "Price, currency, and commercial terms", "item_price", "Listed price value", "Numeric; no currency symbols",
  "Price, currency, and commercial terms", "item_currency", "Currency", "ISO 4217",
  "Price, currency, and commercial terms", "price_currency_raw", "Currency as written", "Preserve original",
  "Price, currency, and commercial terms", "price_unit", "Unit price applies to", "Controlled list aligned to item_unit",
  "Price, currency, and commercial terms", "price_type", "Asking/negotiated/auction/wholesale", "Controlled list",
  "Price, currency, and commercial terms", "price_min", "Minimum price if range", "Numeric",
  "Price, currency, and commercial terms", "price_max", "Maximum price if range", "Numeric",
  "Price, currency, and commercial terms", "price_total_value", "Total transaction value if stated", "Numeric; define inclusion of shipping/tax",
  "Price, currency, and commercial terms", "shipping_cost_value", "Shipping cost", "Numeric",
  "Price, currency, and commercial terms", "shipping_cost_currency", "Shipping cost currency", "ISO 4217",
  "Price, currency, and commercial terms", "tax_included_flag", "Tax included flag", "Boolean (TRUE/FALSE)/unknown",
  "Price, currency, and commercial terms", "discount_flag", "Discount mentioned", "Boolean (TRUE/FALSE)/unknown",
  "Price, currency, and commercial terms", "discount_details", "Discount terms", "Preserve raw or structure fields",
  "Price, currency, and commercial terms", "min_order_quantity", "Minimum order quantity", "Numeric",
  "Price, currency, and commercial terms", "bulk_terms", "Wholesale/bulk terms", "Structured where possible",
  "Price, currency, and commercial terms", "trade_terms", "Trade/Incoterms if stated", "Controlled list (EXW, FOB, CIF, etc)",
  "Price, currency, and commercial terms", "availability_status", "Availability (in_stock/sold/unknown)", "Controlled list",
  
  # 6) Geography and geocoding (GADM compatible)
  "Geography and geocoding (GADM compatible)", "location_known", "Whether a usable location is present", "Boolean (TRUE/FALSE)",
  "Geography and geocoding (GADM compatible)", "location_level0", "Admin level 0 (country name)", "Use GADM naming for chosen version",
  "Geography and geocoding (GADM compatible)", "location_level1", "Admin level 1 name", "Use GADM naming",
  "Geography and geocoding (GADM compatible)", "location_level2", "Admin level 2 name", "Use GADM naming",
  "Geography and geocoding (GADM compatible)", "location_level3", "Admin level 3 name", "Use GADM naming",
  "Geography and geocoding (GADM compatible)", "location_level4", "Admin level 4 name", "Use GADM naming",
  "Geography and geocoding (GADM compatible)", "ad_location", "Raw location string from the ad", "Preserve original",
  "Geography and geocoding (GADM compatible)", "geo_context_type", "What the location represents", "Controlled list (seller_claimed, ship_from, ship_to, origin_claimed, inferred, meetup, other)",
  "Geography and geocoding (GADM compatible)", "geo_country_name", "Country name (canonical)", "Use GADM/ISO canonical name",
  "Geography and geocoding (GADM compatible)", "geo_country_iso2", "Country ISO2", "ISO 3166-1 alpha-2",
  "Geography and geocoding (GADM compatible)", "geo_country_iso3", "Country ISO3", "ISO 3166-1 alpha-3",
  "Geography and geocoding (GADM compatible)", "gadm_version", "GADM version used", "Store exact version string (eg 4.1)",
  "Geography and geocoding (GADM compatible)", "geo_admin0_gadm_id", "GADM admin0 ID", "Store exact GADM ID",
  "Geography and geocoding (GADM compatible)", "geo_admin1_name", "Admin1 name (canonical)", "Match GADM",
  "Geography and geocoding (GADM compatible)", "geo_admin1_gadm_id", "Admin1 GADM ID", "Store exact GADM ID",
  "Geography and geocoding (GADM compatible)", "geo_admin2_name", "Admin2 name (canonical)", "Match GADM",
  "Geography and geocoding (GADM compatible)", "geo_admin2_gadm_id", "Admin2 GADM ID", "Store exact GADM ID",
  "Geography and geocoding (GADM compatible)", "geo_admin3_gadm_id", "Admin3 GADM ID", "Store exact GADM ID",
  "Geography and geocoding (GADM compatible)", "geo_admin4_gadm_id", "Admin4 GADM ID", "Store exact GADM ID",
  "Geography and geocoding (GADM compatible)", "geo_locality_name", "Locality/city name", "Prefer gazetteer match",
  "Geography and geocoding (GADM compatible)", "geo_place_raw", "Raw place string", "Preserve original",
  "Geography and geocoding (GADM compatible)", "geo_lat", "Latitude", "WGS84 decimal degrees",
  "Geography and geocoding (GADM compatible)", "geo_lon", "Longitude", "WGS84 decimal degrees",
  "Geography and geocoding (GADM compatible)", "geo_coord_precision_m", "Coordinate precision estimate", "Numeric meters; define method/categories",
  "Geography and geocoding (GADM compatible)", "geo_bbox", "Bounding box if only area known", "min/max lat/lon or GeoJSON bbox",
  "Geography and geocoding (GADM compatible)", "geo_geocode_method", "Geocode method", "Controlled list (manual, gazetteer, NER, reverse_geocode, ML)",
  "Geography and geocoding (GADM compatible)", "geo_geocoder_name", "Geocoder/tool used", "Controlled list (Nominatim, Google, ArcGIS, internal)",
  "Geography and geocoding (GADM compatible)", "geo_geocode_confidence", "Geocode confidence", "Numeric; define scale",
  "Geography and geocoding (GADM compatible)", "geo_location_sensitivity", "Masking level for exports", "Controlled list (public, masked_admin2, masked_admin1, masked_country)",
  
  # 7) Shipping, routes, and movement indicators
  "Shipping, routes, and movement indicators", "ship_available_flag", "Shipping offered", "Boolean (TRUE/FALSE)/unknown",
  "Shipping, routes, and movement indicators", "ship_methods", "Shipping methods mentioned", "Controlled list (courier, post, bus, air, hand_carry, unknown)",
  "Shipping, routes, and movement indicators", "ship_origin_text", "Shipping origin (raw)", "Preserve raw + parse to geo",
  "Shipping, routes, and movement indicators", "ship_destination_text", "Shipping destination (raw)", "Preserve raw + parse to geo",
  "Shipping, routes, and movement indicators", "origin_country", "Origin country (claimed/inferred)", "ISO 3166-1 alpha-3 preferred",
  "Shipping, routes, and movement indicators", "destination_country", "Destination country (claimed/inferred)", "ISO 3166-1 alpha-3 preferred",
  "Shipping, routes, and movement indicators", "item_sold_in", "Market country where sold", "ISO 3166-1 alpha-3",
  "Shipping, routes, and movement indicators", "crossborder_flag", "Cross-border implied", "Boolean (TRUE/FALSE)/unknown",
  "Shipping, routes, and movement indicators", "meetup_flag", "Meetup/in-person handoff implied", "Boolean (TRUE/FALSE)/unknown",
  "Shipping, routes, and movement indicators", "meetup_location_text", "Meetup location (raw)", "Preserve raw + parse",
  "Shipping, routes, and movement indicators", "is_delivery_available", "Delivery/shipping offered (system flag)", "Boolean (TRUE/FALSE)",
  "Shipping, routes, and movement indicators", "delivery_method", "Delivery method (system)", "Controlled list",
  "Shipping, routes, and movement indicators", "delivery_time_text", "Claimed delivery time", "Preserve raw; optional parsed numeric days",
  "Shipping, routes, and movement indicators", "packaging_text", "Packaging/concealment mentions", "Sensitive; handle with policy",
  
  # 8) Media, evidence, and forensics metadata
  "Media, evidence, and forensics metadata", "media_count", "Number of media items", "Integer",
  "Media, evidence, and forensics metadata", "media_types", "Types present", "Controlled list (image, video, audio, document)",
  "Media, evidence, and forensics metadata", "media_url_list", "List of media URLs", "Normalize; access controls may apply",
  "Media, evidence, and forensics metadata", "item_image", "Image(s) captured/linked", "Store secure evidence refs",
  "Media, evidence, and forensics metadata", "image_text", "Text extracted from images (OCR)", "Record language + method",
  "Media, evidence, and forensics metadata", "screenshot_captured_flag", "Screenshots taken", "Boolean (TRUE/FALSE)",
  "Media, evidence, and forensics metadata", "screenshot_file_refs", "References to stored screenshots", "Use secure storage URIs",
  "Media, evidence, and forensics metadata", "screen_recording_flag", "Screen recording captured", "Boolean (TRUE/FALSE)",
  "Media, evidence, and forensics metadata", "media_file_hash_sha256", "SHA-256 hash for evidence", "Hex string; preferred",
  "Media, evidence, and forensics metadata", "media_file_hash_md5", "MD5 hash (legacy)", "Hex string",
  "Media, evidence, and forensics metadata", "media_phash", "Perceptual hash", "Document algorithm (pHash/dHash/aHash)",
  "Media, evidence, and forensics metadata", "media_exif_present", "EXIF present", "Boolean (TRUE/FALSE)",
  "Media, evidence, and forensics metadata", "media_exif_datetime", "EXIF datetime", "ISO 8601",
  "Media, evidence, and forensics metadata", "media_exif_gps_lat", "EXIF GPS latitude", "WGS84; sensitive",
  "Media, evidence, and forensics metadata", "media_exif_gps_lon", "EXIF GPS longitude", "WGS84; sensitive",
  "Media, evidence, and forensics metadata", "evidence_storage_system", "Evidence storage system", "Controlled list (S3, Drive, internal vault)",
  "Media, evidence, and forensics metadata", "evidence_access_control", "Evidence access class", "Controlled list (team, org, LE_only)",
  "Media, evidence, and forensics metadata", "chain_of_custody_id", "Chain of custody reference", "UUID/reference code",
  "Media, evidence, and forensics metadata", "capture_browser_user_agent", "Capture browser UA", "Standard UA string",
  "Media, evidence, and forensics metadata", "integrity_check_date_utc", "Evidence integrity check date", "ISO 8601 UTC",
  
  # 9) Linking, de-duplication, entity resolution, network fields
  "Linking, dedupe, and network fields", "dedupe_key", "Key used to detect duplicates", "Deterministic string; document construction",
  "Linking, dedupe, and network fields", "duplicate_group_id", "Duplicate group ID", "UUID",
  "Linking, dedupe, and network fields", "duplicate_of_record_id", "Canonical record id if duplicate", "UUID",
  "Linking, dedupe, and network fields", "duplicate_match_method", "How match was made", "Controlled list (url, hash, text_sim, image_phash, seller_id)",
  "Linking, dedupe, and network fields", "similarity_score", "Similarity score", "Numeric; define range",
  "Linking, dedupe, and network fields", "related_record_ids", "Related records", "Prefer relation table; list if needed",
  "Linking, dedupe, and network fields", "relatedness_type", "Relation type", "Controlled list (same_seller, same_group, repost, same_images, same_phone)",
  "Linking, dedupe, and network fields", "item_multiple_detections", "Detected multiple times", "Boolean (TRUE/FALSE)",
  "Linking, dedupe, and network fields", "seller_phone_hash", "Hashed phone for matching", "Salted hash; document policy",
  "Linking, dedupe, and network fields", "seller_email_hash", "Hashed email for matching", "Salted hash; document policy",
  "Linking, dedupe, and network fields", "seller_payment_hash", "Hashed payment handle for matching", "Salted hash; document policy",
  "Linking, dedupe, and network fields", "network_node_id", "Network node id", "Stable ID mapping to entity",
  "Linking, dedupe, and network fields", "network_edge_id", "Network edge id", "Stable ID mapping",
  "Linking, dedupe, and network fields", "network_edge_type", "Network edge type", "Controlled list (contact_shared, reposted, co_listed, co_admin, co_comment)",
  
  # 10) Legal and enforcement tracking (optional)
  "Legal and enforcement tracking (optional)", "case_id", "Internal case identifier", "UUID/structured code",
  "Legal and enforcement tracking (optional)", "case_status", "Case lifecycle status", "Controlled list (open, active, paused, closed)",
  "Legal and enforcement tracking (optional)", "is_case_of_interest", "Flag for elevated interest", "Boolean (TRUE/FALSE)",
  "Legal and enforcement tracking (optional)", "was_recommended", "Recommended/escalated", "Boolean (TRUE/FALSE)",
  "Legal and enforcement tracking (optional)", "referral_to_le_flag", "Referred to law enforcement", "Boolean (TRUE/FALSE)",
  "Legal and enforcement tracking (optional)", "referral_date_utc", "Referral date", "ISO 8601 UTC",
  "Legal and enforcement tracking (optional)", "referral_recipient_org", "Recipient agency", "Canonical org registry code",
  "Legal and enforcement tracking (optional)", "referral_channel", "Referral channel", "Controlled list (email, portal, meeting, secure_link)",
  "Legal and enforcement tracking (optional)", "referral_reference_id", "Recipient reference number", "String",
  "Legal and enforcement tracking (optional)", "intelligence_product_type", "Product type", "Controlled list (alert, brief, case_file, memo)",
  "Legal and enforcement tracking (optional)", "legal_basis_jurisdiction", "Jurisdiction", "ISO 3166-1 alpha-3",
  "Legal and enforcement tracking (optional)", "suspected_offense_type", "Suspected offense category", "Controlled list aligned to consortium taxonomy",
  "Legal and enforcement tracking (optional)", "enforcement_outcome", "Known outcome", "Controlled list (seizure, arrest, warning, takedown, unknown)",
  "Legal and enforcement tracking (optional)", "outcome_date_utc", "Outcome date", "ISO 8601 UTC",
  "Legal and enforcement tracking (optional)", "seizure_quantity", "Quantity seized", "Numeric",
  "Legal and enforcement tracking (optional)", "seizure_unit", "Unit seized", "Controlled list aligned to item_unit",
  "Legal and enforcement tracking (optional)", "seizure_species_confirmed", "Confirmed species post-seizure", "Scientific name standard + taxon ID",
  "Legal and enforcement tracking (optional)", "prosecution_flag", "Prosecution initiated", "Boolean (TRUE/FALSE)/unknown",
  "Legal and enforcement tracking (optional)", "court_case_reference", "Court reference", "Sensitive; access-controlled",
  
  # 11) Vulnerability and bycatch indicators (GMS)
  "Vulnerability and bycatch indicators", "is_seller_vulnerable_group", "Whether the seller appears to be a vulnerable group member (only if explicitly stated or clearly indicated in context)", "Boolean (TRUE/FALSE)/unknown; do NOT infer protected attributes; record only explicit self-identification or direct statements; document basis in note",
  "Vulnerability and bycatch indicators", "vulnerable_group", "Vulnerable group category (if applicable)", "Controlled list agreed by consortium; allow 'unknown'; avoid free text drift",
  "Vulnerability and bycatch indicators", "is_bycatch", "Whether the item is bycatch or incidental take", "Boolean (TRUE/FALSE)/unknown; capture basis (seller claim, expert assessment) in notes",
  
  # 11) Governance, interoperability, export controls
  "Governance, interoperability, and export controls", "schema_version", "Data model version used", "Semantic versioning (major.minor.patch)",
  "Governance, interoperability, and export controls", "field_subset_profile", "Subset profile applied", "Controlled list (eg NGO_light, LE_full)",
  "Governance, interoperability, and export controls", "sharing_license", "Terms for downstream use", "Controlled list (internal, partner, restricted, open)",
  "Governance, interoperability, and export controls", "data_retention_class", "Retention class", "Controlled list (30d, 1y, 5y, indefinite)",
  "Governance, interoperability, and export controls", "anonymization_level", "Anonymization level applied", "Controlled list (none, partial, strong)",
  "Governance, interoperability, and export controls", "export_timestamp_utc", "Export timestamp", "ISO 8601 UTC",
  "Governance, interoperability, and export controls", "export_request_id", "Export request id", "UUID",
  "Governance, interoperability, and export controls", "export_format", "Export format", "Controlled list (csv, xlsx, json, parquet)",
  "Governance, interoperability, and export controls", "interoperability_profile", "Interoperability profile", "Controlled list (DarwinCore, CITES_like, internal)",
  
  # 12) Automation, scraping, and model-assist fields
  "Automation, scraping, and model-assist fields", "ingest_pipeline", "Pipeline name", "Controlled list",
  "Automation, scraping, and model-assist fields", "ingest_run_id", "Pipeline run id", "UUID",
  "Automation, scraping, and model-assist fields", "scraped_html_stored_flag", "Whether HTML was archived", "Boolean (TRUE/FALSE); ensure policy compliance",
  "Automation, scraping, and model-assist fields", "scraper_source", "Scraper tool name/version", "Versioned string",
  "Automation, scraping, and model-assist fields", "extraction_model_name", "Model used for extraction/classification", "Versioned string; do not store secrets",
  "Automation, scraping, and model-assist fields", "extraction_confidence", "Extraction/classification confidence", "Numeric; define range",
  "Automation, scraping, and model-assist fields", "classification_label", "Model label (ad, non-ad, unsure)", "Controlled list",
  "Automation, scraping, and model-assist fields", "species_ml_suggestion", "Model-suggested taxon", "Scientific name + taxon ID if possible",
  "Automation, scraping, and model-assist fields", "species_ml_confidence", "Confidence of species suggestion", "Numeric; define range",
  "Automation, scraping, and model-assist fields", "location_ml_suggestion", "Model-suggested location", "Structured geo fields + confidence",
  "Automation, scraping, and model-assist fields", "pii_auto_detected_flag", "Automated PII detection flag", "Boolean (TRUE/FALSE)",
  "Automation, scraping, and model-assist fields", "text_embedding_id", "Embedding reference id", "Store id only; embeddings stored separately",
  "Automation, scraping, and model-assist fields", "content_fingerprint", "Text fingerprint for matching", "Document algorithm"
) %>%
  mutate(in_gms = field_id %in% gms_fields) %>%
  arrange(category, field_id)

all_field_ids <- gdm_fields$field_id

# -----------------------------
# Baseline profiles (monitoring categories)
# Used to highlight minimum necessary fields
# -----------------------------
profiles <- list(
  "Global Monitoring System" = gms_fields,
  
  "General monitoring" = c(
    "record_id","record_date","datahub",
    "url","platform_name","website_type","content_type",
    "is_group","group_type","group_name",
    "item_title","item_text","ad_language","search_term",
    "was_recommended","is_case_of_interest","case_id",
    "location_known","location_level0","location_level1","location_level2","ad_location",
    "origin_country","destination_country","item_sold_in",
    "item_type","item_category","item_common_name","item_common_name_website",
    "item_phylum","item_class","item_order","item_family","item_genus","item_species","item_taxa",
    "item_CITES","iucn_category","legality_assessment","illegality_indicators",
    "item_date_posted",
    "price_available","item_price","item_currency","item_count","item_unit",
    "seller_contact_known","payment_method","is_delivery_available","delivery_method",
    "note"
  ),
  "Academic research (low PII)" = c(
    "record_id","record_date","collection_program","collecting_organization",
    "collection_method","monitoring_mode","validation_status",
    "url","platform_name","platform_type","content_type","access_level",
    "ad_language","item_title","item_text","content_translation",
    "item_date_posted","date_observed_first_utc",
    "engagement_likes","engagement_comments","engagement_shares","engagement_views",
    "location_level0","location_level1","location_level2","geo_lat","geo_lon","geo_geocode_method","geo_geocode_confidence","gadm_version",
    "origin_country","destination_country","item_sold_in",
    "item_category","item_common_name","item_scientific_name","gbif_species_key","col_taxon_id",
    "item_phylum","item_class","item_order","item_family","item_genus","item_species","taxon_rank","identification_method","identification_confidence",
    "item_CITES","iucn_category","national_protection_status",
    "price_available","item_price","item_currency","item_count","item_unit",
    "legality_assessment","illegality_indicators",
    "anonymization_level","data_source_confidentiality"
  ),
  "Law enforcement" = c(
    "record_id","record_date","collection_program","collecting_organization","collecting_unit","analyst_id","review_status","review_date_utc",
    "url","url_raw","platform_name","platform_type","content_type","access_level",
    "is_group","group_id","group_name","page_id","page_name",
    "item_title","item_text","ad_language","search_term",
    "item_date_posted","date_observed_first_utc","date_observed_last_utc",
    "seller_entity_id","seller_profile_url","seller_user_id","seller_username","seller_display_name","seller_account_type",
    "seller_contact_known","seller_contact_phone","seller_contact_email","seller_contact_whatsapp","seller_contact_telegram","seller_contact_wechat","seller_contact_other",
    "payment_method","seller_payment_handles",
    "location_known","location_level0","location_level1","location_level2","ad_location","geo_lat","geo_lon","geo_geocode_method","geo_geocode_confidence","gadm_version",
    "origin_country","destination_country","item_sold_in","crossborder_flag","ship_methods","ship_origin_text","ship_destination_text",
    "item_category","item_type","product_form","condition","life_stage","sex",
    "item_common_name","item_scientific_name","gbif_species_key","col_taxon_id","cites_taxon_id",
    "item_CITES","iucn_category","national_protection_status",
    "permit_claimed_flag","permit_type_claimed","permit_number_raw","captive_breeding_claim",
    "price_available","item_price","item_currency","item_count","item_unit","price_type",
    "item_image","image_text","screenshot_captured_flag","screenshot_file_refs","media_file_hash_sha256","media_phash","chain_of_custody_id",
    "legality_assessment","illegality_indicators","suspected_offense_type","legal_basis_jurisdiction",
    "referral_to_le_flag","referral_date_utc","referral_recipient_org","referral_reference_id","intelligence_product_type",
    "case_id","case_status","enforcement_outcome","outcome_date_utc",
    "evidence_access_control","data_source_confidentiality","legal_sensitivity_flag","pii_present_flag","redaction_status",
    "note"
  ),
  "Trend analysis" = c(
    "record_id","record_date",
    "url","platform_name","platform_type","website_type",
    "content_type","ad_language",
    "item_date_posted","date_observed_first_utc",
    "location_level0","location_level1","location_level2",
    "origin_country","destination_country","item_sold_in",
    "item_taxa","item_common_name","item_genus","item_species","item_CITES","iucn_category",
    "item_count","item_unit",
    "price_available","item_price","item_currency",
    "item_multiple_detections","duplicate_group_id",
    "risk_score","risk_score_method"
  ),
  "Policy and regulation" = c(
    "record_id","record_date","collection_program","collecting_organization",
    "platform_name","platform_type","content_type","ad_language",
    "item_date_posted","location_level0","location_level1","location_level2",
    "origin_country","destination_country","item_sold_in","crossborder_flag",
    "item_category","product_form","condition",
    "item_common_name","item_scientific_name","item_genus","item_species","item_taxa",
    "item_CITES","iucn_category","national_protection_status",
    "legality_assessment","illegality_indicators","suspected_offense_type","legal_basis_jurisdiction",
    "price_available","item_price","item_currency","item_count","item_unit",
    "enforcement_outcome","outcome_date_utc","seizure_quantity","seizure_unit","seizure_species_confirmed",
    "sharing_license","anonymization_level","data_source_confidentiality"
  )
)

profiles <- lapply(profiles, function(x) unique(x[x %in% all_field_ids]))
profile_names <- names(profiles)

default_profile <- "Global Monitoring System"
if (!default_profile %in% profile_names) default_profile <- profile_names[1]

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; }
      .small-help { color: #666; font-size: 12px; }
      .summary-box { padding: 10px 12px; background: #f7f7f7; border-radius: 10px; margin-bottom: 10px; }
      .btn-row { display:flex; gap:8px; }
      .btn-row .btn { flex:1; }
      .pill { display:inline-block; padding:2px 8px; border-radius: 999px; background:#f2f2f2; margin-left:6px; font-size: 12px;}
    "))
  ),
  
  titlePanel("Global Data Model Builder"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput("profile", "Monitoring category baseline", choices = profile_names, selected = default_profile),
      div(class = "small-help", "Baseline fields are highlighted. You can add/remove fields from the exportable template."),
      
      hr(),
      
      checkboxInput("show_only_gms", "Show only fields that exist in current GMS schema", value = FALSE),
      
      div(class = "btn-row",
          actionButton("btn_add", "Add selected rows"),
          actionButton("btn_remove", "Remove selected rows")
      ),
      
      div(class = "btn-row",
          actionButton("btn_reset_baseline", "Reset to baseline"),
          actionButton("btn_clear_all", "Clear all selected")
      ),
      
      hr(),
      
      h4("Export"),
      radioButtons(
        "export_mode",
        "Export mode",
        choices = c(
          "Blank template (headers only)" = "blank",
          "Data dictionary (Field_ID + description + standards)" = "dictionary"
        ),
        selected = "blank"
      ),
      checkboxInput("include_category_in_dictionary", "Include category column in dictionary export", value = TRUE),
      checkboxInput("include_in_gms_flag", "Include 'in_gms' flag in dictionary export", value = TRUE),
      
      fluidRow(
        column(6, downloadButton("download_csv", "Export .csv", width = "100%")),
        column(6, downloadButton("download_xlsx", "Export .xlsx", width = "100%"))
      ),
      
      if (!.has_openxlsx) {
        div(class = "small-help", "Excel export requires openxlsx. Install with install.packages('openxlsx').")
      },
      
      hr(),
      div(class = "small-help",
          "How to use:",
          tags$ul(
            tags$li("Pick a monitoring category baseline (dropdown)."),
            tags$li("Scroll the table and select rows you want to add/remove."),
            tags$li("Click Add or Remove."),
            tags$li("Export includes only the fields currently selected (highlighted).")
          )
      )
    ),
    
    mainPanel(
      width = 8,
      
      uiOutput("selection_summary"),
      
      DTOutput("fields_table"),
      
      br(),
      div(class = "small-help",
          "Endless scrolling is enabled. Categories appear as headers. ",
          "Selected (exportable) fields are highlighted. Row selection is temporary until you click Add/Remove."
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    selected_fields = profiles[[default_profile]]
  )
  
  # Helper: build table data with a highlight flag column
  table_data <- reactive({
    df <- gdm_fields
    
    if (isTRUE(input$show_only_gms)) {
      df <- df %>% filter(in_gms)
    }
    
    df %>%
      mutate(
        selected_template = as.integer(field_id %in% (rv$selected_fields %||% character(0))),
        in_gms_label = ifelse(in_gms, "Yes", "No")
      ) %>%
      select(
        category,
        Field_ID = field_id,
        description,
        `notes on data standardization` = standardization_notes,
        `in GMS` = in_gms_label,
        selected_template
      ) %>%
      arrange(category, Field_ID)
  })
  
  # Reset selection to baseline when profile changes (and when reset button clicked)
  observeEvent(input$profile, {
    rv$selected_fields <- profiles[[input$profile]]
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_reset_baseline, {
    rv$selected_fields <- profiles[[input$profile]]
  })
  
  observeEvent(input$btn_clear_all, {
    rv$selected_fields <- character(0)
  })
  
  # Render table once; update via proxy replaceData thereafter
  output$fields_table <- renderDT({
    df <- table_data()
    
    dt <- datatable(
      df,
      rownames = FALSE,
      selection = list(mode = "multiple", selected = integer(0)),
      filter = "top",
      extensions = c("RowGroup", "Scroller"),
      options = list(
        deferRender = TRUE,
        scrollY = "70vh",
        scrollCollapse = TRUE,
        scroller = TRUE,
        paging = TRUE,
        pageLength = 75,
        dom = "ftip",
        rowGroup = list(dataSrc = 0),
        order = list(list(0, "asc"), list(1, "asc")),
        columnDefs = list(
          list(visible = FALSE, targets = c(0)),  # hide category column (still used for RowGroup)
          list(visible = FALSE, targets = c(5))   # hide selected_template flag column
        )
      )
    ) %>%
      formatStyle(
        "selected_template",
        target = "row",
        backgroundColor = styleInterval(0.5, c("", "#E7F5FF"))
      )
    
    dt
  })
  
  proxy <- DT::dataTableProxy("fields_table")
  
  # Keep table highlights in sync without resetting scroll
  observeEvent(list(rv$selected_fields, input$show_only_gms), {
    DT::replaceData(proxy, table_data(), resetPaging = FALSE, rownames = FALSE)
    DT::selectRows(proxy, integer(0))
  }, ignoreInit = TRUE)
  
  # Add/Remove actions based on currently selected rows in the table (temporary selection)
  observeEvent(input$btn_add, {
    df <- table_data()
    rows <- input$fields_table_rows_selected %||% integer(0)
    if (length(rows) == 0) return()
    
    add_ids <- df$Field_ID[rows]
    rv$selected_fields <- sort(unique(c(rv$selected_fields %||% character(0), add_ids)))
    
    DT::selectRows(proxy, integer(0))
  })
  
  observeEvent(input$btn_remove, {
    df <- table_data()
    rows <- input$fields_table_rows_selected %||% integer(0)
    if (length(rows) == 0) return()
    
    rm_ids <- df$Field_ID[rows]
    rv$selected_fields <- setdiff(rv$selected_fields %||% character(0), rm_ids)
    
    DT::selectRows(proxy, integer(0))
  })
  
  output$selection_summary <- renderUI({
    sel <- rv$selected_fields %||% character(0)
    total <- length(sel)
    gms_overlap <- sum(sel %in% gms_fields)
    
    div(
      class = "summary-box",
      tags$b("Baseline: "), input$profile, tags$span(class="pill", paste0("Selected: ", total)), tags$span(class="pill", paste0("In GMS: ", gms_overlap))
    )
  })
  
  # ---- Export helpers (selected fields across ALL categories) ----
  export_blank_template <- function(fields) {
    as.data.frame(setNames(replicate(length(fields), logical(0), simplify = FALSE), fields))
  }
  
  export_dictionary <- function(df, include_category, include_in_gms) {
    out <- df %>%
      transmute(
        category = category,
        Field_ID = field_id,
        description = description,
        `notes on data standardization` = standardization_notes,
        in_gms = in_gms
      )
    if (!include_category) out <- out %>% select(-category)
    if (!include_in_gms) out <- out %>% select(-in_gms)
    out
  }
  
  safe_slug <- function(x) {
    x %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_replace_all("^_|_$", "")
  }
  
  output$download_csv <- downloadHandler(
    filename = function() {
      prof_slug <- safe_slug(input$profile)
      base <- if (input$export_mode == "blank") "gdm_template" else "gdm_dictionary"
      paste0(base, "_", prof_slug, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      sel <- rv$selected_fields %||% character(0)
      if (length(sel) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      if (input$export_mode == "blank") {
        out <- export_blank_template(sel)
        write.csv(out, file, row.names = FALSE, na = "")
      } else {
        df <- gdm_fields %>%
          filter(field_id %in% sel) %>%
          mutate(in_gms = field_id %in% gms_fields) %>%
          arrange(category, field_id)
        
        out <- export_dictionary(df, input$include_category_in_dictionary, input$include_in_gms_flag)
        write.csv(out, file, row.names = FALSE, na = "")
      }
    }
  )
  
  output$download_xlsx <- downloadHandler(
    filename = function() {
      prof_slug <- safe_slug(input$profile)
      base <- if (input$export_mode == "blank") "gdm_template" else "gdm_dictionary"
      paste0(base, "_", prof_slug, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      if (!.has_openxlsx) {
        showNotification("Excel export requires openxlsx. Install with install.packages('openxlsx').", type = "error")
        return()
      }
      
      sel <- rv$selected_fields %||% character(0)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "export")
      
      if (length(sel) == 0) {
        openxlsx::writeData(wb, "export", data.frame())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }
      
      if (input$export_mode == "blank") {
        out <- export_blank_template(sel)
        openxlsx::writeData(wb, "export", out)
      } else {
        df <- gdm_fields %>%
          filter(field_id %in% sel) %>%
          mutate(in_gms = field_id %in% gms_fields) %>%
          arrange(category, field_id)
        
        out <- export_dictionary(df, input$include_category_in_dictionary, input$include_in_gms_flag)
        openxlsx::writeData(wb, "export", out)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
}
