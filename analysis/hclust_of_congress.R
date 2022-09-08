####################################################################################
####################################################################################
####################################################################################
# Linguistic analysis of Congress Twitter 2011-2022 using hierarchical clustering
####################################################################################
####################################################################################
####################################################################################

# Data from ProPublica as of 8/5/2022 collected for the Politiwoops project
# Twitter data filtered for all official Congress accounts using Ballard et al.'s 2022 [list](https://onlinelibrary.wiley.com/doi/abs/10.1111/lsq.12374)
# The lexicon of emotional, affective language comes from Simchon et al.'s 2022 [study](https://academic.oup.com/pnasnexus/article/1/1/pgac019/6546199)

library(tidyverse)
library(tidytext)        # text analysis
library(jsonlite)        # ingest JSON
library(ape)             # visualize cluster dendrograms
library(rtweet)          # hit Twitter API
library(anytime)         # convert unix to datetime

# All official legislator handles from Ballard et al 2022
# https://dataverse.harvard.edu/file.xhtml?fileId=5747889&version=1.0
cong_handles_master_and_ids = read.csv("cong_handles_master_and_ids08082022.csv")
glimpse(cong_handles_master_and_ids)

# For tokenizing tweets
reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

# This analysis will cluster and visualize 2011-2012 data.
# Simply copy the code and re-do for the 113th through 117th congresses.

#############################################
#############################################
#############################################
# DO ALL CONGRESS 2011_2012 W/ 112th FILTER
#############################################
#############################################
#############################################

# 112th
tweets_2011_2012 = readRDS("tweets_2011_2012.RDS")
glimpse(tweets_2011_2012)

propublica_tweets_112 = tweets_2011_2012 %>%
  mutate(twitter_lower = tolower(RECORDS.user_name)) %>%
  inner_join(cong_handles_master_and_ids %>%
               filter(c112th==1 & Official == 1) %>%
               dplyr::select(twitter_lower), by="twitter_lower") %>%
  dplyr::rename(user_name = RECORDS.user_name,
                first =RECORDS.first_name,
                last = RECORDS.last_name,
                party_id = RECORDS.party_id,
                state=RECORDS.state,
                text=RECORDS.content) %>%
  mutate(PARTY = case_when(
    party_id == 1 ~ "D",
    party_id == 2 ~ "R",
    party_id == 3 ~ "I"),
    name = paste0(first, " ", last, " ", PARTY, "-",state)) %>%
  distinct() %>%
  mutate(year = case_when(
    str_detect(created_at, "2011") ~ 2011,
    str_detect(created_at, "2012") ~ 2012),
  month = case_when(
    str_detect(created_at, "Jan") ~ 1,
    str_detect(created_at, "Feb") ~ 2,
    str_detect(created_at, "Mar") ~ 3,
    str_detect(created_at, "Apr") ~ 4,
    str_detect(created_at, "May") ~ 5,
    str_detect(created_at, "Jun") ~ 6,
    str_detect(created_at, "Jul") ~ 7,
    str_detect(created_at, "Aug") ~ 8,
    str_detect(created_at, "Sep") ~ 9,
    str_detect(created_at, "Oct") ~ 10,
    str_detect(created_at, "Nov") ~ 11,
    str_detect(created_at, "Dec") ~ 12
  ))
glimpse(propublica_tweets_112)

options(scipen = 999)

all_posts_2011_2012 = propublica_tweets_112 %>%
  dplyr::select(name, text, PARTY, twitter_lower) %>%
  distinct()
glimpse(all_posts_2011_2012)

# EDA
all_posts_2011_2012 %>%
  dplyr::select(name, PARTY) %>%
  distinct() %>%
  count(PARTY)

all_words_2011_2012 = all_posts_2011_2012 %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(substr(word, 1, 1) != '#',
         substr(word, 1, 1) != '@') %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("t.co", "amp","https", "www", "http",
                      "facebook","fb","ly","bit","rt","ow","ht")) %>%
  count(word, name) %>%
  filter(n >= 5)
glimpse(all_words_2011_2012)

tweets_corr_2011_2012 <- all_words_2011_2012 %>%
  subset(!is.na(name)) %>%
  group_by(name) %>%
  mutate(Prop = n / sum(n))  %>%
  subset(n >= 5) %>%
  select(-n) %>%
  spread(name, Prop)
tweets_corr_2011_2012

tweets_corr_2011_2012[is.na(tweets_corr_2011_2012)] <- 0
tweets_corr_t_2011_2012 <- t(tweets_corr_2011_2012[,-1])
tweets_dist_2011_2012 <- dist(tweets_corr_t_2011_2012, method="euclidean")
tweets_dist_2011_2012
fit_2011_2012 <- hclust(tweets_dist_2011_2012, method="ward.D")

clustered_data_2011_2012 <- cutree(fit_2011_2012, k=10)
clustered_data_tidy_2011_2012 <- as.data.frame(as.table(clustered_data_2011_2012)) %>% glimpse()
colnames(clustered_data_tidy_2011_2012) <- c("name","cluster")
clustered_data_tidy_2011_2012$name <- as.character(clustered_data_tidy_2011_2012$name)
glimpse(clustered_data_tidy_2011_2012)

joined_clusters_2011_2012 <- all_posts_2011_2012 %>%
  inner_join(clustered_data_tidy_2011_2012, by = "name") %>%
  distinct()
glimpse(joined_clusters_2011_2012)

names_by_cluster_hclust_2011_2012 = joined_clusters_2011_2012 %>%
  count(name, cluster) %>%
  glimpse()

clustered_data_tidy_party_2011_2012 = clustered_data_tidy_2011_2012 %>%
  inner_join(joined_clusters_2011_2012 %>%
               dplyr::select(name, PARTY) %>%
               distinct(), by="name") %>%
  mutate(color = ifelse(PARTY == "D", "#3279E6", "#EA5558"),
         labels = "●")

party_by_cluster_hclust_2011_2012 = joined_clusters_2011_2012 %>%
  dplyr::count(PARTY, cluster) %>%
  pivot_wider(names_from = "PARTY", values_from = "n") %>%
  glimpse()

party_pols_by_cluster_hclust_2011_2012 = joined_clusters_2011_2012 %>%
  dplyr::select(name, PARTY, cluster) %>%
  distinct() %>%
  dplyr::count(PARTY, cluster) %>%
  pivot_wider(names_from = "PARTY", values_from = "n") %>%
  glimpse()

top_words_and_names_by_hierar_cluster_2011_2012 = joined_clusters_2011_2012 %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(substr(word, 1, 1) != '#'
         ,substr(word, 1, 1) != '@') %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("t.co", "amp","https", "www", "http","facebook",
                      "fb","ly","bit","rt","ow","ht")) %>%
  dplyr::count(cluster, word) %>%
  arrange(n) %>%
  group_by(cluster) %>%
  top_n(20, n) %>%
  arrange(-n) %>%
  select(cluster, word) %>%
  dplyr::summarise(words = list(word)) %>%
  dplyr::mutate(words = purrr::map(words, paste, collapse = ", ")) %>%
  inner_join(party_pols_by_cluster_hclust_2011_2012, "cluster") %>%
  inner_join(names_by_cluster_hclust_2011_2012 %>%
               arrange(n) %>%
               group_by(cluster) %>%
               top_n(40, n) %>%
               arrange(-n) %>%
               select(cluster, name) %>%
               dplyr::summarise(names = list(name)) %>%
               dplyr::mutate(names = purrr::map(names, paste, collapse = ", ")), by="cluster")

top_words_by_hierar_cluster_2011_2012 = joined_clusters_2011_2012 %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(substr(word, 1, 1) != '#'
         ,substr(word, 1, 1) != '@') %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("t.co", "amp","https", "www", "http","facebook",
                      "fb","ly","bit","rt","ow","ht")) %>%
  dplyr::count(cluster, word) %>%
  arrange(n) %>%
  group_by(cluster) %>%
  top_n(40, n) %>%
  arrange(-n) %>%
  dplyr::select(cluster, word, n)



###########################
# VISUALIZE 2011-2012
###########################

clustered_data_tidy_party_2011_2012 = clustered_data_tidy_2011_2012 %>%
  inner_join(joined_clusters_2011_2012 %>%
               dplyr::select(name, PARTY) %>%
               distinct(), by="name") %>%
  mutate(color = ifelse(PARTY == "R", "#EA5558", "#3279E6"),
         labels = "●")

fit_2011_2012_lab = fit_2011_2012
fit_2011_2012_lab$colors = clustered_data_tidy_party_2011_2012$color
fit_2011_2012_lab$labels = clustered_data_tidy_party_2011_2012$labels

png("clusters_2011_2012_propublica_official_1200.png", width = 1200, height=1200, res=300)
plot(as.phylo(fit_2011_2012_lab),
     type = "unrooted",
     use.edge.length = FALSE,
     tip.color = fit_2011_2012_lab$colors,
     cex = 0.9,
     lab4ut = "axial",
     edge.color = "gray80",
     show.tip.label = T,
     no.margin = TRUE)
dev.off()

saveRDS(fit_2011_2012, "clusters2022/fit_2011_2012_propublica.RDS")
saveRDS(top_words_by_hierar_cluster_2011_2012, "clusters2022/top_words_by_hierar_cluster_2011_2012_propublica.RDS")


########################################################################
########################################################################
# USE OF EMOTIONAL/AFFECTIVE LANG OVER TIME
########################################################################
########################################################################

# Simchon's lexicon
affective = read.csv("affective_lexicon.csv")

all_congress_112_117_tokenized = all_congress_112_117_years %>%
  dplyr::select(text, year, month, PARTY) %>%
  mutate(date1 = paste0(month,"/",year),
         date = lubridate::my(date1),
         six_month = lubridate::floor_date(date, "6 months")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(substr(word, 1, 1) != '#'
         ,substr(word, 1, 1) != '@'
  ) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("t.co", "amp","https", "www", "http",
                      "facebook","fb","ly","bit","rt","ow","ht")) %>%
  add_count(date, PARTY, name="partywords")
glimpse(all_congress_112_117_tokenized)

all_congress_112_117_tokenized_summarized = all_congress_112_117_tokenized %>%
  filter(PARTY != "I") %>%
  inner_join(affective, by="word") %>%
  filter(word != "violence") %>%
  group_by(date, PARTY, partywords) %>%
  count(word) %>%
  glimpse() %>%
  summarise(all_affective_words = sum(n),
            affective_prop = all_affective_words/partywords) %>%
  distinct()

all_congress_112_117_tokenized_summarized %>%
  ggplot(aes(x=date, y=affective_prop, color=PARTY)) +
  geom_line() +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(title="Uptick in emotional, combative language on Congress Twitter") +
  scale_color_manual(values =c("#1375b7", "#c93135"))



##############################################################
##############################################################
# VISUALIZE AND INSPECT FOR WORD OVERLAPS
##############################################################
##############################################################


clustered_data_tidy_party_2011_2012 = clustered_data_tidy_2011_2012 %>%
  inner_join(joined_clusters_2011_2012 %>%
               dplyr::select(name, PARTY) %>%
               distinct(), by="name") %>%
  mutate(color = ifelse(PARTY == "D", "#3279E6", "#EA5558"), #labels = "\u25cf"
         labels = "o")
glimpse(clustered_data_tidy_party_2011_2012)

fit_2011_2012_lab = fit_2011_2012
fit_2011_2012_lab$colors = clustered_data_tidy_party_2011_2012$color
fit_2011_2012_lab$labels = clustered_data_tidy_party_2011_2012$labels

plot(as.phylo(fit_2011_2012_lab),
     type = "unrooted",
     use.edge.length = FALSE,
     tip.color = fit_2011_2012_lab$colors,
     cex = 0.9,
     lab4ut = "axial",
     edge.color = "gray80",
     show.tip.label = T,
     no.margin = TRUE)

str(fit_2011_2012)
plot(as.phylo(fit_2011_2012),
     cex = 0.3,
     type = "fan")

# LABEL CLUSTER ON PLOT
fit_2011_2012_lab$labels = clustered_data_tidy_party_2011_2012$cluster

plot(as.phylo(fit_2011_2012_lab),
     type = "unrooted",
     use.edge.length = FALSE,
     tip.color = fit_2011_2012_lab$colors,
     cex = 0.9,
     lab4ut = "axial",
     edge.color = "gray80",
     show.tip.label = T,
     no.margin = TRUE)

# LABEL SPECIFIC POLITICIAN ON PLOT
label_pol = clustered_data_tidy_2011_2012 %>%
  inner_join(joined_clusters_2011_2012 %>%
               dplyr::select(name, PARTY) %>%
               distinct(), by="name") %>%
  mutate(labels = ifelse(str_detect(name,
                                    "Howard Berman"), "#3279E6", ""))

fit_2011_2012_lab$labels = label_pol$labels
plot(as.phylo(fit_2011_2012_lab),
     type = "unrooted",
     use.edge.length = FALSE,
     cex = 0.9,
     lab4ut = "axial",
     edge.color = "gray80",
     show.tip.label = T,
     no.margin = TRUE)

# EXPLORE OVERLAP IN WORDS BETWEEN TWO POLITICIANS
pol1 = joined_clusters_2011_2012 %>%
  filter(str_detect(name, "Sherrod Brown")) %>%
  glimpse() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  dplyr::filter(!word %in% c("t.co", "https","http", "rt", "amp")) %>%
  dplyr::count(word, sort=T)

pol2 = joined_clusters_2011_2012 %>%
  filter(str_detect(name, "Olympia Snowe")) %>%
  glimpse() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  dplyr::filter(!word %in% c("t.co", "https", "http", "rt", "amp")) %>%
  dplyr::count(word, sort=T)

overlap = pol1 %>%
  dplyr::rename(pol1_n = n) %>%
  inner_join(pol2 %>%
               dplyr::rename(pol2_n = n), "word") %>%
  mutate(comb = pol1_n+pol2_n) %>%
  glimpse()




####################################################################################
####################################################################################
#### ANALYSIS OF DOMAINS BY POLITICAL BIAS USING MEDIA BIAS FACT CHECK
####################################################################################
####################################################################################


all_graz_112_117_urls = all_graz_112_117 %>%
  mutate(domain = str_extract(expanded_urls, "(?<=//)[^\\s:/]+"),
         url2 = str_extract(expanded_urls, "(?<=//)[^\\s:]+"),
         url = gsub("']","", url2),
         domain = gsub("www.","",domain))

mbfc_database = read.csv("mbfc_database_082622.csv") %>%
  dplyr::select(Domain, Bias) %>%
  mutate(bias_score = case_when(
    Bias == "Left" ~ "-2",
    Bias == "Left-Center" ~ "-1",
    Bias == "Least Biased" ~ "0",
    Bias == "Right-Center" ~ "1",
    Bias == "Right" ~ "2",
    Bias == "Questionable " ~ "NA",
    Bias == "Satire" ~ "NA",
    Bias == "Pro-Science" ~ "NA",
    Bias == "Conspiracy-Pseudoscience" ~ "NA",
    Bias == "Low" ~ "NA"
  ),
  bias_score = as.numeric(bias_score),
  domain = gsub("www.", "", Domain)) %>%
  dplyr::select(domain, bias_score, Bias) %>%
  bind_rows(data.frame(domain = c("nyti.ms","wapo.st"),
                       bias_score = c(-1, -1)))
glimpse(mbfc_database)

# Pre-2017 some of these domains have different scores,
# so we use MBFC scores saved by an academic researcher

mbfc_2016 = sources %>%
  mutate(domain = gsub("http://","", homepage),
         domain = gsub("https://", "", domain),
         domain = gsub("www.","", domain),
         domain = gsub("\\/","", domain),
         domain = gsub("#","", domain),
         domain = gsub("#\\w+", "", domain)) %>%
  glimpse() %>%
  mutate(bias_score = case_when(
    bias == "left" ~ "-2",
    bias == "leftcenter" ~ "-1",
    bias == "center" ~ "0",
    bias == "right-center" ~ "1",
    bias == "right" ~ "2",
    bias == "satire" ~ "NA",
    bias == "pro-science" ~ "NA",
    bias == "conspiracy" ~ "NA"),
    bias_score = as.numeric(bias_score),
    year = 2016) %>%
  mutate(Bias = case_when(
    bias == "left" ~ "Left",
    bias == "leftcenter" ~ "Left-Center",
    bias == "center" ~ "Least Biased",
    bias == "right-center" ~ "Right-Center",
    bias == "right" ~ "Right",
    bias == "satire" ~ "Satire",
    bias == "pro-science" ~ "Pro-Science",
    bias == "conspiracy" ~ "Conspiracy-Pseudoscience"
  )) %>%
  dplyr::select(domain, bias_score, Bias)
glimpse(mbfc_2016)

mbfc_database_no2016 = mbfc_database %>%
  filter(!domain %in% mbfc_2016$domain) %>%
  bind_rows(mbfc_2016) %>%
  distinct()
glimpse(mbfc_database_no2016)

all_graz_112_117_urls_month_mbfc_before2017 = all_graz_112_117_urls %>%
  filter(year <= 2016) %>%
  inner_join(mbfc_database_no2016 %>%
               dplyr::select(domain,Bias, bias_score), by="domain") %>%
  add_count(domain, PARTY, month, name ="scored_domain_count") %>%
  distinct()
glimpse(all_graz_112_117_urls_month_mbfc_before2017)

all_graz_112_117_urls_month_mbfc = all_graz_112_117_urls %>%
  filter(year > 2016) %>%
  inner_join(mbfc_database %>%
               dplyr::select(domain,Bias, bias_score), by="domain") %>%
  add_count(domain, PARTY, month, name ="scored_domain_count")%>%
  distinct()
glimpse(all_graz_112_117_urls_month_mbfc)

all_graz_112_117_urls_month_mbfc_merged = bind_rows(all_graz_112_117_urls_month_mbfc_before2017,
                                                    all_graz_112_117_urls_month_mbfc) %>%
  distinct() %>%
  arrange(desc(scored_domain_count))

all_graz_112_117_urls_month_mbfc_merged %>%
  mutate(month = lubridate::ymd(month)) %>%
  na.omit() %>%
  filter(PARTY != "I") %>%
  group_by(month, PARTY) %>%
  summarise(avg_score = weighted.mean(bias_score, scored_domain_count, na.rm=T)) %>%
  ggplot(aes(y=avg_score, x=month, color=PARTY)) +
  geom_line() +
  scale_color_manual(values=c("#3279E6", "#EA5558")) +
  theme_minimal()

