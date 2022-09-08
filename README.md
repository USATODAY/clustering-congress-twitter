# How Congress has become more polarized on Twitter

A linguistic analysis of Congress Twitter 2011-2022 using hierarchical clustering in R. 

Analysis by Aleszu Bajak, Data Team

## Project goal

*A USA TODAY analysis of official Twitter accounts from members of Congress posted during their time in Congress. This analysis uses agglomerative hierarchical clustering in R to find linguistic overlaps and polarization in how members of Congress use Twitter. The analysis includes all words appearing more than five times in tweets from the official accounts of members of the 112th through 117th Congress.*

## Project notes

### Reporters involved

*Aleszu Bajak: abajak@usatoday.com*

### Data sources

*ProPublica for Congress Twitter data.*

*Andy Ballard et al.'s 2022 study "[Dynamics of Polarizing Rhetoric in Congressional Tweets](https://onlinelibrary.wiley.com/doi/10.1111/lsq.12374)" for list of official congressional Twitter accounts.*

*Affective/emotional lexicon from Almog Simchon et al.'s 2022 study "[Troll and divide: the language of online polarization](https://academic.oup.com/pnasnexus/article/1/1/pgac019/6546199?login=false)."*

*Jana Lasser and David Garcia, Technical University of Graz, Austria for Congress Twitter dataset with expanded URLs and total interactions.*


## Technical

```
.
├── README.md
├── analysis
│   └── hclust_of_congress.R
├── data
│   ├── processed
│   ├──── fit_2011_2012_propublica.RDS 
│   ├──── fit_2013_2014_propublica.RDS 
│   ├──── fit_2015_2016_propublica.RDS 
│   ├──── fit_2017_2018_propublica.RDS 
│   ├──── fit_2019_2020_propublica.RDS 
│   ├──── fit_2021_2022_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2011_2012_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2013_2014_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2015_2016_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2017_2018_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2019_2020_propublica.RDS 
│   ├──── top_words_by_hierar_cluster_2021_2022_propublica.RDS 
│   ├── source
│   ├──── cong_handles_master_and_ids08082022.csv
│   ├──── affective_lexicon.csv
│   └──── labMT.csv
├── scratch
│   ├── all_clusters_numbers_annotated.png
│   ├── hope_vs_fight_by_party_overtime.png
│   ├── polarizing_language_by_party_growing_overtime.png
│   └── monthly_avg_interactions_vs_polarizing_tweets.png
└── viz
    ├── increase_in_emotional_language_overtime_Infogram.png
    ├── 2011_2012.pdf
    ├── 2013_2014.pdf
    ├── 2015_2016.pdf
    ├── 2017_2018.pdf
    ├── 2019_2020.pdf
    ├── 2021_2022.pdf
    ├── fit_2011_2012_FAN_NAMES.pdf
    ├── fit_2013_2014_FAN_NAMES.pdf
    ├── fit_2015_2016_FAN_NAMES.pdf
    |── fit_2017_2018_FAN_NAMES.pdf    
    ├── fit_2019_2020_FAN_NAMES.pdf
    └── fit_2021_2022_FAN_NAMES.pdf
```

### Project setup instructions

All analysis is in the hclust_of_congress.R file


## Data notes and more reading

*Twitter data courtesy of ProPublica via Derek Willis.*

*Official congressional Twitter accounts from Ballard et al.'s 2022 study "[Dynamics of Polarizing Rhetoric in Congressional Tweets](https://onlinelibrary.wiley.com/doi/10.1111/lsq.12374)."*

*Emotional lexicon from Simchon et al.'s 2022 study "[Troll and divide: the language of online polarization](https://academic.oup.com/pnasnexus/article/1/1/pgac019/6546199?login=false)."*

*https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0123507*

*https://scholar.harvard.edu/files/shapiro/files/politext.pdf*

*https://www.tandfonline.com/doi/abs/10.1080/19331681.2019.1651685*

*https://en.wikipedia.org/wiki/NOMINATE_(scaling_method)*

*https://www.pewresearch.org/wp-content/uploads/sites/4/2014/06/6-12-2014-Political-Polarization-Release.pdf*

*https://www.pewresearch.org/politics/2022/08/09/as-partisan-hostility-grows-signs-of-frustration-with-the-two-party-system/*

*https://scholars.ttu.edu/en/publications/elite-polarization-party-extremity-and-affective-polarization*
