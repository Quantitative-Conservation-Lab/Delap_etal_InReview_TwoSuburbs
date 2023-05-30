## *A tale of two suburbs: influence of new residential construction varying in housing density on bird species, human tolerance guilds, and communities*

#### Jack H DeLap, John M. Marzluff, and Sarah J Converse 

##### Please contact the first author for questions about the code or data: Jack DeLap (jdelap@cornish.edu)
##### Secondary contact: Sarah Converse (sconver@usgs.gov)

_______________________________________________________________________________________

## Abstract

Human population growth and changing settlement patterns fuel the development of urban fringe lands worldwide, with implications for biodiversity. We conducted a 12-year study of birds in the fast-developing urban fringe lands of the Central Puget Sound region, Washington, USA, to examine the effect of development configuration on birds, including human-tolerance guilds and individual species. We hypothesized that lower-intensity conservation developments (CDs), compared to higher-intensity planned community developments (PCDs), would benefit the overall bird community, as well as native forest birds (avoiders of human development) and avian generalist species (adapters to human development), but that PCDs would benefit synanthropic species (exploiters of human development). Consistent with these hypotheses, we observed a greater proportion of the overall community, avoiders, and adapters in CDs, while we observed a greater proportion of exploiters in PCDs. We also hypothesized that human tolerance guilds would be a useful but imperfect, predictor of individual species responses. This hypothesis was also supported: we found that, for avoiders, 94% of species in the guild had the same response as the overall guild; for adapters, 64%; and for exploiters, 56%. Our results indicate that the configuration of suburban developments can have a meaningful impact on bird communities.

### Table of Contents 

Folders include scripts, data, results, and figures. See files listed below. 

### [Scripts](./scripts)

analysis.R

plotting_code_spp.R

plotting_code_guilds.R
 
### [Data](./data) 

Bird_Data.csv

### [Results](./results)

spp.1.57.v2.RData

allSpp_AIC.RData

allBoots_15April.RData


### [Figures](./figures)

Adaptor_Figure.png

Avoider_Figure.png

Exploiter_Figure.png

Guilds_Figure.png

### Required Packages and Versions Used 

here (v1.0.1)

tidyr (v1.3.0)

lme4 (v1.1.33)

stringr (v1.5.0)

ggplot2 (v3.4.2)

ggpub4 (v0.6.0)

viridis (v0.6.3)


### Details of Article 

DeLap JH, JM Marzluff, and SJ Converse. In Review. A tale of two suburbs: influence of new residential construction varying in housing density on bird species, human tolerance guilds, and communities. 

### How to Use this Repository 

Run analysis code first, which will create several data objects that can be subsequently run to create figures. Run time for all analyses is >24 hours. 