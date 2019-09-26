# Make North America map
# Steven Moran <steven.moran@uzh.ch>

library(dplyr)
library(ggplot2)
library(maps)

# phoible
phoible <- read.csv('https://raw.githubusercontent.com/phoible/dev/refactor-agg/data/phoible-by-phoneme.csv', header=T, stringsAsFactors=F)

# glottolog
glottolog <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-E7DE-FA06-8817-0/languages_and_dialects_geo.csv', header=T, stringsAsFactors=F)

# merge
phoible <- left_join(phoible, glottolog, by=c("Glottocode"="glottocode"))
glimpse(phoible)

# segments
segments <- read.csv('GRs_PHOIBLE.csv', header=T, stringsAsFactors = F)

# subset
temp <- phoible %>% filter(macroarea=="North America") %>% select(InventoryID, Glottocode, LanguageName, Phoneme, Marginal, latitude, longitude)
temp <- temp %>% filter(Phoneme %in% segments$Phoneme)

# merge
temp <- left_join(temp, segments)
# plot phoneme types
ggplot(data=temp, aes(x=longitude,y=latitude, fill=GlottalizedPhoneme, color=GlottalizedPhoneme)) + 
  borders("world", xlim = c(-130, -60), ylim = c(20, 50)) + 
  geom_point(size=5)

# get languages
langs <- temp %>% select(LanguageName, latitude, longitude) %>% distinct()
# plot langs
ggplot(data=langs, aes(x=longitude,y=latitude)) + 
  borders("world", xlim = c(-130, -60), ylim = c(20, 50)) + 
  geom_point(size=5)

# languages with and without a glottalized phoneme
temp <- phoible %>% filter(macroarea=="North America") %>% select(InventoryID, Glottocode, LanguageName, macroarea, Phoneme, Marginal, latitude, longitude)
table(temp$macroarea)
temp <- left_join(temp, segments)
temp <- temp %>% group_by(InventoryID, Glottocode, LanguageName, latitude, longitude) %>% summarize(GlottalizedPhonemes = any(!is.na(GlottalizedPhoneme)))
# filter out lat < 15
temp <- temp %>% filter(latitude>15)
# plot the langs
ggplot(data=temp, aes(x=longitude,y=latitude, fill=GlottalizedPhonemes, color=GlottalizedPhonemes)) + 
  borders("world", xlim = c(-180, -60), ylim = c(20, 80)) + 
  geom_point(size=5, aes(alpha=GlottalizedPhonemes))
glimpse(temp)
