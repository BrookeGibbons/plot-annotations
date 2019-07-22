setwd("C:/GitHub/plot-annotations/Example data")
dir()

library(fst)
library(dplyr)
library(googlesheets)
library(GlobalArchive)


# Life history ----
life.history <- gs_title("Australia.life.history")%>%
  gs_read(ws = "australia.life.history")%>%
  mutate(RLS.trophic.group=tolower(RLS.trophic.group))%>%
  mutate(Commercial=str_detect(Fishing.type,"C"))%>%
  mutate(Recreational=str_detect(Fishing.type,"R"))%>%
  mutate(Bycatch=str_detect(Fishing.type,"B"))%>%
  dplyr::select(Family,Genus,Species,RLS.trophic.group,Fishing.type,Commercial,Recreational,Bycatch)%>%
  #dplyr::rename('Trophic group'=RLS.trophic.group,'Target group'=Fishing.type)%>%
  ga.clean.names()


test<-life.history%>%
  select(fishing.type)%>%
  dplyr::mutate(target.group=str_replace_all(.$fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch")))%>%
  tidyr::replace_na(list(target.group="Non-target"))%>%
  mutate(target.group = factor(target.group, levels = c("Commercial","Commercial/Recreational","Recreational","Bycatch","Non-target")))

unique(test$target.group)

str(test$target.group)

# MAXN
maxn<-read_fst("complete.maxn.fst")%>%
  glimpse()


maxn.data <- read_fst("complete.maxn.fst")%>%
  as.data.frame()%>%
  filter(maxn>0)%>%
  dplyr::rename(Family=family,Genus=genus,Species=species)%>%
  left_join(.,life.history)%>%
  dplyr::rename(target.group=Fishing.type)%>%
  dplyr::mutate(target.group=str_replace_all(.$target.group,c("R"="Recreation","C"="Commercial","B/"="","B"=NA)))%>%
  tidyr::replace_na(list(target.group="Non-target"))%>%
  group_by(target.group)%>%
  dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
  ungroup()%>%
  arrange(-Total.abundance)%>%
  dplyr::rename('Total abundance'=Total.abundance,'Number of samples'=Number.of.samples,'Target group'=target.group)%>%
  glimpse()



summarised.maxn<-maxn%>%
  filter(maxn>0)%>%
  left_join(life.history)%>%
  dplyr::mutate(target.group=str_replace_all(.$fishing.type,c("R"="Recreation","C"="Commercial","B/"="","B"=NA)))%>%
  dplyr::rename(trophic.group=rls.trophic.group)%>%
  tidyr::replace_na(list(target.group="Non-target",trophic.group="no trophic group"))%>%
  dplyr::group_by(target.group)%>%
  dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  left_join(life.history)
  
  unique(summarised.maxn$target.group)

overall.total<-sum(summarised.maxn$total.abundance)
species.richness<-length(unique(maxn$scientific))

# LENGTH
length<-read_fst("complete.length.fst")%>%
  glimpse()

summarised.length<-length%>%
  filter(length>0)%>%
  dplyr::group_by(family,genus,species)%>%
  dplyr::summarise(total.measured=sum(number),number.of.samples=length(unique(id)),mean.length=mean(length),min.length=min(length),max.length=max(length))%>%
  ungroup()%>%
  arrange(-total.measured)

# Create assemblage data frame
names(maxn)

rec.target<-maxn%>%
  #filter(maxn>0)%>%
  left_join(life.history)%>%
  filter(recreational==TRUE)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Abundance by target group")%>%
  mutate(level="Recreational")
  
com.target<-maxn%>%
  #filter(maxn>0)%>%
  left_join(life.history)%>%
  filter(commercial==TRUE)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Abundance by target group")%>%
  mutate(level="Commercial")

com.target<-maxn%>%
  #filter(maxn>0)%>%
  left_join(life.history)%>%
  filter(commercial==TRUE)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Abundance by target group")%>%
  mutate(level="Commercial")

bycatch.target<-maxn%>%
  #filter(maxn>0)%>%
  left_join(life.history)%>%
  filter(bycatch==TRUE)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Abundance by target group")%>%
  mutate(level="Bycatch")


unique(life.history$trophic.group)

trophic<-maxn%>%
  #filter(maxn>0)%>%
  left_join(life.history)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude,trophic.group)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  replace_na(list(trophic.group="missing trophic group"))%>%
  mutate(indicator="Abundance by trophic group")%>%
  dplyr::rename(level=trophic.group)
  
total.abundance<-maxn%>%
  left_join(life.history)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Total abundance")

species.richness<-maxn%>%
  filter(maxn>0)%>%
  left_join(life.history)%>%
  dplyr::group_by(campaignid,sample,latitude,longitude)%>%
  dplyr::summarise(total.abundance=length(unique(scientific)))%>%
  ungroup()%>%
  arrange(-total.abundance)%>%
  mutate(indicator="Species richness")

assemblage<-bind_rows(rec.target, com.target, bycatch.target, trophic, total.abundance, species.richness)
  