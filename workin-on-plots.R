tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(width = "100%", height = "400px") %>%
  addTiles(tilesURL)

data("eco2mix")

prod2016 <- eco2mix %>%
  mutate(
    renewable = bioenergy + solar + wind + hydraulic,
    non_renewable = total - bioenergy - solar - wind - hydraulic
  ) %>%
  filter(grepl("2016", month) & area != "France") %>%
  select(-month) %>%
  group_by(area, lat, lng) %>%
  summarise_all(sum) %>%
  ungroup()


library(leaflet.minicharts)
colors <- c("#4fc13c", "#cccccc")

dir()
habitat<-read_fst("complete.habitat.fst")%>%
  glimpse()

names(habitat)

biota.name<-habitat%>%select(starts_with("biota"))%>%as.vector()

colors <- c("#66C5CC","#F6CF71","#F89C74","#DCB0F2","#87C55F","#9EB9F3","#FE88B1","#C9DB74","#8BE0A4","#B497E7","#D3B484")

#colors <- c("#4fc13c", "#cccccc")

basemap %>%
  addMinicharts(
    habitat$longitude, habitat$latitude,
    type = "pie",
    chartdata = habitat[, c("biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated")], 
    colorPalette = colors, 
    width = 20, transitionTime = 0
  )



basemap %>%
  addMinicharts(
    habitat$longitude, habitat$latitude,
    type = "pie",
    chartdata = prod2016[, c("biota.ascidians ", "biota.bryozoa","biota.consolidated","")], 
    colorPalette = colors, 
    width = 60 * sqrt(prod2016$total) / sqrt(max(prod2016$total)), transitionTime = 0
  )

habitat.lite<-habitat%>%
  select(campaignid,sample,"biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated")

# Gather habitat to bubble plot easier
test<-gather(habitat.lite,"biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated",key="habitat.type",value="percent.cover")

names(maxn)


## mean +/-se plots
ggplot(maxn, aes(x=status, y=maxn, group=status, color=status)) +  #, group=Location, color=Location
  stat_summary(fun.y=mean, geom="line", size=1) +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1)+
  #scale_color_manual(values = c("#00AFBB","#E7B800","red"))+
  theme_bw()+Theme1+ 
  xlab("Month")+
  ylab("Average Lobster per pot (+/- SE)")


maxn.per.sample<-maxn%>%
  group_by(campaignid,sample,status)%>%
  summarise(maxn=sum(maxn))

ggplot(maxn.per.sample, aes(x = status,y=maxn)) + 
  stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Average abundance per drop (+/- SE)")+
  theme_bw()+
  Theme1+theme(panel.grid = element_blank(), panel.border = element_blank())
