
function(input, output, session) {
  
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste("example-plotting-script", "R", sep=".")
      },
      
      content <- function(file) {
        file.copy("example-plotting-script.R", file)
      },
      contentType = "R File/R"
    )
  
  # Populate the CampaignID dropdown when the app loads
  ## Read in data and create campaign id drop downs----
  observe({
    
    req(input$complete.maxn)
    # read in fst data
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      dplyr::mutate(value=maxn)%>%
      as.data.frame()
    
    options <- maxn.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "campaignid.selector", choices = options, selected = "All")
    updateSelectInput(session, "assemblage.maxn.campaignid.selector", choices = options, selected = "All")
  })
  
  
  observe({
    req(input$complete.length)
    # read in fst data
    length.data<-fst::read_fst(input$complete.length$datapath)%>%
      dplyr::mutate(key="length")%>%
      dplyr::mutate(value=length)%>%
      as.data.frame()
    
    options <- length.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "length.campaignid.selector", choices = options, selected = "All")
  })
  
  observe({
      req(input$complete.mass)
    mass.data<-fst::read_fst(input$complete.mass$datapath)%>%
      dplyr::mutate(key="mass")%>%
      dplyr::mutate(value=mass.g)%>%
      as.data.frame()
    
    options <- mass.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "mass.campaignid.selector", choices = options, selected = "All")
  })
  
  observe({
    req(input$complete.habitat)
    hab.data<-fst::read_fst(input$complete.habitat$datapath)%>%
      as.data.frame()
    
    options <- hab.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "habitat.campaignid.selector", choices = options, selected = "All")
  })
  
  # Read in life history google sheet
life.history<-  reactive({
    req(input$worksheet.name, input$sheet.name)
    
    life.history <- gs_title(input$worksheet.name)%>%
      gs_read(ws = input$sheet.name)%>%
      mutate(RLS.trophic.group=tolower(RLS.trophic.group))%>%
      mutate(Commercial=str_detect(Fishing.type,"C"))%>%
      mutate(Recreational=str_detect(Fishing.type,"R"))%>%
      mutate(Bycatch=str_detect(Fishing.type,"B"))%>%
      dplyr::select(Family,Genus,Species,RLS.trophic.group,Fishing.type,Commercial,Recreational,Bycatch)%>%
      dplyr::rename('Trophic group'=RLS.trophic.group,'Target group'=Fishing.type)
    
    life.history
    
  })
  
  # Create a dropdown function -----
  create_dropdown <- function(input_name, choices, label) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]] 
    } else {
      selected <- choices[1]
    }
    
    selectInput(
      inputId = input_name, 
      label = label,
      choices = choices,
      selected = selected
    )
  }
  
  # Create MaxN reactive data frame ----
  campaignid_data <- reactive({
    req(input$campaignid.selector)
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      as.data.frame()
    
    if (input$campaignid.selector == "All") {
      maxn.data
      
    } else {
      campaign.name <- input$campaignid.selector
      filter(maxn.data, campaignid == campaign.name)
    }
  })
  
# Create MaxN assemblage reactive data frame ----
assemblage_maxn_data <- reactive({
  req(input$assemblage.maxn.campaignid.selector)
  
  maxn <- fst::read_fst(input$complete.maxn$datapath)%>%
    as.data.frame()
  
  life.history<-life.history()
  
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
  
  assemblage <- bind_rows(rec.target, com.target, bycatch.target, trophic, total.abundance, species.richness)
  
  if (input$assemblage.maxn.campaignid.selector == "All") {
    assemblage
    
  } else {
    campaign.name <- input$assemblage.maxn.campaignid.selector
    filter(assemblage, campaignid == campaign.name)
  }
})


  # Create Length reactive data frame ----
  length_campaignid_data <- reactive({
    req(input$length.campaignid.selector)
    length.data <- fst::read_fst(input$complete.length$datapath)%>%
      dplyr::mutate(key="length")%>%
      as.data.frame()
    
    if (input$length.campaignid.selector == "All") {
      length.data
      
    } else {
      campaign.name <- input$length.campaignid.selector
      filter(length.data, campaignid == campaign.name)
    }
  })
  
  # Create Mass reactive data frame ----
  mass_campaignid_data <- reactive({
    req(input$mass.campaignid.selector)
    mass.data <- fst::read_fst(input$complete.mass$datapath)%>%
      as.data.frame()
    
    if (input$mass.campaignid.selector == "All") {
      mass.data
      
    } else {
      campaign.name <- input$mass.campaignid.selector
      filter(mass.data, campaignid == campaign.name)
    }
  })
  
  # Create Habitat reactive data frame ----
  hab_campaignid_data <- reactive({
    req(input$habitat.campaignid.selector)
    hab.data <- fst::read_fst(input$complete.habitat$datapath)%>%
      as.data.frame()
    
    if (input$habitat.campaignid.selector == "All") {
      hab.data
      
    } else {
      campaign.name <- input$habitat.campaignid.selector
      filter(hab.data, campaignid == campaign.name)
    }
  })
  
  # Create family drop down for MaxN ----
  output$family.selector <- renderUI({

    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
    }
  })

  # Create family drop down for Length ----
  output$length.family.selector <- renderUI({
    
    if (input$length.campaignid.selector == "All") {
      df<-length_campaignid_data()
      
      options <- df %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("length.family.selector", options, "Family:")
      
    } else {
      
      df<-length_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(number>0)%>%
        filter(campaignid == input$length.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$length.campaignid.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("length.family.selector", options, "Family:")
    }
  })
  
  # Create family drop down for Mass ----
  output$mass.family.selector <- renderUI({
    
    if (input$mass.campaignid.selector == "All") {
      df<-mass_campaignid_data()
      
      options <- df %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("mass.family.selector", options, "Family:")
      
    } else {
      
      df<-mass_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(mass.g>0)%>%
        filter(campaignid == input$mass.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$mass.campaignid.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("mass.family.selector", options, "Family:")
    }
  })

  # Create genus drop down for MaxN ----
  output$genus.selector <- renderUI({
    req(input$family.selector)
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        filter(#key == input$key.selector,
               family == input$family.selector) %>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
        create_dropdown("genus.selector", options, "Genus:")
      
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        filter(family == input$family.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      
      create_dropdown("genus.selector", options, "Genus:")
    }
  })
  
  
  # Create genus drop down for Length ----
  output$length.genus.selector <- renderUI({
    req(input$length.family.selector)
    
    if (input$length.campaignid.selector == "All") {
      df<-length_campaignid_data()
      
      options <- df %>%
        filter(family == input$length.family.selector) %>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      create_dropdown("length.genus.selector", options, "Genus:")
      
      
    } else {
      
      df<-length_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(number>0)%>%
        filter(campaignid == input$length.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$length.campaignid.selector) %>%
        filter(family == input$length.family.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      
      create_dropdown("length.genus.selector", options, "Genus:")
    }
  })
  
  
  # Create genus drop down for Mass ----
  output$mass.genus.selector <- renderUI({
    req(input$mass.family.selector)
    
    if (input$mass.campaignid.selector == "All") {
      df<-mass_campaignid_data()
      
      options <- df %>%
        filter(family == input$mass.family.selector) %>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      create_dropdown("mass.genus.selector", options, "Genus:")
      
      
    } else {
      
      df<-mass_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(mass.g>0)%>%
        filter(campaignid == input$mass.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$mass.campaignid.selector) %>%
        filter(family == input$mass.family.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      
      create_dropdown("mass.genus.selector", options, "Genus:")
    }
  })

  # Create species drop down for MaxN ----
  output$species.selector <- renderUI({
    req(input$family.selector, input$genus.selector)
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        filter(
          #key == input$key.selector,
          family == input$family.selector,
          genus == input$genus.selector
        ) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("species.selector", options, "Species:")
      
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$campaignid.selector) %>%
        filter(family == input$family.selector,
          genus == input$genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("species.selector", options, "Species:")
    }
  })
  
  
  # Create species drop down for Length ----
  output$length.species.selector <- renderUI({
    req(input$length.family.selector, input$length.genus.selector)
    
    if (input$length.campaignid.selector == "All") {
      df<-length_campaignid_data()
      
      options <- df %>%
        filter(family == input$length.family.selector,
          genus == input$length.genus.selector) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("length.species.selector", options, "Species:")
      
      
    } else {
      
      df<-length_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(number>0)%>%
        filter(campaignid == input$length.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$length.campaignid.selector) %>%
        filter(family == input$length.family.selector,
               genus == input$length.genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("length.species.selector", options, "Species:")
    }
  })
  
  
  # Create species drop down for Mass ----
  output$mass.species.selector <- renderUI({
    req(input$mass.family.selector, input$mass.genus.selector)
    
    if (input$mass.campaignid.selector == "All") {
      df<-mass_campaignid_data()
      
      options <- df %>%
        filter(
          family == input$mass.family.selector,
          genus == input$mass.genus.selector) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("mass.species.selector", options, "Species:")
      
      
    } else {
      
      df<-mass_campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(mass.g>0)%>%
        filter(campaignid == input$mass.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$mass.campaignid.selector) %>%
        filter(family == input$mass.family.selector,
               genus == input$mass.genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("mass.species.selector", options, "Species:")
    }
  })
  
# Maxn data ----
  trends_data <- reactive({
    req(input$family.selector, input$genus.selector, input$species.selector)
    campaignid_data() %>%
      filter(
        family == input$family.selector,
        genus == input$genus.selector,
        species == input$species.selector
      )
  })
  
  # Length data ----
  length_trends_data <- reactive({
    req(input$length.family.selector, input$length.genus.selector, input$length.species.selector)
    length_campaignid_data() %>%
      filter(
        family == input$length.family.selector,
        genus == input$length.genus.selector,
        species == input$length.species.selector
      )
  })
  
  # Mass data ----
  mass_trends_data <- reactive({
    req(input$mass.family.selector, input$mass.genus.selector, input$mass.species.selector)
    mass_campaignid_data() %>%
      filter(family == input$mass.family.selector,
        genus == input$mass.genus.selector,
        species == input$mass.species.selector )
  })
  
  
  # Habitat data ----
  habitat_data <- reactive({
    hab_campaignid_data()
  })

  # Create scatterplot object the plotOutput function is expecting

  # Maxn Status ----
  output$status.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(status), y = maxn, colour = status, fill = status,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Status") + ylab("Abundance per stereo-BRUV") +
      theme_bw() +
      ggtitle("Plot of abundance by Status")
  })
  
  # maxn Location ----
  
  output$location.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(location), y = maxn, colour = location, fill = location,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Location") + ylab("Abundance per stereo-BRUV") +
      ggtitle("Plot of abundance by Location") +
      theme_bw() +
      Theme1
  })
  
  # Maxn Site ----
  output$site.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(site), y = maxn, colour = site, fill = site,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Site") + ylab("Abundance per stereo-BRUV") +
      ggtitle("Plot of abundance by Site")+
      theme_bw() +
      Theme1
  })
  
  # Length histogram ----
  output$length.histogram <- renderPlot({
    ggplot(length_trends_data(),aes(x = length,colour = status,fill=status))+
      geom_histogram(alpha=0.5, position="identity",binwidth=input$length.binwidth)+
      #geom_density(alpha=0.6)+
      xlab("Length (mm)") + ylab("Count") +
      theme_bw() +
      Theme1
  })
  
  # Mass histogram -----
  output$mass.histogram <- renderPlot({
    ggplot(mass_trends_data(),aes(x = mass.g,colour = status,fill=status))+
      geom_histogram(alpha=0.5, position="identity",binwidth=input$mass.binwidth)+
      #geom_density(alpha=0.6)+
      xlab("Mass (g)") + ylab("Count") +
      theme_bw() +
      Theme1
  })
  
  # Length vs. Range
  output$length.vs.range <- renderPlot({
    ggplot(length_trends_data(), aes(x = range,y = length)) +
    geom_point()+
    geom_smooth()+
      xlab("Range (mm)") + ylab("Length (mm)") +
      theme_bw() +
      Theme1
  })
  
  # Biomass vs. Length
  output$length.vs.mass <- renderPlot({
    ggplot(mass_trends_data(), aes(x = length,y = mass.g)) +
      geom_point()+
      geom_smooth()+
      xlab("Length (mm)") + ylab("Mass (g)") +
      theme_bw() +
      Theme1
  })

  # Maxn Spatial plot ----
  # Create spatial plot
  output$spatial.plot <- renderLeaflet({
    map <- leaflet(trends_data()) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))

    overzero <- filter(trends_data(), maxn > 0)
    equalzero <- filter(trends_data(), maxn == 0)
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((maxn/max(maxn))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    map
  })
  
## Habitat spatial plot ----
  output$habitat.spatial.plot <- renderLeaflet({
    
    #data <- hab_campaignid_data()[, input$habitat.types]
    
    data <- hab_campaignid_data()[, c("biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated")]
    
    basemap <- leaflet(width = "100%", height = "800px") %>%
      addTiles()
    
    basemap %>%
      addMinicharts(
        habitat_data()$longitude, habitat_data()$latitude,
        type = "pie",
        chartdata = data,
        colorPalette = colors,
        width = 20, transitionTime = 0
      )
    
    #basemap
  })
  
  # Habitat bubble plot
  
  output$habitat.bubble.plot <- renderLeaflet({
    req(input$habitat.type)
    
    habitat.lite<-hab_campaignid_data()%>%
      select(campaignid,sample,longitude,latitude,"biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated")
    # Gather habitat to bubble plot easier
    habitat<-gather(habitat.lite,"biota.ascidians","biota.consolidated","biota.crinoids","biota.hydroids","biota.invertebrate.complex","biota.macroalgae","biota.octocoral.black","biota.seagrasses","biota.sponges","biota.stony.corals","biota.unconsolidated",key="habitat.type",value="percent.cover")
    
    habitat.bubble<-habitat%>%
      filter(habitat.type==input$habitat.type)
    
    map <- leaflet(habitat.bubble) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    overzero <- habitat.bubble%>%
      filter(percent.cover > 0)
    
    equalzero <- habitat.bubble%>%
      filter(percent.cover == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((percent.cover/max(percent.cover))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(percent.cover)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(percent.cover)
        )
    }
    map
  })

# SUMMARYS -----
  output$maxn.summary.family.selector <- renderUI({
    req(input$complete.maxn)
    # read in fst data
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()
    
    options <- maxn.data %>%
      dplyr::distinct(family) %>% 
      pull("family")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "maxn.summary.family.selector", choices = options, selected = "All")
  })
  
  output$family.selector <- renderUI({
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
    }
  })
  
  
  
  
  
  
  
  # Maxn data ----
  maxn.summary.data <- reactive({
    life.history<-life.history()
    
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      dplyr::group_by(family,genus,species)%>%
      dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-Total.abundance)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species,'Total abundance'=Total.abundance,'Number of samples'=Number.of.samples)%>%
      left_join(.,life.history)
  })
  
  output$maxn.summary <- DT::renderDataTable(
    DT::datatable(maxn.summary.data()[, input$show_vars, drop = FALSE], options = list(
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
        pageLength = 15, rownames= FALSE
      )
    )
  )
  
  maxn.summary <- reactive({
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      as.data.frame()
  })
  
  output$overall.abundance <- renderText({ 
    paste("Overall total abundance:",sum(maxn.summary.data()$'Total abundance'))
  })
  
  output$species.richness <- renderText({ 
    paste("Overall species richness: <b>",length(unique(maxn.summary()$scientific)),"</b>")
  })
  
  
  # Length data ----
  length.summary.data <- reactive({
    life.history<-life.history()
    
    length.data <- fst::read_fst(input$complete.length$datapath)%>%
      as.data.frame()%>%
      filter(length>0)%>%
      dplyr::group_by(family,genus,species)%>%
      dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
      mutate(Mean.length=round(Mean.length, digits=2))%>%
      mutate(Min.length=round(Min.length, digits=2))%>%
      mutate(Max.length=round(Max.length, digits=2))%>%
      ungroup()%>%
      arrange(-Total.measured)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species)%>%
      dplyr::rename('Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length)%>%
      left_join(.,life.history)
  })
  
  output$length.summary <- DT::renderDataTable(
    DT::datatable(length.summary.data()[, input$length_show_vars, drop = FALSE], options = list( #
      lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
      pageLength = 15, rownames= FALSE,digits=2)
    )
  )

  
}