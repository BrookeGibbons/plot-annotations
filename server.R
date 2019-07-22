
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
      as.data.frame()
    
    options <- maxn.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    
    updateSelectInput(session, "campaignid.selector", choices = options, selected = "All")
    updateSelectInput(session, "assemblage.campaignid.selector", choices = options, selected = "All")
    updateSelectInput(session, "metrics.maxn.campaignid.selector", choices = options, selected = "All")

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
    updateSelectInput(session, "length.metric.campaignid.selector", choices = options, selected = "All")
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
      dplyr::select(Family,Genus,Species,RLS.trophic.group,Fishing.type,Commercial,Recreational,Bycatch)
    
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
  req(input$assemblage.campaignid.selector)

  maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
    as.data.frame()
  
  if (input$assemblage.campaignid.selector == "All") {
    maxn.data

  } else {
    campaign.name <- input$assemblage.campaignid.selector
    filter(maxn.data, campaignid == campaign.name)
  }
})

assemblage_metric_data <- reactive({
maxn <- assemblage_maxn_data()%>%
  as.data.frame()

total.abundance<-maxn%>%
  dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  mutate(metric="Total abundance")

species.richness<-maxn%>%
  filter(maxn>0)%>%
  dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
  dplyr::summarise(total.abundance=length(unique(scientific)))%>%
  ungroup()%>%
  mutate(metric="Species richness")

assemblage <- bind_rows(total.abundance, species.richness)
assemblage

})

# Create MaxN metric reactive data frame ----
metric_maxn_data <- reactive({
  req(input$metrics.maxn.campaignid.selector)
  
  maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
    as.data.frame()
  
  if (input$metrics.maxn.campaignid.selector == "All") {
    maxn.data
    
  } else {
    campaign.name <- input$metrics.maxn.campaignid.selector
    filter(maxn.data, campaignid == campaign.name)
    maxn.data
  }
})

maxn_metric_data <- reactive({
  maxn <- metric_maxn_data()%>%
    as.data.frame()
  
  life.history<-life.history()
  
  trophic<-maxn%>%
    dplyr::rename(Family=family,Genus=genus,Species=species)%>%
    left_join(.,life.history)%>%
    dplyr::rename(trophic.group=RLS.trophic.group)%>%
    dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude,trophic.group)%>%
    dplyr::summarise(total.abundance=sum(maxn))%>%
    ungroup()%>%
    replace_na(list(trophic.group="missing trophic group"))%>%
    dplyr::mutate(metric="Trophic group")%>%
    dplyr::rename(level=trophic.group)%>%
    glimpse()
  
  target<-maxn%>%
    dplyr::rename(Family=family,Genus=genus,Species=species)%>%
    left_join(life.history)%>%
    dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch")))%>%
    tidyr::replace_na(list(target.group="Non-target"))%>%
    dplyr::mutate(target.group=str_replace_all(.$target.group,c("Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
    mutate(target.group = factor(target.group, levels = c("Target","Bycatch","Non-target")))%>%
    mutate(target.group = fct_relevel(target.group, "Target","Bycatch","Non-target")) %>%
    dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude,target.group)%>%
    dplyr::summarise(total.abundance=sum(maxn))%>%
    ungroup()%>%
    dplyr::mutate(metric="Target group")%>%
    dplyr::rename(level=target.group)%>%
    glimpse()
  
  metric.dat <- bind_rows(trophic, target)
  metric.dat
  
})

# Create Length metric reactive data frame ----
metric_length <- reactive({
  req(input$length.metric.campaignid.selector)
  
  length.data <- fst::read_fst(input$complete.length$datapath)%>%
    as.data.frame()
  
  if (input$length.metric.campaignid.selector == "All") {
    length.data
    
  } else {
    campaign.name <- input$length.metric.campaignid.selector
    filter(length.data, campaignid == campaign.name)
    length.data
  }
})


metric_length_data <- reactive({
  length <- metric_length()%>%
    as.data.frame()
  
  life.history<-life.history()
  
  trophic<-length%>%
    dplyr::rename(Family=family,Genus=genus,Species=species)%>%
    left_join(.,life.history)%>%
    dplyr::rename(trophic.group=RLS.trophic.group)%>%
    replace_na(list(trophic.group="missing trophic group"))%>%
    dplyr::mutate(metric="Trophic group")%>%
    dplyr::rename(level=trophic.group)%>%
    glimpse()
  
  target<-length%>%
    dplyr::rename(Family=family,Genus=genus,Species=species)%>%
    left_join(life.history)%>%
    dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch")))%>%
    tidyr::replace_na(list(target.group="Non-target"))%>%
    dplyr::mutate(target.group=str_replace_all(.$target.group,c("Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
    mutate(target.group = factor(target.group, levels = c("Target","Bycatch","Non-target")))%>%
    mutate(target.group = fct_relevel(target.group, "Target","Bycatch","Non-target")) %>%
    dplyr::mutate(metric="Target group")%>%
    dplyr::rename(level=target.group)%>%
    glimpse()
  
  metric.dat <- bind_rows(trophic, target)
  metric.dat
  
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
    
    maxn.per.sample<-trends_data()%>%
      group_by(campaignid,sample,status)%>%
      summarise(maxn=sum(maxn))
    
    ggplot(maxn.per.sample, aes(x = status,y=maxn, fill = status)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle("Plot of abundance by Status")
  })
  
  # maxn Location ----
  
  output$location.plot <- renderPlot({
    
    maxn.per.sample<-trends_data()%>%
      group_by(campaignid,sample,location)%>%
      summarise(maxn=sum(maxn))
    
    ggplot(maxn.per.sample, aes(x = location,y=maxn)) + 
      stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle("Plot of abundance by Location")
    
  })
  
  # Maxn Site ----
  output$site.plot <- renderPlot({
    maxn.per.sample<-trends_data()%>%
      group_by(campaignid,sample,site)%>%
      summarise(maxn=sum(maxn))
    
    ggplot(maxn.per.sample, aes(x = site,y=maxn)) + 
      stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle("Plot of abundance by Site")
  })
  

# # make drop down for metrics
# observe({
#   req(input$complete.maxn)
#   # read in fst data
#   maxn.data <- assemblage_metric_data()%>%
#     as.data.frame()
#   
#   options <- maxn.data %>%
#     dplyr::distinct(metric) %>% 
#     pull("metric")%>%
#     sort()
#   
#   updateSelectInput(session, "assemblage.maxn.metric.selector", choices = options, selected = "Total abundance")
#   
# })


# Maxn assemblage Status ----
output$assemblage.maxn.status.plot <- renderPlot({
  
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$assemblage.maxn.metric.selector)%>%
    glimpse()
  
  ggplot(maxn.per.sample, aes(x = status,y=total.abundance, fill = status)) + 
    stat_summary(fun.y=mean, geom="bar",colour="black") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("")+
    ylab("Per stereo-BRUV \n(+/- SE)")+
    theme_bw()+
    Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Plot of abundance by Status")
})

# maxn Location ----

output$assemblage.maxn.location.plot <- renderPlot({
  
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$assemblage.maxn.metric.selector)%>%
    glimpse()
  
  ggplot(maxn.per.sample, aes(x = location,y=total.abundance)) + 
    stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("")+
    ylab("Per stereo-BRUV \n(+/- SE)")+
    theme_bw()+
    Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Plot of abundance by Location")
  
})

# Maxn Site ----
output$assemblage.maxn.site.plot <- renderPlot({
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$assemblage.maxn.metric.selector)%>%
    glimpse()
  
  ggplot(maxn.per.sample, aes(x = site,y=total.abundance)) + 
    stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
    stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
    geom_hline(aes(yintercept=0))+
    xlab("")+
    ylab("Per stereo-BRUV \n(+/- SE)")+
    theme_bw()+
    Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
    ggtitle("Plot of abundance by Site")
})

# Maxn metric Status ----
output$metrics.maxn.status.plot <- renderPlot({
  
  maxn.per.sample<-maxn_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$metrics.maxn.metric.selector)%>%
    glimpse()
  
  posn.d <- position_dodge(0.9)
    
    ggplot(maxn.per.sample, aes(x = level, y=total.abundance, fill=status, group=status)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      Theme1+theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle("Plot of abundance by Status")
})

# length metric Status ----
output$length.metric.status <- renderPlot({
  
  length.metric<-metric_length_data()%>%
    as.data.frame()%>%
    filter(metric == input$length.metric.selector)%>%
    glimpse()
  
  posn.d <- position_dodge(0.9)
  

  ggplot(length.metric,aes(x = level, y = length, fill=status, notch=FALSE, outlier.shape = NA)) + 
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, position=posn.d)+ #this is adding the dot for the mean
    theme_bw()+
    xlab("") + ylab("Length (mm)") +
    ggtitle("Plot of length by Status") +
    theme_bw()+
    Theme1+
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))
    
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
  
  
  # Maxn Assemblage Spatial plot ----
  # Create spatial plot
  output$assemblage.maxn.spatial.plot <- renderLeaflet({
    
    data<-assemblage_metric_data()%>%
      as.data.frame()%>%
      filter(metric == input$assemblage.maxn.metric.selector)%>%
      glimpse()
    
    map <- leaflet(data) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    overzero <- filter(data, total.abundance > 0)
    equalzero <- filter(data, total.abundance == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((total.abundance/max(total.abundance))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(total.abundance)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(total.abundance)
        )
    }
    map
  })
  
  
  
# Length plots ----

  output$length.status.plot <- renderPlot({
    
  ggplot(length_trends_data(),aes(x = factor(status), y = length,  fill = status, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
    theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
    theme_bw()+
    Theme1+
    xlab("Status") + ylab("Length (mm)") +
    theme_bw() +
    Theme1
  })
  
## Habitat spatial plot ----
  output$habitat.spatial.plot <- renderLeaflet({
    
    data<-hab_campaignid_data()[,c("macroalgae","stony.corals","octocoral.black","sponges","hydroids","consolidated","unconsolidated")]
    
    names(data)<-ga.capitalise(names(data))
    names(data)<-str_replace_all(names(data),c("[^[:alnum:]]"=" "))
    
    colors <- c("#4eb570","#d94c45","#78807a","#d99445","#d67cc9","#bd6539","#faef52")
    
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
      select(campaignid,sample,longitude,latitude,consolidated,hydroids,macroalgae,octocoral.black,sponges,stony.corals,unconsolidated) # "biota.ascidians", "biota.crinoids", "biota.invertebrate.complex","biota.seagrasses",
    # Gather habitat to bubble plot easier
    habitat<-gather(habitat.lite,"macroalgae","stony.corals","octocoral.black","sponges","hydroids","consolidated","unconsolidated",key="habitat.type",value="percent.cover")
    
    habitat<-habitat%>%
      mutate(habitat.type=ga.capitalise(habitat.type))%>%
      mutate(habitat.type=str_replace_all(.$habitat.type, c("[^[:alnum:]]"=" ")))%>%
      glimpse()
    
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
  
  # Maxn data ----
  maxn.summary.data <- reactive({
    life.history<-life.history()
    
    # maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
    #   dplyr::mutate(key="maxn")%>%
    #   as.data.frame()%>%
    #   filter(maxn>0)%>%
    #   ## scientific, trophic, target
    #   dplyr::group_by(family,genus,species)%>%
    #   dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
    #   ungroup()%>%
    #   arrange(-Total.abundance)%>%
    #   dplyr::rename(Family=family,Genus=genus,Species=species,'Total abundance'=Total.abundance,'Number of samples'=Number.of.samples)%>%
    #   left_join(.,life.history)
  
  if (input$maxn.summary.groupby=="Species") {
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      dplyr::group_by(family,genus,species)%>%
      dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-Total.abundance)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species,'Total abundance'=Total.abundance,'Number of samples'=Number.of.samples)%>%
      left_join(.,life.history)%>%
      dplyr::rename('Trophic group'=RLS.trophic.group,'Target group'="Fishing.type")
  }
    
  if (input$maxn.summary.groupby=="Target group") {
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species)%>%
      left_join(.,life.history)%>%
      #dplyr::rename(target.group=Fishing.type)%>%
      dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch")))%>%
      tidyr::replace_na(list(target.group="Non-target"))%>%
      dplyr::mutate(target.group=str_replace_all(.$target.group,c("Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
      dplyr::group_by(target.group)%>%
      dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-Total.abundance)%>%
      dplyr::rename('Total abundance'=Total.abundance,'Number of samples'=Number.of.samples,'Target group'=target.group)%>%
      glimpse()
  }
    
  if (input$maxn.summary.groupby=="Trophic group") {
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species)%>%
      left_join(.,life.history)%>%
      dplyr::rename(trophic.group=RLS.trophic.group)%>%
      tidyr::replace_na(list(trophic.group="no trophic group"))%>%
      dplyr::group_by(trophic.group)%>%
      dplyr::summarise(Total.abundance=sum(maxn),Number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-Total.abundance)%>%
      dplyr::rename('Total abundance'=Total.abundance,'Number of samples'=Number.of.samples,'Trophic group'=trophic.group)
  }
    
    maxn.data
  })
    
  output$maxn.summary <- DT::renderDataTable(
    DT::datatable(maxn.summary.data(), options = list( # [, input$show_vars, drop = FALSE]
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
    paste("Overall species richness:",length(unique(maxn.summary()$scientific)))
  })
  
  
  # Length data ----
  length.summary.data <- reactive({
    life.history<-life.history()
    
    if (input$length.summary.groupby=="Species") {
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
        left_join(.,life.history)%>%
        dplyr::rename('Trophic group'=RLS.trophic.group,"Target group"=Fishing.type)
    
    }
    
    if (input$length.summary.groupby=="Target group") {
      length.data <- fst::read_fst(input$complete.length$datapath)%>%
        as.data.frame()%>%
        filter(length>0)%>%
        dplyr::rename(Family=family,Genus=genus,Species=species)%>%
        left_join(life.history)%>%
        dplyr::rename(target.group=Fishing.type)%>%
        dplyr::mutate(target.group=str_replace_all(.$target.group,c("R"="Recreational","C"="Commercial","B/"="","B"=NA)))%>%
        dplyr::mutate(target.group=str_replace_all(.$target.group,c("Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
        tidyr::replace_na(list(target.group="Non-target"))%>%
        dplyr::group_by(target.group)%>%
        dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
        mutate(Mean.length=round(Mean.length, digits=2))%>%
        mutate(Min.length=round(Min.length, digits=2))%>%
        mutate(Max.length=round(Max.length, digits=2))%>%
        ungroup()%>%
        arrange(-Total.measured)%>%
        dplyr::rename('Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length,'Target group'=target.group)
    }
    
    if (input$length.summary.groupby=="Trophic group") {
      length.data <- fst::read_fst(input$complete.length$datapath)%>%
        as.data.frame()%>%
        filter(length>0)%>%
        dplyr::rename(Family=family,Genus=genus,Species=species)%>%
        left_join(life.history)%>%
        dplyr::rename(trophic.group=RLS.trophic.group)%>%
        tidyr::replace_na(list(trophic.group="No trophic group"))%>%
        dplyr::group_by(trophic.group)%>%
        dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
        mutate(Mean.length=round(Mean.length, digits=2))%>%
        mutate(Min.length=round(Min.length, digits=2))%>%
        mutate(Max.length=round(Max.length, digits=2))%>%
        ungroup()%>%
        arrange(-Total.measured)%>%
        dplyr::rename('Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length, 'Trophic group'=trophic.group)
      
    }
    length.data 
  })
  
  output$length.summary <- DT::renderDataTable(
    DT::datatable(length.summary.data(), options = list( #
      lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
      pageLength = 15, rownames= FALSE,digits=2)
    )
  )

  
}