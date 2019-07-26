# 1. Read in data and create campaignID dropdowns for each data frame
# 2. Create reactive data frames for maxn, length, mass and habitat (filter to campaignid)
# 3. Update drop downs for Family, Genus and Species for Maxn and Length
# 4. Create data frames for species plots
# 5. Summary tables for length and maxn
# 6. Plots

function(input, output, session) {
## Dropdown function -----
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
  
######################### DATA #########################
### 1. Read in data and campaign id drop downs----
  # MaxN ----
  observe({
    
    req(input$complete.maxn)
    
    # Read in fst data
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()
    
    # Create list of campaigns
    options <- maxn.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    
    # Update selector
    updateSelectInput(session, "maxn.campaignid.selector", choices = options, selected = "All")
    updateSelectInput(session, "maxn.metric.campaignid.selector", choices = options, selected = "All")

  })
    
  # Length ----
  observe({
    req(input$complete.length)
    
    # Read in fst data
    length.data<-fst::read_fst(input$complete.length$datapath)%>%
      as.data.frame()
    
    # Create list of campaigns
    options <- length.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    
    # Update selector
    updateSelectInput(session, "length.campaignid.selector", choices = options, selected = "All")
    updateSelectInput(session, "length.metric.campaignid.selector", choices = options, selected = "All")
  })
  
  # Mass ----
  observe({
      req(input$complete.mass)
    
    # Read in fst data
    mass.data<-fst::read_fst(input$complete.mass$datapath)%>%
      dplyr::mutate(key="mass")%>%
      dplyr::mutate(value=mass.g)%>%
      as.data.frame()
    
    # Create list of campaigns
    options <- mass.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Update selector
    options <- c("All", options)
    updateSelectInput(session, "mass.campaignid.selector", choices = options, selected = "All")
  })
  
  # Habitat ----
  observe({
    req(input$complete.habitat)
    
    # Read in fst data
    hab.data<-fst::read_fst(input$complete.habitat$datapath)%>%
      as.data.frame()
    
    # Create list of campaigns
    options <- hab.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Update selector
    options <- c("All", options)
    updateSelectInput(session, "habitat.campaignid.selector", choices = options, selected = "All")
  })
  
### Read in life history google sheet ----
life.history<-  reactive({
  
    req(input$worksheet.name, input$sheet.name)
    
    life.history <- gs_title(input$worksheet.name)%>%
      gs_read(ws = input$sheet.name)%>%
      mutate(trophic.group=ga.capitalise(RLS.trophic.group))%>%
      dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch","Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
      dplyr::mutate(trophic.group=str_replace_all(.$trophic.group,c("NANA"="Missing trophic group","NA"="Missing trophic group")))%>%
      tidyr::replace_na(list(target.group="Non-target",trophic.group="Missing trophic group"))%>%
      dplyr::mutate(target.group = factor(target.group, levels = c("Target","Bycatch","Non-target")))%>%
      dplyr::mutate(target.group = fct_relevel(target.group, "Target","Bycatch","Non-target"))%>%
      dplyr::select(Family,Genus,Species,trophic.group,target.group)%>%
      ga.clean.names()
    
    life.history
    
  })
  
### 2. Reactive data frames ----
  # MaxN ----
  maxn_data <- reactive({
    req(input$maxn.campaignid.selector)
    
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()
    
    if (input$maxn.campaignid.selector == "All") {
      maxn.data
      
    } else {
      campaign.name <- input$maxn.campaignid.selector
      filter(maxn.data, campaignid == campaign.name)
    }
  })
  
  # MaxN metric ----
 maxn_metric_data <- reactive({
  req(input$maxn.metric.campaignid.selector)

  maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
    as.data.frame()
  
  if (input$maxn.metric.campaignid.selector == "All") {
    maxn.data

  } else {
    campaign.name <- input$maxn.metric.campaignid.selector
    filter(maxn.data, campaignid == campaign.name)
  }
  
total.abundance<-maxn.data%>%
  dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
  dplyr::summarise(total.abundance=sum(maxn))%>%
  ungroup()%>%
  mutate(metric="Total abundance")

species.richness<-maxn.data%>%
  filter(maxn>0)%>%
  dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
  dplyr::summarise(total.abundance=length(unique(scientific)))%>%
  ungroup()%>%
  mutate(metric="Species richness")

assemblage <- bind_rows(total.abundance, species.richness)
assemblage

})

# metric_data <- reactive({
#   req(input$metrics.campaignid.selector)
#   
#   maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
#     as.data.frame()
#   
#   if (input$metrics.campaignid.selector == "All") {
#     maxn.data
#     
#   } else {
#     campaign.name <- input$metrics.campaignid.selector
#     filter(maxn.data, campaignid == campaign.name)
#   }
# })
# 
# maxn_metric_data <- reactive({
#   maxn <- metric_data()%>%
#     as.data.frame()
#   
#   if (input$metrics.campaignid.selector == "All") {
#     maxn<-maxn
#     
#   } else {
#     campaign.name <- input$metrics.campaignid.selector
#     filter(maxn, campaignid == campaign.name)
#   }
#   
#   life.history<-life.history()
#   
#   trophic<-maxn%>%
#     dplyr::rename(Family=family,Genus=genus,Species=species)%>%
#     left_join(.,life.history)%>%
#     dplyr::rename(trophic.group=RLS.trophic.group)%>%
#     dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude,trophic.group)%>%
#     dplyr::summarise(total.abundance=sum(maxn))%>%
#     ungroup()%>%
#     replace_na(list(trophic.group="missing trophic group"))%>%
#     dplyr::mutate(metric="Trophic group")%>%
#     dplyr::rename(level=trophic.group)%>%
#     glimpse()
#   
#   target<-maxn%>%
#     dplyr::rename(Family=family,Genus=genus,Species=species)%>%
#     left_join(life.history)%>%
#     dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch")))%>%
#     tidyr::replace_na(list(target.group="Non-target"))%>%
#     dplyr::mutate(target.group=str_replace_all(.$target.group,c("Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
#     mutate(target.group = factor(target.group, levels = c("Target","Bycatch","Non-target")))%>%
#     mutate(target.group = fct_relevel(target.group, "Target","Bycatch","Non-target")) %>%
#     dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude,target.group)%>%
#     dplyr::summarise(total.abundance=sum(maxn))%>%
#     ungroup()%>%
#     dplyr::mutate(metric="Target group")%>%
#     dplyr::rename(level=target.group)%>%
#     glimpse()
#   
#   metric.dat <- bind_rows(trophic, target)
#   metric.dat
#   
# })


  # Length ----
length_data <- reactive({
  req(input$length.campaignid.selector)
  
  length.data <- fst::read_fst(input$complete.length$datapath)%>%
    as.data.frame()
  
  if (input$length.campaignid.selector == "All") {
    length.data
    
  } else {
    campaign.name <- input$length.campaignid.selector
    filter(length.data, campaignid == campaign.name)
  }
})

  # Length metric ----
metric_length_data <- reactive({
  
  req(input$length.metric.campaignid.selector)
  
  length.data <- fst::read_fst(input$complete.length$datapath)%>%
    as.data.frame()
  
  if (input$length.metric.campaignid.selector == "All") {
    length.data
    
  } else {
    campaign.name <- input$length.metric.campaignid.selector
    filter(length.data, campaignid == campaign.name)
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

  # Mass ----
  mass_data <- reactive({
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
  
  # Habitat ----
  habitat_data <- reactive({
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
  
### 3. Drop downs for species plots ----
  # Family for MaxN ----
  output$maxn.family.selector <- renderUI({
      
      if (input$maxn.campaignid.selector == "All") {
        df<-maxn_data()
        
        options <- df %>%
          distinct(family) %>%
          pull("family")%>%
          sort()
        
        create_dropdown("maxn.family.selector", options, "Family:")
        
      } else {
        
        df<-maxn_data()
        
        family.genus.species.to.keep<-df%>%
          filter(maxn>0)%>%
          filter(campaignid == input$maxn.campaignid.selector)%>%
          distinct(family,genus,species)
        
        options <- df %>%
          filter(campaignid == input$maxn.campaignid.selector) %>%
          semi_join(family.genus.species.to.keep)%>%
          distinct(family) %>%
          pull("family")%>%
          sort()
        
        create_dropdown("maxn.family.selector", options, "Family:")
      }
    })

  # Family for Length ----
  output$length.family.selector <- renderUI({
    
    if (input$length.campaignid.selector == "All") {
      df<-length_data()
      
      options <- df %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("length.family.selector", options, "Family:")
      
    } else {
      
      df<-length_data()
      
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

  # Genus for MaxN ----
  output$maxn.genus.selector <- renderUI({
      req(input$maxn.family.selector)
      
      if (input$maxn.campaignid.selector == "All") {
        df<-maxn_data()
        
        options <- df %>%
          filter(#key == input$key.selector,
            family == input$maxn.family.selector) %>%
          distinct(genus) %>%
          pull("genus")%>%
          sort()
        create_dropdown("maxn.genus.selector", options, "Genus:")
        
        
      } else {
        
        df<-maxn_data()
        
        family.genus.species.to.keep<-df%>%
          filter(maxn>0)%>%
          filter(campaignid == input$maxn.campaignid.selector)%>%
          distinct(family,genus,species)
        
        options <- df %>%
          #filter(key == input$key.selector) %>%
          filter(campaignid == input$maxn.campaignid.selector) %>%
          filter(family == input$maxn.family.selector) %>%
          semi_join(family.genus.species.to.keep)%>%
          distinct(genus) %>%
          pull("genus")%>%
          sort()
        
        create_dropdown("genus.selector", options, "Genus:")
      }
    })
  
  # Genus for Length ----
  output$length.genus.selector <- renderUI({
    req(input$length.family.selector)
    
    if (input$length.campaignid.selector == "All") {
      df<-length_data()
      
      options <- df %>%
        filter(family == input$length.family.selector) %>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      create_dropdown("length.genus.selector", options, "Genus:")
      
      
    } else {
      
      df<-length_data()
      
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
  
  # Species for MaxN ----
output$maxn.species.selector <- renderUI({
    req(input$maxn.family.selector, input$maxn.genus.selector)
    
    if (input$maxn.campaignid.selector == "All") {
      df<-maxn_data()
      
      options <- df %>%
        filter(
          #key == input$key.selector,
          family == input$maxn.family.selector,
          genus == input$maxn.genus.selector
        ) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("maxn.species.selector", options, "Species:")
      
      
    } else {
      
      df<-maxn_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$maxn.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$maxn.campaignid.selector) %>%
        filter(family == input$maxn.family.selector,
               genus == input$maxn.genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("maxn.species.selector", options, "Species:")
    }
  })
  
  # Species for Length ----
  output$length.species.selector <- renderUI({
    req(input$length.family.selector, input$length.genus.selector)
    
    if (input$length.campaignid.selector == "All") {
      df<-length_data()
      
      options <- df %>%
        filter(family == input$length.family.selector,genus == input$length.genus.selector) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      
      create_dropdown("length.species.selector", options, "Species:")
      
    } else {
      
      df<-length_data()
      
      family.genus.species.to.keep<-df%>%
        filter(number>0)%>%
        filter(campaignid == input$length.campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        filter(campaignid == input$length.campaignid.selector) %>%
        filter(family == input$length.family.selector,genus == input$length.genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      
      create_dropdown("length.species.selector", options, "Species:")
    }
  })
  
### 4. Data frames for species plots ----
  # Maxn  ----
 maxn_species_data <- reactive({
   req(input$maxn.family.selector, input$maxn.genus.selector, input$maxn.species.selector)
   maxn_data() %>%
     filter(
       family == input$maxn.family.selector,
       genus == input$maxn.genus.selector,
       species == input$maxn.species.selector
     )
 })
  
  # Length ----
  length_species_data <- reactive({
    req(input$length.family.selector, input$length.genus.selector, input$length.species.selector)
    
    length_data() %>%
      filter(
        family == input$length.family.selector,
        genus == input$length.genus.selector,
        species == input$length.species.selector
      )
  })

# Habitat data ---- # can just use (habitat_data())


### 5. Summarys ----
  # Maxn ----
maxn.summary.data <- reactive({
  life.history<-life.history()
  
  if (input$maxn.summary.groupby=="Species") {
    
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      dplyr::group_by(family,genus,species)%>%
      dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-total.abundance)%>%
      left_join(.,life.history)%>%
      tidyr::replace_na(list(target.group="Non-target",trophic.group="Missing trophic group"))%>%
      dplyr::rename(Family=family,Genus=genus,Species=species,'Total abundance'=total.abundance,'Number of samples'=number.of.samples,'Trophic group'=trophic.group,'Target group'=target.group)
  }
  
  if (input$maxn.summary.groupby=="Target group") {
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      left_join(.,life.history)%>%
      tidyr::replace_na(list(target.group="Non-target"))%>%
      dplyr::group_by(target.group)%>%
      dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-total.abundance)%>%
      dplyr::rename('Total abundance'=total.abundance,'Number of samples'=number.of.samples,'Target group'=target.group)%>%
      glimpse()
  }
  
  if (input$maxn.summary.groupby=="Trophic group") {
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      as.data.frame()%>%
      filter(maxn>0)%>%
      left_join(.,life.history)%>%
      tidyr::replace_na(list(trophic.group="Missing trophic group"))%>%
      dplyr::group_by(trophic.group)%>%
      dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(-total.abundance)%>%
      dplyr::rename('Total abundance'=total.abundance,'Number of samples'=number.of.samples,'Trophic group'=trophic.group)
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
    as.data.frame()
})

output$overall.abundance <- renderText({ 
  paste("Overall total abundance:",sum(maxn.summary.data()$'Total abundance'))
})

output$species.richness <- renderText({ 
  paste("Overall species richness:",length(unique(maxn.summary()$scientific)))
})


  # Length ----
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
      left_join(.,life.history)%>%
      dplyr::rename(Family=family,Genus=genus,Species=species,'Trophic group'=trophic.group,"Target group"=target.group,'Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length)
    
  }
  
  if (input$length.summary.groupby=="Target group") {
    length.data <- fst::read_fst(input$complete.length$datapath)%>%
      as.data.frame()%>%
      filter(length>0)%>%
      left_join(life.history)%>%
      replace_na(list(target.group="Non-target"))%>%
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
      left_join(life.history)%>%
      replace_na(list(trophic.group="Missing trophic group"))%>%
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

######################### PLOTS #########################
# Maxn species plot Status ----
output$maxn.status.plot <- renderPlot({
  
  maxn.per.sample<-maxn_species_data()%>%
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

# Maxn species plot Location ----
  
  output$maxn.location.plot <- renderPlot({
    
    maxn.per.sample<-maxn_species_data()%>%
      group_by(campaignid,sample,location)%>%
      summarise(maxn=sum(maxn))
    
    ggplot(maxn.per.sample, aes(x = location,y=maxn)) + 
      stat_summary(fun.y=mean, geom="bar",fill="white",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      Theme1+
      theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
      ggtitle("Plot of abundance by Location")
    
  })
  
# Maxn species plot Site ----
  output$maxn.site.plot <- renderPlot({
    
    maxn.per.sample<-maxn_species_data()%>%
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
 
# Maxn metric plot Status ----
output$metrics.maxn.status.plot <- renderPlot({
  
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$metrics.selector)%>%
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

# Maxn metric plot Location ----

output$assemblage.maxn.location.plot <- renderPlot({
  
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$metrics.selector)%>%
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

# Maxn metric plot Site ----
output$assemblage.maxn.site.plot <- renderPlot({
  maxn.per.sample<-assemblage_metric_data()%>%
    as.data.frame()%>%
    filter(metric == input$metrics.selector)%>%
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

# Maxn metric plot Status ----
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


# Length metric plot Status ----
output$length.metric.status <- renderPlot({
  
  length.metric<-metric_length_data()%>%
    as.data.frame()%>%
    filter(metric == input$length.metric.selector)%>%
    glimpse()
  
  posn.d <- position_dodge(0.75)
  
  
  stats.dat<-length.metric%>%
    filter(metric==input$length.metric.selector)%>%
    group_by(level)%>%
    summarise(max=max(boxplot.stats(length)$stats))%>%
    summarise(max=max(max))%>%
    as.data.frame()

  ggplot(length.metric,aes(x = level, y = length, fill=status, notch=FALSE, outlier.shape = NA)) + 
    stat_boxplot(geom='errorbar')+
    geom_boxplot(outlier.color = NA, notch=FALSE)+
    stat_summary(fun.y=mean, geom="point", shape=23, size=4, position=posn.d)+ #this is adding the dot for the mean
    theme_bw()+
    xlab("") + ylab("Length (mm)") +
    ggtitle("Plot of length by Status") +
    theme_bw()+
    Theme1+
    theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
    coord_cartesian(ylim = c(0,max(stats.dat)*1.15)) #1.45
    
})



# Mass metric plot Status ----
output$mass.status <- renderPlot({

  req(input$mass.campaignid.selector)
  
  mass.data <- fst::read_fst(input$complete.mass$datapath)%>%
    as.data.frame()
  
  if (input$mass.campaignid.selector == "All") {
    mass.data<-mass.data
    
  } else {
    campaign.name <- input$mass.campaignid.selector
    filter(mass.data, campaignid == campaign.name)
    mass.data<-mass.data
  }

life.history<-life.history()%>%
  ga.clean.names()%>%
  dplyr::mutate(fishing.type=str_replace_all(.$fishing.type,c("R"="Recreational","C"="Commercial","B/"=NA,"B"="Bycatch","Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target","NA"=NA)))%>%
  mutate(fishing.type = factor(fishing.type, levels = c("Target","Bycatch","Non-target")))%>%
  mutate(fishing.type = fct_relevel(fishing.type, "Target","Bycatch","Non-target"))%>%
  tidyr::replace_na(list(fishing.type="Non-target"))

if (input$mass.group.selector == "Target group") {

over.200<-mass.data%>%
  left_join(.,life.history)%>%
  filter(length>=200)%>%
  tidyr::replace_na(list(fishing.type="Non-target"))%>%
  dplyr::group_by(campaignid,sample,status,fishing.type)%>%
  dplyr::summarise(total.mass=sum(mass.g))%>%
  dplyr::mutate(metric="Mass of all fish greater than 200 mm")%>%
  replace_na(list(total.mass=0))%>%
  dplyr::rename(level=fishing.type)%>%
  glimpse()

over.300<-mass.data%>%
  left_join(.,life.history)%>%
  filter(length>=300)%>%
  tidyr::replace_na(list(fishing.type="Non-target"))%>%
  dplyr::group_by(campaignid,sample,status,fishing.type)%>%
  dplyr::summarise(total.mass=sum(mass.g))%>%
  dplyr::mutate(metric="Mass of all fish greater than 300 mm")%>%
  replace_na(list(total.mass=0))%>%
  dplyr::rename(level=fishing.type)%>%
  glimpse()

total.mass<-mass.data%>%
  left_join(.,life.history)%>%
  filter(length>0)%>%
  tidyr::replace_na(list(fishing.type="Non-target"))%>%
  dplyr::group_by(campaignid,sample,status,fishing.type)%>%
  dplyr::summarise(total.mass=sum(mass.g))%>%
  dplyr::mutate(metric="Total mass of all fish")%>%
  replace_na(list(total.mass=0))%>%
  dplyr::rename(level=fishing.type)%>%
  glimpse()

} else {
  over.200<-mass.data%>%
    left_join(.,life.history)%>%
    filter(length>=200)%>%
    dplyr::group_by(campaignid,sample,status,rls.trophic.group)%>%
    dplyr::summarise(total.mass=sum(mass.g))%>%
    dplyr::mutate(metric="Mass of all fish greater than 200 mm")%>%
    replace_na(list(total.mass=0))%>%
    dplyr::rename(level=rls.trophic.group)
  
  over.300<-mass.data%>%
    left_join(.,life.history)%>%
    filter(length>=300)%>%
    dplyr::group_by(campaignid,sample,status,rls.trophic.group)%>%
    dplyr::summarise(total.mass=sum(mass.g))%>%
    dplyr::mutate(metric="Mass of all fish greater than 300 mm")%>%
    replace_na(list(total.mass=0))%>%
    dplyr::rename(level=rls.trophic.group)
  
  total.mass<-mass.data%>%
    left_join(.,life.history)%>%
    filter(length>0)%>%
    dplyr::group_by(campaignid,sample,status,rls.trophic.group)%>%
    dplyr::summarise(total.mass=sum(mass.g))%>%
    dplyr::mutate(metric="Total mass of all fish")%>%
    replace_na(list(total.mass=0))%>%
    dplyr::rename(level=rls.trophic.group)
}

mass.metrics<-bind_rows(over.200,over.300,total.mass)%>%
  filter(metric==input$mass.metric.selector)%>%
  mutate(total.mass=total.mass/1000)

posn.d <- position_dodge(0.75)

stats.dat<-mass.metrics%>%
  filter(metric==input$mass.metric.selector)%>%
  group_by(level)%>%
  summarise(max=max(boxplot.stats(total.mass)$stats))%>%
  summarise(max=max(max))%>%
  as.data.frame()

posn.d <- position_dodge(0.9)

ggplot(mass.metrics, aes(x = level, y=total.mass, fill = status, group=status)) + 
  stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
  geom_hline(aes(yintercept=0))+
  xlab("")+
  ylab("Per stereo-BRUV \n(+/- SE)")+
  theme_bw()+
  Theme1+
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Plot of mass by Status")
})



# Maxn species spatial plot ----
  # Create spatial plot
  output$maxn.spatial.plot <- renderLeaflet({
    map <- leaflet(maxn_species_data()) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))

    overzero <- filter(maxn_species_data(), maxn > 0)%>%glimpse()
    equalzero <- filter(maxn_species_data(), maxn == 0)%>%glimpse()
    
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
  
  
# Maxn metric spatial plot ----
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
  
  
  
# Habitat spatial plot ----
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


# Download script ----
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("example-plotting-script", "R", sep=".")
    },
    
    content <- function(file) {
      file.copy("example-plotting-script.R", file)
    },
    contentType = "R File/R"
  )
  
}