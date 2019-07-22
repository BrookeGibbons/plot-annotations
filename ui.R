navbarPage(
  "Plot Annotation data",
  tabPanel(
    "Upload Data and load Life History",
    sidebarLayout(
      sidebarPanel(
        
  # File uploads ----
        
        fileInput("complete.maxn", "Upload complete maxn FST File",
                  accept = c("image/vnd.fst",
                             ".fst")),
        
        fileInput("complete.length", "Upload complete length FST File",
                  accept = c("image/vnd.fst",
                             ".fst")),
      
        fileInput("complete.mass", "Upload complete mass FST File",
                accept = c("image/vnd.fst",
                           ".fst")),
        
      fileInput("complete.habitat", "Upload complete habitat FST File",
                accept = c("image/vnd.fst",
                           ".fst")),
      
      textInput("worksheet.name", label = ("Life history googlesheet name"), value = "Australia.life.history"),
      textInput("sheet.name", label = ("Life history sheet name"), value = "australia.life.history")),
      
    mainPanel(plotOutput(outputId= "line.plot.test", height = "300px")))),
  
  # MaxN summary Tab ----
  tabPanel(
    "MaxN Summary",
    sidebarLayout(
      sidebarPanel(
        #checkboxGroupInput("show_vars", "Select column headers:",
        #                   c("Family","Genus","Species","Total abundance","Number of samples","Trophic group","Target group","Commercial","Recreational","Bycatch"), selected = c("Family","Genus","Species","Total abundance","Number of samples","Trophic group","Target group","Commercial","Recreational","Bycatch")),
        
        selectInput("maxn.summary.groupby", "Summarise by :",
                    c("Species" = "Species",
                      "Target group" = "Target group",
                      "Trophic group" = "Trophic group"))),
      
      mainPanel(textOutput("overall.abundance"),
                br(),
                textOutput("species.richness"),
                br(),
                br(),
                DT::dataTableOutput('maxn.summary')
      )
    )
  ),
  
  # MaxN Plot species Tab ----
  tabPanel(
    "Species MaxN plots",
    sidebarLayout(
      sidebarPanel(
        
        # Select CampaignID
        selectInput(inputId = "campaignid.selector", label = "CampaignID",
                    choices = NULL),
        
        # Select Family, Genus and Species
        htmlOutput("family.selector",multiple=TRUE),
        htmlOutput("genus.selector",multiple=TRUE),
        htmlOutput("species.selector",multiple=TRUE)
      ),
      
      # Plots
      mainPanel(
        leafletOutput(outputId = "spatial.plot", height = "500px"),
        plotOutput(outputId = "status.plot", height = "300px"),
        plotOutput(outputId = "location.plot", height = "300px"),
        plotOutput(outputId = "site.plot", height = "300px")
      )
    )
  ),
  

  
  # MaxN assemblage plots Tab ----
  tabPanel(
    "MaxN plots of assemblage",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "assemblage.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        selectInput("assemblage.maxn.metric.selector", "Select metric to plot", choices = c("Total abundance","Species richness"), multiple = FALSE)),
      
      mainPanel(leafletOutput(outputId = "assemblage.maxn.spatial.plot", height = "500px"),
                plotOutput(outputId = "assemblage.maxn.status.plot", height = "300px"),
                plotOutput(outputId = "assemblage.maxn.location.plot", height = "300px"),
                plotOutput(outputId = "assemblage.maxn.site.plot", height = "300px")
      )
    )
  ),
  
  # MaxN metric plots Tab ----
  tabPanel(
    "MaxN plots of metrics",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "metrics.maxn.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        selectInput("metrics.maxn.metric.selector", "Select metric to plot", choices = c("Target group","Trophic group"), multiple = FALSE)),
      
      mainPanel(plotOutput(outputId = "metrics.maxn.status.plot", height = "300px"),
                plotOutput(outputId = "metrics.maxn.location.plot", height = "300px"),
                plotOutput(outputId = "metrics.maxn.site.plot", height = "300px")
      )
    )
  ),
  
  
  # Length summary Tab ----
  tabPanel(
    "Length Summary",
    sidebarLayout(
      sidebarPanel(
        selectInput("length.summary.groupby", "Summarise by :",
                    c("Species" = "Species",
                      "Target group" = "Target group",
                      "Trophic group" = "Trophic group"))),
      
      mainPanel(DT::dataTableOutput('length.summary')
      )
    )
  ),

  
  # Length species Tab -----
  tabPanel(
    "Plot species length",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "length.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        
        # Select Family, Genus and species
        htmlOutput("length.family.selector"),
        htmlOutput("length.genus.selector"),
        htmlOutput("length.species.selector"),

        # Bin widths
        numericInput("length.binwidth","Binwidth", value = 5)),
      
      # Plots
      mainPanel(
        plotOutput(outputId = "length.histogram", height = "300px"),
        plotOutput(outputId = "length.boxplot", height = "300px"),
        #plotOutput(outputId = "length.vs.range", height = "300px"),
        leafletOutput(outputId = "length.spatial.plot", height = "500px")
      )
    )
  ),
  
  # Length metrics Tab -----
  tabPanel(
    "Plot length metrics",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "length.metric.campaignid.selector", label = "CampaignID",
                    choices = NULL)),
      
      # Plots
      mainPanel(
        plotOutput(outputId = "length.target.status", height = "300px"),
        plotOutput(outputId = "length.trophic.status", height = "300px"))
    )
  ),
  
  # Mass metrics Tab ----
  tabPanel(
    "Plot mass metrics",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "mass.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        htmlOutput("mass.metric.selector")),
      
      # Plots
      mainPanel(
        plotOutput(outputId = "mass.status", height = "300px"),
        leafletOutput(outputId = "mass.spatial.plot", height = "500px")
      )
    )
  ),
  
  # Habitat Tab ----
  tabPanel(
    "Habitat",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "habitat.campaignid.selector", label = "CampaignID",
                    choices = NULL),
      selectInput("habitat.type", "Select habitat type for bubble plot", choices = c("Macroalgae","Stony corals","Octocoral black","Sponges","Hydroids","Consolidated","Unconsolidated"), multiple = FALSE)),
      # Plots
      mainPanel(
        leafletOutput(outputId = "habitat.spatial.plot", height = "500px"),
        br(),
        leafletOutput(outputId = "habitat.bubble.plot", height = "500px")
      )
    )
  ),
  
  # Example scripts ----
  tabPanel(
    "Download Scripts",
    #headerPanel("Simple Shiny Ace!"),
    sidebarPanel(
      downloadButton("downloadData", label = "Download"),
      #selectInput("mode", "Mode: ", choices = modes, selected = "plain_text"),
      #selectInput("theme", "Theme: ", choices = themes, selected = "textmate"),
      #numericInput("size", "Tab size:", 4),
      #radioButtons("soft", NULL, c("Soft tabs" = TRUE, "Hard tabs" = FALSE), inline = TRUE),
      #radioButtons("invisible", NULL, c("Hide invisibles" = FALSE, "Show invisibles" = TRUE), inline = TRUE),
      #actionButton("reset", "Reset text"),
      #actionButton("clear", "Clear text"),
      HTML("<hr />"),
      helpText(HTML("A simple Shiny Ace editor.
                  <p>Created using <a href = \"http://github.com/trestletech/shinyAce\">shinyAce</a>."))
    ),
    mainPanel(
      aceEditor("ace", theme = "tomorrow_night_blue", mode = "r", value = "# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))

# se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

# status plot
ggplot(trends_data(),aes(x = factor(status), y = maxn, colour = status, fill = status,notch=FALSE, outlier.shape = NA))+ 
      theme( panel.background = element_blank(),axis.line = element_line(colour = \"black\"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom=\"point\", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab(\"Status\") + ylab(\"Abundance per stereo-BRUV\") +
      ggtitle(\"Plot of abundance by Status\")")
    )
)
)
