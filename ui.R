navbarPage(
  "Plot Annotation data",
  tabPanel(
    "Upload",
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
    "MaxN summary",
    sidebarLayout(
      sidebarPanel(
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
    "MaxN plot species",
    sidebarLayout(
      sidebarPanel(
        
        # Select CampaignID
        selectInput(inputId = "maxn.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        
        # Select Family, Genus and Species
        htmlOutput("maxn.family.selector",multiple=TRUE),
        htmlOutput("maxn.genus.selector",multiple=TRUE),
        htmlOutput("maxn.species.selector",multiple=TRUE)
      ),
      
      # Plots
      mainPanel(
        leafletOutput(outputId = "maxn.spatial.plot", height = "500px"),
        plotOutput(outputId = "maxn.status.plot", height = "300px"),
        plotOutput(outputId = "maxn.location.plot", height = "300px"),
        plotOutput(outputId = "maxn.site.plot", height = "300px")
      )
    )
  ),
  
  # MaxN assemblage plots Tab ----
  tabPanel(
    "MaxN plot metrics",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "maxn.metric.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        selectInput("metric.selector", "Select metric to plot", choices = c("Total abundance",
                                                                             "Species richness",
                                                                             "Abundance by trophic group",
                                                                             "Abundance by target group"), multiple = FALSE)),
      
      mainPanel(leafletOutput(outputId = "maxn.metric.spatial.plot", height = "500px"),
                plotOutput(outputId = "maxn.metric.maxn.status.plot", height = "300px"),
                plotOutput(outputId = "maxn.metric.location.plot", height = "300px"),
                plotOutput(outputId = "maxn.metric.site.plot", height = "300px")
      )
    )
  ),
  
  # # MaxN metric plots Tab ----
  # tabPanel(
  #   "MaxN plot metrics",
  #   sidebarLayout(
  #     sidebarPanel(
  #       selectInput(inputId = "metrics.maxn.campaignid.selector", label = "CampaignID",
  #                   choices = NULL),
  #       selectInput("metrics.maxn.metric.selector", "Select metric to plot", choices = c("Target group","Trophic group"), multiple = FALSE)),
  #     
  #     mainPanel(plotOutput(outputId = "metrics.maxn.status.plot", height = "600px"))
  #   )
  # ),
  
  # Length summary Tab ----
  tabPanel(
    "Length summary",
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
    "Length plot species",
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
        plotOutput(outputId = "length.histogram", height = "400px"),
        plotOutput(outputId = "length.status.plot", height = "400px"),
        #plotOutput(outputId = "length.vs.range", height = "300px"),
        leafletOutput(outputId = "length.spatial.plot", height = "500px")
      )
    )
  ),
  
  # Length metrics Tab -----
  tabPanel(
    "Length plot metrics",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "length.metric.campaignid.selector", label = "CampaignID",
                    choices = NULL),
      selectInput("length.metric.selector", "Select metric to plot", choices = c("Target group","Trophic group"), multiple = FALSE)),
      
      # Plots
      mainPanel(
        plotOutput(outputId = "length.metric.status", height = "600px"),
        plotOutput(outputId = "length.trophic.status", height = "300px"))
    )
  ),
  
  # Mass metrics Tab ----
  tabPanel(
    "Mass plot metrics",
    sidebarLayout(
      sidebarPanel(
        # Select CampaignID
        selectInput(inputId = "mass.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        selectInput("mass.group.selector", "Select group to plot by:", choices = c("Target group","Trophic group"), multiple = FALSE),
        selectInput("mass.metric.selector", "Select metric to plot:", choices = c("Total mass of all fish","Mass of all fish greater than 200 mm","Mass of all fish greater than 300 mm"), multiple = FALSE)),
      
      # Plots
      mainPanel(
        plotOutput(outputId = "mass.status", height = "500px"),
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
    "Download",
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
