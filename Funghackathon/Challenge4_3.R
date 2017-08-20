library(shiny)
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sourcing Country Analytics"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("Type_Product", "Choose a Type of Product:",
                  choices = c('SWEATER','KNIT','WOVEN','ACCESSORY','SLEEP','ACCESSORIES MISC','BAGS','HAIR','JEWELRY','SOCKS','HATS','SHOES','SUNGLASSES','BELTS','UNDERWEAR','COLD WEATHER','TIGHT','TOYS','TIES','COSTUMES','Accessories','Socks','Shoes','Woven','Knit','Sweater','Sleep','NA')),
      selectInput("Retail_Class", "Choose a Retail Class:",
                  choices = c('Tops','Bottoms','Outerwear','Dresses','Other Apparel','Mens Apparel','Onepieces','Swim','Accessories','Womens Apparel','Sleepwear','Other Non-Apparel','Socks','Hats','Shoes','Underwear','Basic Tops','Knit Tops','Woven Tops','Woven Shorts','Woven Pants','Active Tops','Knit Shorts','Denim Pants','Sweaters','Denim Shorts','Skorts','Skirts','Knit Pants','Overall/Shortall','Graphic Tops','Sets','Cold Weather','Hair','Misc','Tights','Jewelry','Girl Sleepwear','Boy Sleepwear','Pants','Shorts','Onepiece','Other','Sunglasses','Sleep','Bags','Bibs','Blanket','Rompers','Jumpers','Toys','Uni Sleepwear','Costume','NA')),
      selectInput("Style Number", "Choose a Style Number:",
                  choices = c(14083743,14082212)),
      
      # Input: Specify the number of observations to view ----
      numericInput("Price", "Current Cotton Price(in US$):", 10),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Generate Suggestion")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Suggested Ordering Country"),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput1 <- eventReactive(input$update, {
    switch(input$Type_Product,
           'SWEATER'=TypeProduct_Desc_SWEATER
           ,'KNIT'=TypeProduct_Desc_KNIT
           ,'WOVEN'=TypeProduct_Desc_WOVEN
           ,'ACCESSORY'=TypeProduct_Desc_ACCESSORY
           ,'SLEEP'=TypeProduct_Desc_SLEEP
           ,'ACCESSORIES MISC'=TypeProduct_Desc_ACCESSORIES.MISC
           ,'BAGS'=TypeProduct_Desc_BAGS
           ,'HAIR'=TypeProduct_Desc_HAIR
           ,'JEWELRY'=TypeProduct_Desc_JEWELRY
           ,'SOCKS'=TypeProduct_Desc_SOCKS
           ,'HATS'=TypeProduct_Desc_HATS
           ,'SHOES'=TypeProduct_Desc_SHOES
           ,'SUNGLASSES'=TypeProduct_Desc_SUNGLASSES
           ,'BELTS'=TypeProduct_Desc_BELTS
           ,'UNDERWEAR'=TypeProduct_Desc_UNDERWEAR
           ,'COLD WEATHER'=TypeProduct_Desc_COLD.WEATHER
           ,'TIGHT'=TypeProduct_Desc_TIGHT
           ,'TOYS'=TypeProduct_Desc_TOYS
           ,'TIES'=TypeProduct_Desc_TIES
           ,'COSTUMES'=TypeProduct_Desc_COSTUMES
           ,'Accessories'=TypeProduct_Desc_Accessories
           ,'Socks'=TypeProduct_Desc_Socks
           ,'Shoes'=TypeProduct_Desc_Shoes
           ,'Woven'=TypeProduct_Desc_Woven
           ,'Knit'=TypeProduct_Desc_Knit
           ,'Sweater'=TypeProduct_Desc_Sweater
           ,'Sleep'=TypeProduct_Desc_Sleep
           )}, ignoreNULL = FALSE)
  datasetInput2 <- eventReactive(input$update, {
    switch(input$Retail_Class,
           'Tops'=Retail_Class_Desc_Tops
           ,'Bottoms'=Retail_Class_Desc_Bottoms
           ,'Outerwear'=Retail_Class_Desc_Outerwear
           ,'Dresses'=Retail_Class_Desc_Dresses
           ,'Other Apparel'=Retail_Class_Desc_Other.Apparel
           ,'Mens Apparel'=Retail_Class_Desc_Mens.Apparel
           ,'Onepieces'=Retail_Class_Desc_Onepieces
           ,'Swim'=Retail_Class_Desc_Swim
           ,'Accessories'=Retail_Class_Desc_Accessories
           ,'Womens Apparel'=Retail_Class_Desc_Womens.Apparel
           ,'Sleepwear'=Retail_Class_Desc_Sleepwear
           ,'Other Non-Apparel'=Retail_Class_Desc_Other.Non-Apparel
           ,'Socks'=Retail_Class_Desc_Socks,'Hats'=Retail_Class_Desc_Hats
           ,'Shoes'=Retail_Class_Desc_Shoes
           ,'Underwear'=Retail_Class_Desc_Underwear
           ,'Basic Tops'=Retail_Class_Desc_Basic.Tops
           ,'Knit Tops'=Retail_Class_Desc_Knit.Tops
           ,'Woven Tops'=Retail_Class_Desc_Woven.Tops
           ,'Woven Shorts'=Retail_Class_Desc_Woven.Shorts
           ,'Woven Pants'=Retail_Class_Desc_Woven.Pants
           ,'Active Tops'=Retail_Class_Desc_Active.Tops
           ,'Knit Shorts'=Retail_Class_Desc_Knit.Shorts
           ,'Denim Pants'=Retail_Class_Desc_Denim.Pants
           ,'Sweaters'=Retail_Class_Desc_Sweaters
           ,'Denim Shorts'=Retail_Class_Desc_Denim.Shorts
           ,'Skorts'=Retail_Class_Desc_Skorts
           ,'Skirts'=Retail_Class_Desc_Skirts
           ,'Knit Pants'=Retail_Class_Desc_Knit.Pants
           ,'Overall/Shortall'=Retail_Class_Desc_Overall/Shortall
           ,'Graphic Tops'=Retail_Class_Desc_Graphic.Tops
           ,'Sets'=Retail_Class_Desc_Sets
           ,'Cold Weather'=Retail_Class_Desc_Cold.Weather
           ,'Hair'=Retail_Class_Desc_Hair
           ,'Misc'=Retail_Class_Desc_Misc
           ,'Tights'=Retail_Class_Desc_Tights
           ,'Jewelry'=Retail_Class_Desc_Jewelry
           ,'Girl Sleepwear'=Retail_Class_Desc_Girl.Sleepwear
           ,'Boy Sleepwear'=Retail_Class_Desc_Boy.Sleepwear
           ,'Pants'=Retail_Class_Desc_Pants
           ,'Shorts'=Retail_Class_Desc_Shorts
           ,'Onepiece'=Retail_Class_Desc_Onepiece
           ,'Other'=Retail_Class_Desc_Other
           ,'Sunglasses'=Retail_Class_Desc_Sunglasses
           ,'Sleep'=Retail_Class_Desc_Sleep
           ,'Bags'=Retail_Class_Desc_Bags
           ,'Bibs'=Retail_Class_Desc_Bibs
           ,'Blanket'=Retail_Class_Desc_Blanket
           ,'Rompers'=Retail_Class_Desc_Rompers
           ,'Jumpers'=Retail_Class_Desc_Jumpers
           ,'Toys'=Retail_Class_Desc_Toys
           ,'Uni Sleepwear'=Retail_Class_Desc_Uni.Sleepwear
           ,'Costume'=Retail_Class_Desc_Costume
           ,'NA'=Retail_Class_Desc_NA
    )
  }, ignoreNULL = FALSE)
  

  
  output$table<-renderTable({
    
  }
  )
  
  # Generate a summary of the dataset ----
  #output$summary <- renderPrint({
  #  dataset <- datasetInput()
  #  summary(dataset)
  #})
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  
  output$view <- renderTable({
    data.temp2
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)