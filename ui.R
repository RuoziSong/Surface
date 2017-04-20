library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

type <- c(
  "Any" = "Any",
  "Afghan" = "Afghan",
  "African" = "African",
  "American" = "American",
  "Armenian" = "Armenian",
  "Asian" = "Asian",
  "Australian" = "Australian",
  "Bagels/Pretzels" = "Bagels",
  "Bakery" = "Bakery",
  "Bangladeshi" = "Bangladeshi",
  "Barbecue" = "Barbecue",
  "Bottled beverages, including water, sodas, juices, etc." = "Bottled beverages, including water, sodas, juices, etc.",
  "Brazilian" = "Brazilian",
  "CafÃ©/Coffee/Tea" = "CafÃ©/Coffee/Tea",
  "Cajun" = "Cajun",
  "Californian" = "Californian",
  "Caribbean" = "Caribbean",
  "Chicken" = "Chicken",
  "Chilean" = "Chilean",
  "Chinese"	= "Chinese",
  "Chinese/Cuban"	= "Chinese/Cuban",
  "Chinese/Japanese" = "Chinese/Japanese",
  "Continental" = "Continental",
  "Creole" = "Creole",
  "Creole/Cajun" = "Creole/Cajun",
  "Czech"	= "Czech",
  "Delicatessen" = "Delicatessen",
  "Donuts" = "Donuts",
  "Eastern European" = "Eastern European",
  "Egyptian" = "Egyptian",
  "English" = "English",
  "Ethiopian" = "Ethiopian",
  "Filipino" = "Filipino",
  "French" = "French",
  "Fruits/Vegetables" = "Fruits/Vegetables",
  "German" = "German",
  "Greek" = "Greek",
  "Hamburgers" = "Hamburgers",
  "Hawaiian" = "Hawaiian",
  "Hotdogs" = "Hotdogs",
  "Hotdogs/Pretzels" = "Hotdogs/Pretzels",
  "Ice Cream, Gelato, Yogurt, Ices"	= "Ice Cream, Gelato, Yogurt, Ices",
  "Indian" = "Indian",
  "Indonesian" = "Indonesian",
  "Iranian" = "Iranian",
  "Irish"	= "Irish",
  "Italian"	= "Italian",
  "Japanese" = "Japanese",
  "Jewish/Kosher"	= "Jewish/Kosher",
  "Juice, Smoothies, Fruit Salads" = "Juice, Smoothies, Fruit Salads",
  "Korean" = "Korean",
  "Latin (Cuban, Dominican, Puerto Rican, South & Central American)" = "Latin (Cuban, Dominican, Puerto Rican, South & Central American)",
  "Mediterranean" = "Mediterranean",
  "Mexican"	= "Mexican",
  "Middle Eastern" = "Middle Eastern",
  "Moroccan" = "Moroccan",
  "Not Listed/Not Applicable" = "Not Listed/Not Applicable",
  "Nuts/Confectionary" = "Nuts/Confectionary",
  "Other"	= "Other",
  "Pakistani" = "Pakistani",
  "Pancakes/Waffles" = "Pancakes/Waffles",
  "Peruvian" = "Peruvian",
  "Pizza" = "Pizza",
  "Pizza/Italian"	= "Pizza/Italian",
  "Polish" = "Polish",
  "Polynesian" = "Polynesian",
  "Portuguese" = "Portuguese",
  "Russian"	= "Russian",
  "Salads" = "Salads",
  "Sandwiches" = "Sandwiches",
  "Sandwiches/Salads/Mixed Buffet" = "Sandwiches/Salads/Mixed Buffet",
  "Scandinavian" = "Scandinavian",
  "Seafood" = "Seafood",
  "Soul Food" = "Soul Food",
  "Soups"	= "Soups",
  "Soups & Sandwiches" = "Soups & Sandwiches",
  "Southwestern" = "Southwestern",
  "Spanish" = "Spanish",
  "Steak" = "Steak",
  "Tapas" = "Tapas",
  "Tex-Mex" = "Tex-Mex",
  "Thai" = "Thai",
  "Turkish" = "Turkish",
  "Vegetarian" = "Vegetarian",
  "Vietnamese/Cambodian/Malaysia"	= "Vietnamese/Cambodian/Malaysia"
)

loc <- c(
  Any = "Any",
  MANHATTAN = "MANHATTAN",
  QUEENS = "QUEENS",
  Staten.ISLAND = "STATEN ISLAND",
  BRONX = "BRONX",
  BROOKLYN = "BROOKLYN"
)
analysis_obj<-c(
  "Restaurant Type" = "CUISINE.DESCRIPTION",
  "Violation Type" = "VIOLATION.CODE",
  "Time" = "INSPECTION.DATE"
)
navbarPage("Resturant Inspection", id="nav",
           
           tabPanel("Interactive map",
                    
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="70%", height="60%"),
                        absolutePanel(id = "pic1", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = "65%", left = "1%", right = "auto", bottom = "1%",
                                      width = "32%", height = "auto",
                                      plotOutput("scorebyTime", height = 200)
                        ),
                        absolutePanel(id = "pic1", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = "65%", left = "37.5%", right = "auto", bottom = "1%",
                                      width = "32%", height = "auto",
                                      plotOutput("scorebyViolationCode", height = 200)
                        ),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = "25%", height = "auto",
                                      
                                      h2("ZIP explorer"),
                                      
                                      #selectInput("color", "Color", vars),
                                      textInput(inputId = "restName", "Name"),
                                      textInput(inputId = "zipcode", "Zip Code"),
                                      #selectInput("directions", "Resturant Type", loc),
                                      # checkboxGroupInput(inputId = "directions", "Show",
                                      #                    choices = c(
                                      #                      MANHATTAN = "MANHATTAN",
                                      #                      QUEENS = "QUEENS",
                                      #                      ISLAND = "STATEN ISLAND",
                                      #                      BRONX = "BRONX",
                                      #                      BROOKLYN = "BROOKLYN"
                                      #                    )),
                                      selectInput(inputId = "directions", "Direction", loc),
                                      selectInput(inputId = "type", "Resturant Type", type),
                                      actionButton(inputId = "click",label = "Search"),
                                      #selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      absolutePanel(id = "icon1", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = "65%", left = "auto", right = 20, bottom = "26%",
                                                    width = "25%", height = "auto",
                                                    imageOutput("codeIcon1", height = "100%", width = "100%")
                                      ),
                                      absolutePanel(id = "icon2", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = "71.5%", left = "auto", right = 20, bottom = "19.5%",
                                                    width = "25%", height = "auto",
                                                    imageOutput("codeIcon2", height = "100%", width = "100%")
                                      ),
                                      absolutePanel(id = "icon3", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = "78%", left = "auto", right = 20, bottom = "13%",
                                                    width = "25%", height = "auto",
                                                    imageOutput("codeIcon3",height = "100%", width = "100%")
                                      ),
                                      absolutePanel(id = "icon4", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = "84.5%", left = "auto", right = 20, bottom = "6.5%",
                                                    width = "25%", height = "auto",
                                                    imageOutput("codeIcon4", height = "100%", width = "100%")
                                      ),
                                      absolutePanel(id = "icon5", class = "panel panel-default", fixed = TRUE,
                                                    draggable = TRUE, top = "91%", left = "auto", right = 20, bottom = "0%",
                                                    width = "25%", height = "auto",
                                                    imageOutput("codeIcon5", height = "100%", width = "100%")
                                      )
                        )
                    )
           ),
           
           tabPanel("Analysis - Score by zipcode",
                    fluidRow(
                      column(width = 9,
                             #box(width = NULL, solidHeader = TRUE,
                                 leafletOutput("zipcodeMap", height=400),
                             
                             #box(width=NULL,
                                 dataTableOutput("zipcodeTable")
                             ),
                      
                      column(width=3,
                             #box(width=NULL, 
                                 uiOutput("yearSelect"),
                                 radioButtons("measure", "Measure",c("Mean"="Mean", "Median"="Median")),
                                 uiOutput("violationType")
                             )
                      )
                    ),
           
           tabPanel("Analysis",
                    fluidRow(
                      column(3,
                             selectInput(inputId = "analysis_x", "Analysis Object", analysis_obj)
                      )
                    ),
                    actionButton(inputId = "click2",label = "Plot"),
                    hr(),
                    plotOutput("histCentile", height = 400),
                    plotOutput("score", height = 400),
                    plotOutput("most_v", height = 400)
           )
)
