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
                       MANHATTAN = "MA",
                       QUEENS = "QU",
                       ISLAND = "IS",
                       BRONX = "BX",
                       BROOKLYN = "BK"
)
navbarPage("Superzip", id="nav",
           
           tabPanel("Interactive map",
                    
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="70%", height="60%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = "25%", height = "auto",
                                      
                                      h2("ZIP explorer"),
                                      
                                      #selectInput("color", "Color", vars),
                                      textInput("restName", "Name", "Name"),
                                      #selectInput("directions", "Resturant Type", loc),
                                      checkboxGroupInput("directions", "Show",
                                                         choices = c(
                                                           MANHATTAN = "MANHATTAN",
                                                           QUEENS = "QUEENS",
                                                           ISLAND = "STATEN ISLAND",
                                                           BRONX = "BRONX",
                                                           BROOKLYN = "BROOKLYN"
                                                         )),
                                      selectInput("type", "Resturant Type", type),
                                      actionButton(inputId = "click",label = "Search"),
                                      #selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
