#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
      h3('CoT'),
      verbatimTextOutput(ns('test'))
  )
}

#' data_import Server Functions
#'
#' @noRd
mod_data_import_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe(
        r$CoT_data <- CoT_data_fetch()
    )

    quandl_api_key("4uhW7tJsNypwPsnidegi")
    fredr_set_key("e8c127b78bd193a8f581b7bf43875c70")

    output$test <- renderPrint({
        x <- reactiveValuesToList(r)
        x
    })


    #### CoT - Commitment of Traders Report ####
    CoT_data_fetch <- reactive({
        data <- Quandl(c(
            'CFTC/134741_F_ALL', # SOFR
            'CFTC/042601_F_ALL', # 2yr T Note
            'CFTC/044601_F_ALL', # 5yr T Note
            'CFTC/043602_F_ALL',  # 10yr T Note
            'CFTC/020601_F_ALL', # 30yr Bond
            'CFTC/134741_F_L_ALL', # SOFR Legacy format
            'CFTC/042601_F_L_ALL', # 2yr T Note Legacy format
            'CFTC/044601_F_L_ALL', # 5yr T Note Legacy format
            'CFTC/043602_F_L_ALL',  # 10yr T Note Legacy format
            'CFTC/020601_F_L_ALL', # 30yr Bond Legacy format
            'CFTC/097741_F_ALL', # Japanese Yen
            'CFTC/097741_F_L_ALL', # Japanese Yen Legacy format
            'CFTC/092741_F_ALL', # Swiss Franc
            'CFTC/092741_F_L_ALL',
            'CFTC/232741_F_ALL', # Aussie Dollar
            'CFTC/232741_F_L_ALL',
            'CFTC/090741_F_ALL', # Canadian Dollar
            'CFTC/090741_F_L_ALL',
            'CFTC/112741_F_ALL', # New Zealand Dollar
            'CFTC/112741_F_L_ALL',
            'CFTC/095741_F_ALL', # Mexican Peso
            'CFTC/095741_F_L_ALL',
            'CFTC/099741_F_ALL', # Euro
            'CFTC/099741_F_L_ALL',
            'CFTC/096742_F_ALL', # British Pound
            'CFTC/096742_F_L_ALL',
            'CFTC/13874A_F_ALL', # S&P e-mini
            'CFTC/13874A_F_L_ALL',
            'CFTC/209747_F_ALL', # Nasdaq micro e-mini
            'CFTC/209747_F_L_ALL',
            'CFTC/12460P_F_ALL', # DJIA e-mini
            'CFTC/12460P_F_L_ALL',
            'CFTC/239777_F_ALL', # Russell 2000
            'CFTC/239777_F_L_ALL',
            'CFTC/240741_F_ALL', # Nikkei Stock Average
            'CFTC/240741_F_L_ALL',
            'CFTC/1170E1_F_ALL', # VIX
            'CFTC/1170E1_F_L_ALL',
            'CFTC/T_F_ALL', # WTI Crude
            'CFTC/T_F_L_ALL',
            'CFTC/RBB_F_ALL', # RBOB Crack Spread
            'CFTC/RBB_F_L_ALL',
            'CFTC/023651_F_ALL', # Natual Gas NYME
            'CFTC/023651_F_L_ALL',
            'CFTC/BH_F_ALL', # Heating Oil #2
            'CFTC/BH_F_L_ALL',
            'CFTC/075651_F_ALL', # Palladium
            'CFTC/075651_F_L_ALL',
            'CFTC/076651_F_ALL', # Platinum
            'CFTC/076651_F_L_ALL',
            'CFTC/084602_F_ALL', # Silver Troy 1000
            'CFTC/084602_F_L_ALL',
            'CFTC/085692_F_ALL', # Copper
            'CFTC/085692_F_L_ALL',
            'CFTC/088606_F_ALL', # Gold 100 Troy Oz
            'CFTC/088606_F_L_ALL',
            'CFTC/001612_F_ALL', # Wheat HRW
            'CFTC/001612_F_L_ALL',
            'CFTC/PYC1_F_ALL', # Corn
            'CFTC/PYC1_F_L_ALL',
            'CFTC/001602_F_ALL', # Wheat SRW
            'CFTC/001602_F_L_ALL',
            'CFTC/007601_F_ALL', # Soybeans Oil
            'CFTC/007601_F_L_ALL',
            'CFTC/CZ_F_ALL', #Soybean
            'CFTC/CZ_F_L_ALL',
            'CFTC/SN_F_ALL', # Soybean Meal
            'CFTC/SN_F_L_ALL',
            'CFTC/083731_F_ALL', # Coffee
            'CFTC/083731_F_L_ALL',
            'CFTC/033661_F_ALL', # Cotton
            'CFTC/033661_F_L_ALL',
            'CFTC/080732_F_ALL', # Sugar No. 11
            'CFTC/080732_F_L_ALL',
            'CFTC/040701_F_ALL', # Orange Juice
            'CFTC/040701_F_L_ALL',
            'CFTC/073732_F_ALL', # Cocoa
            'CFTC/073732_F_L_ALL',
            'CFTC/054631_F_ALL', # Live Hogs
            'CFTC/054631_F_L_ALL',
            'CFTC/061641_F_ALL', # Feeder Cattle
            'CFTC/061641_F_L_ALL',
            'CFTC/057642_F_ALL', # Live Cattle
            'CFTC/057642_F_L_ALL'
        ))
        data
    })

  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_1")

## To be copied in the server
# mod_data_import_server("data_import_1")
