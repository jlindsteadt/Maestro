#' CoT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_CoT_ui <- function(id){
  ns <- NS(id)
  tagList(
    dashboardPage(
          dashboardHeader(),
          dashboardSidebar(
              sidebarMenu(
                  menuItem('CoT Report', tabName='CoT'),
                  menuItem('Model Builder', tabName='modeler')
              )
          ),
          dashboardBody(
              tabItems(
                  tabItem(tabName='Econ Dashboard',
                          tabsetPanel(
                              tabPanel('Consumer',

                              )
                          )

                  ),
                  tabItem(tabName='CoT',
                          tabsetPanel(
                              tabPanel('Extreme Net Positions',
                                       column(4,
                                              uiOutput(ns('CoT_net_position_date'))
                                              #selectizeInput(ns('CoT_net_position_date'), 'Report Date', choices=NULL)
                                        ),
                                       column(4,
                                              uiOutput(ns('CoT_investor_type_select'))
                                       ),
                                       # uiOutput('CoT_asset_class_select'),
                                       column(12,
                                              plotOutput(ns('CoT_Net_Position_plot'), height=600)
                                       )
                              ),
                              tabPanel('Weekly Movers',
                                       column(4,
                                              uiOutput(ns('CoT_investor_type_select2'))
                                       ),
                                       column(4,
                                              numericInput(ns('CoT_look_back'), 'Look-back (weeks)', value=2, min=1)
                                       ),
                                       column(12,
                                              plotOutput(ns('CoT_weekly_movers_plt'), height=600)
                                       )
                              ),
                              tabPanel('Hedgers/Large Speculators Breakdown',
                                       column(4,
                                              uiOutput(ns('CoT_asset_class_select'))
                                       ),
                                       column(4,
                                              uiOutput(ns('CoT_contract_select'))
                                       ),
                                       column(4,
                                              selectInput(ns('CoT_measurement'), 'Measurement', c('Number of Contracts'='Net Positioning Contracts',
                                                                                              '% of Open Interest' = 'Net Positioning Percent',
                                                                                              'Z-Score' = 'zscore_5yr'))
                                       ),
                                       column(12,
                                              plotOutput(ns('CoT_Breakdown_plot'), height=600)
                                       )

                              ),
                              tabPanel('Financial Detail',
                                       column(4,
                                              uiOutput(ns('CoT_contract_select2'))
                                       ),
                                       column(4,
                                              selectInput(ns('CoT_measurement2'), 'Measurement', c('Number of Contracts'='Net Positioning Contracts',
                                                                                               '% of Open Interest' = 'Net Positioning Percent',
                                                                                               'Z-Score' = 'zscore_5yr'))
                                       ),
                                       column(12,
                                              plotOutput(ns('CoT_Market_Breakdown_plot'), height=600)
                                       )

                              )
                          )
                  )#,
              #     tabItem(tabName='modeler',
              #             tabsetPanel(
              #                 tabPanel('Setup',
              #                          dataOutputUI("output-1"),
              #                          dataEditUI("edit-1"),
              #                          #tableOutput("tab")#,
              #                          verbatimTextOutput('data_raw'),
              #                          verbatimTextOutput('data_raw2')
              #                 ),
              #                 tabPanel('Univariate',
              #                          uiOutput('select_var'),
              #                          plotOutput('x_var_dist'),
              #                          plotOutput('acf_plot'),
              #                          plotOutput('time_series_plot'),
              #                          plotOutput('scatter_plot'),
              #                          verbatimTextOutput('selected_var')
              #                 ),
              #                 tabPanel('Bivariate',
              #                          plotOutput('corr_heat')
              #                          #verbatimTextOutput('corr_heat')
              #                 )
              #             )
              #     )
               )
           )
       )
  )
}

#' CoT Server Functions
#'
#' @noRd
mod_CoT_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$CoT_contract_select <- renderUI({
        x <- CoT_data_tbl() %>%
            filter(asset_class %in% input$CoT_asset_class_select) %>%
            pull(contract) %>%
            unique()
        selectInput(ns('CoT_contract_select'), 'Market', choices=x)
    })

    output$CoT_contract_select2 <- renderUI({
        x <- CoT_data_tbl() %>%
            pull(contract) %>%
            unique()
        selectInput(ns('CoT_contract_select2'), 'Market', choices=x)
    })

    output$CoT_investor_type_select <- renderUI({
        x <- CoT_data_tbl() %>%
            pull(investor) %>%
            unique()
        selectInput(ns('CoT_investor_type_select'), 'Investor Type', choices=x)
    })

    output$CoT_investor_type_select2 <- renderUI({
        x <- CoT_data_tbl() %>%
            pull(investor) %>%
            unique()
        selectInput(ns('CoT_investor_type_select2'), 'Investor Type', choices=x)
    })

    output$CoT_net_position_date <- renderUI({
        x <- r$CoT_data %>%
            arrange(desc(Date)) %>%
            pull(Date) %>%
            unique()

        selectInput(ns('CoT_net_position_date'), 'Report Date', choices=x[1:5])
    })

    output$CoT_asset_class_select <- renderUI({
        x <- CoT_data_tbl() %>%
            pull(asset_class) %>%
            unique()
        selectInput(ns('CoT_asset_class_select'), 'Asset Class', choices=x)
    })

    CoT_data_tbl <- reactive({
        investor_split <- c(' Longs', ' Shorts', " Spreads")
        # roll_zscore_5yr <- slidify_vec(.f= ~ (.x - mean(.x, na.rm=TRUE))/sd(.x, na.rm=TRUE),
        #                                .period=5*52,
        #                                .align = "left")

        roll_window <- 5*52

        data <- r$CoT_data %>%
            as_tibble() %>%
            pivot_longer(cols=-Date, names_to='name', values_to='value') %>%
            separate(name, c('product', 'report'), sep=" - ", remove=TRUE) %>%
            mutate(contract = case_when(product == 'CFTC.134741_F_ALL' ~ '3-Month SOFR',
                                        product == 'CFTC.042601_F_ALL' ~ '2yr T Note',
                                        product == 'CFTC.044601_F_ALL' ~ '5yr T Note',
                                        product == 'CFTC.043602_F_ALL' ~ '10yr T Note',
                                        product == 'CFTC.020601_F_ALL' ~ '30yr T Bond',
                                        product == 'CFTC.134741_F_L_ALL' ~ '3-Month SOFR',
                                        product == 'CFTC.042601_F_L_ALL' ~ '2yr T Note',
                                        product == 'CFTC.044601_F_L_ALL' ~ '5yr T Note',
                                        product == 'CFTC.043602_F_L_ALL' ~ '10yr T Note',
                                        product == 'CFTC.020601_F_L_ALL' ~ '30yr T Bond',
                                        product == 'CFTC.097741_F_ALL' ~ 'Japanese Yen',
                                        product == 'CFTC.097741_F_L_ALL' ~ 'Japanese Yen',
                                        product == 'CFTC.092741_F_ALL' ~ 'Swiss Franc',
                                        product == 'CFTC.092741_F_L_ALL' ~ 'Swiss Franc',
                                        product == 'CFTC.232741_F_ALL' ~ 'Aussie Dollar',
                                        product == 'CFTC.232741_F_L_ALL' ~ 'Aussie Dollar',
                                        product == 'CFTC.090741_F_ALL' ~ 'Canadian Dollar',
                                        product == 'CFTC.090741_F_L_ALL'~ 'Canadian Dollar',
                                        product == 'CFTC.112741_F_ALL' ~ 'New Zealand Dollar',
                                        product == 'CFTC.112741_F_L_ALL' ~ 'New Zealand Dollar',
                                        product == 'CFTC.095741_F_ALL' ~ 'Mexican Peso',
                                        product == 'CFTC.095741_F_L_ALL' ~ 'Mexican Peso',
                                        product == 'CFTC.099741_F_ALL' ~ 'Euro',
                                        product == 'CFTC.099741_F_L_ALL' ~ 'Euro',
                                        product == 'CFTC.096742_F_ALL' ~ 'British Pound',
                                        product == 'CFTC.096742_F_L_ALL' ~ 'British Pound',
                                        product == 'CFTC.13874A_F_ALL' ~ 'S&P 500 e-mini',
                                        product == 'CFTC.13874A_F_L_ALL' ~ 'S&P 500 e-mini',
                                        product == 'CFTC.209747_F_ALL' ~ 'Nasdaq micro e-mini',
                                        product == 'CFTC.209747_F_L_ALL' ~ 'Nasdaq micro e-mini',
                                        product == 'CFTC.12460P_F_ALL' ~ 'DJIA e-mini',
                                        product == 'CFTC.12460P_F_L_ALL'  ~ 'DJIA e-mini',
                                        product == 'CFTC.239777_F_ALL' ~ 'Russell 2000',
                                        product == 'CFTC.239777_F_L_ALL' ~ 'Russell 2000',
                                        product == 'CFTC.240741_F_ALL' ~ 'Nikkei Stock Average',
                                        product == 'CFTC.240741_F_L_ALL' ~ 'Nikkei Stock Average',
                                        product == 'CFTC.1170E1_F_ALL' ~ 'VIX',
                                        product == 'CFTC.1170E1_F_L_ALL' ~ 'VIX',
                                        product == 'CFTC.T_F_ALL' ~ 'WTI Crude Oil',
                                        product == 'CFTC.T_F_L_ALL' ~ 'WTI Crude Oil',
                                        product == 'CFTC.RBB_F_ALL' ~ 'RBOB Gasoline Crack Spread',
                                        product == 'CFTC.RBB_F_L_ALL'  ~ 'RBOB Gasoline Crack Spread',
                                        product == 'CFTC.023651_F_ALL' ~ 'Natual Gas NYME',
                                        product == 'CFTC.023651_F_L_ALL' ~ 'Natual Gas NYME',
                                        product == 'CFTC.BH_F_ALL' ~ 'Heating Oil #2',
                                        product == 'CFTC.BH_F_L_ALL' ~ 'Heating Oil #2',
                                        product == 'CFTC.075651_F_ALL' ~ 'Palladium',
                                        product == 'CFTC.075651_F_L_ALL' ~ 'Palladium',
                                        product == 'CFTC.076651_F_ALL' ~ 'Platinum',
                                        product == 'CFTC.076651_F_L_ALL' ~ 'Platinum',
                                        product == 'CFTC.084602_F_ALL' ~ 'Silver Troy 1000',
                                        product == 'CFTC.084602_F_L_ALL' ~ 'Silver Troy 1000',
                                        product == 'CFTC.085692_F_ALL' ~ 'Copper',
                                        product == 'CFTC.085692_F_L_ALL'~ 'Copper',
                                        product == 'CFTC.088606_F_ALL' ~ 'Gold 100 Troy Oz',
                                        product == 'CFTC.088606_F_L_ALL' ~ 'Gold 100 Troy Oz',
                                        product == 'CFTC.001612_F_ALL' ~ 'Wheat HRW',
                                        product == 'CFTC.001612_F_L_ALL'  ~ 'Wheat HRW',
                                        product == 'CFTC.PYC1_F_ALL' ~ 'Corn',
                                        product == 'CFTC.PYC1_F_L_ALL' ~ 'Corn',
                                        product == 'CFTC.001602_F_ALL' ~ 'Wheat SRW',
                                        product == 'CFTC.001602_F_L_ALL'  ~ 'Wheat SRW',
                                        product == 'CFTC.007601_F_ALL' ~ 'Soybean Oil',
                                        product == 'CFTC.007601_F_L_ALL' ~ 'Soybean Oil',
                                        product == 'CFTC.CZ_F_ALL' ~ 'Soybean',
                                        product == 'CFTC.CZ_F_L_ALL' ~ 'Soybean',
                                        product == 'CFTC.SN_F_ALL' ~ 'Soybean Meal',
                                        product == 'CFTC.SN_F_L_ALL' ~ 'Soybean Meal',
                                        product == 'CFTC.083731_F_ALL' ~  'Coffee',
                                        product == 'CFTC.083731_F_L_ALL' ~  'Coffee',
                                        product == 'CFTC.033661_F_ALL' ~ 'Cotton',
                                        product == 'CFTC.033661_F_L_ALL' ~ 'Cotton',
                                        product == 'CFTC.080732_F_ALL' ~ 'Sugar No. 11',
                                        product == 'CFTC.080732_F_L_ALL' ~ 'Sugar No. 11',
                                        product == 'CFTC.040701_F_ALL' ~ 'Orange Juice',
                                        product == 'CFTC.040701_F_L_ALL' ~ 'Orange Juice',
                                        product == 'CFTC.073732_F_ALL' ~ 'Cocoa',
                                        product == 'CFTC.073732_F_L_ALL' ~ 'Cocoa',
                                        product == 'CFTC.054631_F_ALL' ~ 'Live Hogs',
                                        product == 'CFTC.054631_F_L_ALL' ~ 'Live Hogs',
                                        product == 'CFTC.061641_F_ALL' ~ 'Feeder Cattle',
                                        product == 'CFTC.061641_F_L_ALL' ~ 'Feeder Cattle',
                                        product == 'CFTC.057642_F_ALL' ~ 'Live Cattle',
                                        product == 'CFTC.057642_F_L_ALL' ~ 'Live Cattle',
                                        TRUE ~ 'Other')) %>%
            mutate(investor = case_when(grepl('Open Interest', report) ~ 'Open Interest',
                                        grepl('Dealer', report) ~ 'Dealer',
                                        grepl('Asset Manager', report) ~ 'Asset Manager',
                                        grepl('Leveraged Funds', report) ~ 'Leveraged Funds',
                                        grepl('Other Reportable', report) ~ 'Other Reportable',
                                        grepl('Total Reportable', report) ~ 'Total Reportable',
                                        grepl('Non Reportable', report) ~ 'Non Reportable (Small Trader)',
                                        grepl('Nonreportable', report) ~ 'Non Reportable (Small Trader)',
                                        grepl('Commercial', report) ~ 'Commercial (Hedger)',
                                        grepl('Noncommercial', report) ~ 'Non Commercial (Large Speculator)',
                                        grepl('Total', report) ~ 'Total Reportable',
                                        grepl('Producer/Merchant/Processor', report) ~ 'Producer/Merchant/Processer',
                                        grepl('Money Manager', report) ~ 'Money Manager',
                                        TRUE ~ 'Other')) %>%
            mutate(position = case_when(grepl('Open Interest', report) ~ 'Open Interest',
                                        grepl('Long', report) ~ 'Long',
                                        grepl('Short', report) ~ 'Short',
                                        grepl('Spread', report) ~ 'Spreads',
                                        TRUE ~ 'Other')) %>%
            filter(!investor %in% c('Non Reportable',
                                    'Total Reportable',
                                    'Open Interest') | product != 'CFTC.042601_F_ALL') %>%
            #filter(Date == '2023-08-08') %>% filter(contract == '2yr T Note') %>% View()
            mutate(`Open Interest` = case_when(report == 'Open Interest' ~ value,
                                               TRUE ~ NA)) %>%
            fill(`Open Interest`, .direction='down') %>%
            filter(report != 'Open Interest') %>%
            select(-report) %>%
            pivot_wider(names_from='position', values_from='value') %>%
            mutate(`Net Positioning Contracts` = Long - Short) %>%
            mutate(`Net Positioning Percent` = (Long - Short)/(`Open Interest`)) %>%
            group_by(investor, contract) %>%
            mutate(roll_mean_5 = slidify_vec(.x=`Net Positioning Percent`,
                                             .f=~mean(., na.rm=TRUE),
                                             .period=roll_window,
                                             .align='right')) %>%
            mutate(roll_sd_5 = slidify_vec(.x=`Net Positioning Percent`,
                                           .f=~sd(., na.rm=TRUE),
                                           .period=roll_window,
                                           .align='right')) %>%
            mutate(zscore_5yr = (`Net Positioning Percent` - roll_mean_5)/roll_sd_5) %>%
            #mutate(zscore_5yr = roll_zscore_5yr(`Net Positioning`)) %>%
            ungroup() %>%
            mutate(asset_class = case_when(grepl('Note', contract) | grepl('Bond', contract) | grepl('SOFR', contract) ~ 'Treasuries',
                                           grepl('Yen', contract) | grepl('Franc', contract) |
                                               grepl('Dollar',contract) | grepl('Peso', contract) |
                                               grepl('Euro', contract) | grepl('Pound', contract) ~ 'Currencies',
                                           grepl('mini', contract) | grepl('Stock', contract) |
                                               grepl('Russel', contract) ~ 'Equities',
                                           grepl('Soybean', contract) | grepl('Wheat', contract) | grepl('Corn', contract) ~'Grains',
                                           grepl('Coffee', contract) | grepl('Cotton', contract) |
                                               grepl('Sugar', contract)| grepl('Orange', contract) |
                                               grepl('Cocoa', contract) ~ 'Soft',
                                           grepl('Cattle', contract) | grepl('Hogs', contract) ~ 'Livestock',
                                           grepl('Palladium', contract) | grepl('Platinum', contract) |
                                               grepl('Silver', contract) | grepl('Gold', contract) |
                                               grepl('Copper', contract) ~ 'Metals',
                                           grepl('Oil', contract) | grepl('Gas', contract) ~ 'Energy',
                                           grepl('VIX', contract) ~ 'Volatility',
                                           TRUE ~ 'Other'))
        data
    })

    output$CoT_Breakdown_plot <- renderPlot({
        selected_measure <- input$CoT_measurement

        g <- CoT_data_tbl() %>%
            filter(contract %in% input$CoT_contract_select) %>%
            filter(asset_class %in% input$CoT_asset_class_select) %>%
            filter(investor %in% c('Commercial (Hedger)',
                                   'Non Commercial (Large Speculator)',
                                   'Non Reportable (Small Trader)')) %>%
            drop_na(selected_measure) %>%
            ggplot(., aes(x=Date, y=get(selected_measure), colour=get(selected_measure))) +
            geom_line() +
            scale_colour_gradient2(low = "#FF0000",
                                   mid = '#3CB043',
                                   high="#013220") +
            #facet_wrap(~investor, ncol=1, scales='free_x')
            facet_grid(investor~., space='free', scales='free_x')

        g
    })

    output$CoT_Market_Breakdown_plot <- renderPlot({
        selected_measure <- input$CoT_measurement2

        g <- CoT_data_tbl() %>%
            filter(contract %in% input$CoT_contract_select2) %>%
            filter(!investor %in% c('Commercial (Hedger)',
                                    'Non Commercial (Large Speculator)',
                                    'Non Reportable (Small Trader)')) %>%
            drop_na(selected_measure) %>%
            ggplot(., aes(x=Date, y=get(selected_measure), colour=get(selected_measure))) +
            geom_line() +
            scale_colour_gradient2(low = "#FF0000",
                                   mid = '#3CB043',
                                   high="#013220") +
            #facet_wrap(~investor, ncol=1, scales='free_x') +
            facet_grid(investor~., space='free', scales='free_x')
        g
    })



    output$CoT_Net_Position_plot <- renderPlot({
        g <- CoT_data_tbl() %>%
            filter(Date == input$CoT_net_position_date) %>%
            filter(investor %in% input$CoT_investor_type_select) %>%
            mutate(contract = as_factor(contract) %>% fct_reorder(., zscore_5yr)) %>%
            ggplot(., aes(x=zscore_5yr, y=contract, fill=zscore_5yr)) +
            geom_col(position='dodge', height=0.35) +
            scale_fill_gradient2(low = "#FF0000",
                                 mid = '#3CB043',
                                 high="#013220") +
            # facet_wrap(~asset_class, ncol=1, drop=TRUE, scales='free_y')
            facet_grid(asset_class~., space='free', scales='free')
        g
    })

    output$CoT_weekly_movers_plt <- renderPlot({
        g <-CoT_data_tbl() %>%
            filter(Date >= as.Date('2023-01-01')) %>%
            filter(investor %in% input$CoT_investor_type_select2) %>%
            arrange(desc(Date)) %>%
            group_by(contract) %>%
            mutate(weekly_move = -1*c(diff(zscore_5yr, lag=2), rep(NA,2))) %>%
            ungroup() %>%
            mutate(contract = as_factor(contract) %>% fct_reorder(., weekly_move)) %>%
            filter(Date == max(Date)) %>%
            ggplot(., aes(x=weekly_move, y=contract, colour=weekly_move)) +
            geom_point(size=5) +
            scale_colour_gradient2(low = "#FF0000",
                                   mid = '#3CB043',
                                   high="#013220") +
            facet_grid(asset_class~., space='free', scales='free')
        g
    })


  })
}

## To be copied in the UI
# mod_CoT_ui("CoT_1")

## To be copied in the server
# mod_CoT_server("CoT_1")
