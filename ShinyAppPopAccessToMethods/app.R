library(shiny)

library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plotly)

library(lubridate)
library(stringr)
library(stringi)

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to display women's access to methods indicators using PMA data 
# There are four parts in this document:
# 0. Database update 
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP 

#******************************
# 0. Database update 
#******************************

#setwd("C:/Users/YoonJoung Choi/Dropbox/0 iSquared/iSquared_PMA/Effective Access/ShinyAppPopAccessToMethods")
#Summary data constructed from Stata

dtaSDP<-read.csv("summary_Access_Indicators_SR.csv")%>%
    filter(xsurvey!="NENiameyR5" & xsurvey!="BFR6" & xsurvey!="NGKanoR3")%>%
    filter(country!="Niger, Niamey" & country!="Nigeria, Lagos" & country!="Nigeria, Kano")%>%
    group_by(country)%>%
    
    mutate(latest=round==max(round))%>%
    ungroup()%>%
    mutate(
        month = ifelse(month==0, 12, month), 
        yearmonth = ISOdate(year, month, 15),
        essential5_offer=round(essential5_offer, 0),
        essential5_curav=round(essential5_curav, 0),
        essential5_noso =round(essential5_noso, 0),
        essential5_ready=round(essential5_ready, 0),
        essential5_rnoso=round(essential5_rnoso, 0),
        
        essential5ec_offer=round(essential5ec_offer, 0),
        essential5ec_curav=round(essential5ec_curav, 0),
        essential5ec_noso =round(essential5ec_noso, 0),
        essential5ec_ready=round(essential5ec_ready, 0),
        essential5ec_rnoso=round(essential5ec_rnoso, 0),
        
        group=as.character(group),
        group=ifelse(group=="All", "All SDPs", group), 
        group=ifelse(group=="Excluding hospitals", "Lower-level SDPs", group) 
    )

    nrow(dtaSDP)
    length(unique(dtaSDP$country)) 
    length(unique(dtaSDP$xsurvey)) 
    table(dtaSDP$group)
    
dtaIR<-data.frame(read_csv("summary_Access_Indicators_IR_PopAccessToMethods.csv"))%>%
    filter(xsurvey!="NENiameyR5" & xsurvey!="BFR6" & xsurvey!="NGKanoR3")%>%
    filter(country!="Niger, Niamey" & country!="Nigeria, Lagos" & country!="Nigeria, Kano")%>%
    filter(year<=2018)%>%
    filter(group=="All")%>%
    
    mutate(month = ifelse(month==0, 12, month), 
           yearmonth = ISOdate(year, month, 15),
           flagSDPlow=( country=="Cote d'Ivoire" |
                        country=="DRC, Kinshasa" |
                        country=="DRC, Kongo Central" |
                        country=="Kenya" |
                        country=="Niger" 
                        )
           #replace with missing for the flasSDPlow countries 
           #mutate_at(vars(starts_with("SDPlow")), funs(.=ifelse(flagSDPlow==TRUE,NA,.)))
           #SDPlow_essential5_offer=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5_offer),
           #SDPlow_essential5_curav=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5_curav),
           #SDPlow_essential5_ready=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5_ready),
           #SDPlow_essential5_rnoso=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5_rnoso),
           #SDPlow_essential5ec_offer=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5ec_offer),
           #SDPlow_essential5ec_curav=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5ec_curav),
           #SDPlow_essential5ec_ready=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5ec_ready),
           #SDPlow_essential5ec_rnoso=ifelse(flagSDPlow==TRUE,NA,SDPlow_essential5ec_rnoso)
           
           )%>%
    group_by(country)%>%
    mutate(latestIRLINK=round==max(round))%>%
    ungroup()

    nrow(dtaIR)
    length(unique(dtaIR$country)) 
    length(unique(dtaIR$xsurvey)) 
    table(dtaIR$group)
    table(dtaIR$xsurvey, dtaIR$latestIRLINK)
    
nsurvey<-length(unique(dtaSDP$country)) 
ncountry<-length(unique(dtaSDP$xsurvey)) 

countrylist<-unique(as.vector(dtaSDP$country))
methodslist<-c("Five methods", 
               "Five methods + EC")

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    headerPanel("Changing perspectives: translating methods availability to women's access to methods"),

    # Title panel 
    titlePanel("Application to explore metrics of women's access to contraceptive methods"),

    # Side panel: define input and output   
    sidebarLayout(
        fluid = TRUE,
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            style = "position:fixed;width:inherit;", 
            width = 3,
            h4(strong("Provide inputs for Parts 3 & 4")),
            br(),
            selectInput("country", 
                        "Select a country",
                        choices = countrylist, 
                        selected = "Burkina Faso"), 
            br(),
            br(),
            br(),
            selectInput("methods", 
                        "Select 'a range of methods'",
                        choices = methodslist, 
                        selected = "Five methods*"), 
            h6("Note: Five methods are IUD, implants, injectables, pills, and male condoms.")

        ),
        
        # Main page for output display 
        mainPanel(
            width = 8,
            
            tabsetPanel(type = "tabs",
                        
                tabPanel("1. Introduction",       
                         
                    h4("Access to a range of methods is a necessary, though insufficient, foundation for contraceptive choice - i.e., using methods of choice without barriers to access them. But, few metrics have been developed to capture this concept."),
                    h4("Using PMA's data from both household and the linked SDP surveys, we develop novel metrics to measure and understand access to methods from women's perspectives.",
                        "More information about the methodology can be found in a", a(strong("brief (forthcoming)"),href=""), 
                        "and in a", a(strong("technical report (forthcoming)"),href="") ),
                    hr(),
                    h4(strong("This interactive application is to explore the metrics using data from nine select countries.")),
                    br(),     
                    h4("There are four parts:"), 
                    h4("1. Introduction"),
                    h4("2. Measures, using Uganda data as an example"), 
                    h4("3. Latest levels in nine countries"), 
                    h4("4. Trends in nine countries"), 
                    br(), 
                    h4("In Parts 3 & 4, provide inputs on the left panel to explore results by:"), 
                    h4("-- country"), 
                    h4("-- definition of 'a range of methods'"), 
                    hr(),
                    h6("See", a("GitHub",href="https://github.com/yoonjoung/PMA_Access"),"for more information."),
                    h6("Application last updated on August 5, 2020"),
                    h6("For typos, errors, and questions:", a("contact YJ Choi at www.iSquared.global",href="https://www.isquared.global/YJ"))
                ), 
                
                tabPanel("2. Measures",                    
                    h4(strong("Women are considered to have access to a range of methods, if a specific set of methods are available at one or more SDPs serving her community"), 
                       "(i.e., sample cluster in the household survey) - hereinafter referred to as 'accessible SDPs'.",
                       "For more information on how PMA sample SDPs,",
                       a("see section 2.1 in this report.",href="https://www.pma2020.org/sites/default/files/SDP_report_2019-03-12-final.pdf")),
                    h4("This measure is influences by:"), 
                    h4("--", strong("how we define availability of methods at SDPs")),
                    h4("--", strong("what and how many methods are included")),
                    h4("--", strong("what SDPs are considered 'accessible' from women's perspectives")),
                    h4("Below, we explore how estimates of women's access are affected by different definitions, using data from", strong("Uganda PMA 2018.")),
                    
                    hr(),
                    h4(span("First, in all figures, we use four gradually restrictive definitions of 'availability'.", style = "color:blue")), 
                    h4("-- SDP",em("offers all specified methods")),
                    h4("-- SDP offers all specified methods and",em("has all methods in stock currently")),
                    h4("-- SDP offers all specified methods, has all methods in stock currently, and",em("is ready to provide IUDs/implant services"), "(i.e., has trained personnel and all supplies/equipment available to insert and remove IUDs/implants)"), 
                    h4("-- SDP offers all specified methods, has all methods in stock currently, is ready to provide IUD/implant services, and",em("has no history of stock-out for any of the methods in the past three months")), 
                    h4("The stricter definitions (darker shade bars in below figures), the lower availability."), 
                    
                    hr(),
                    h4(span("Second, we explore two sets of specific methods.", style = "color:blue")),
                    h4("-- Select five* methods: IUD, implants, injectables, pills, and male condoms"),  
                    h4("-- Five methods + emergency contraception (EC)"),  
                    h4("Figure 2.1. Availability of a range of methods among SDPs,", strong("by definition of the methods")),  
                    plotlyOutput("plot_UG_methods"),   
                    br(),
                    h4(strong("Key summary:"),"The more number of methods, the lower availability at SDPs. The amount of reduction varies across countries (See Part 3)."),
                    
                    hr(),
                    h4(span("Finally, we compare two definitions of 'accessible SDPs'.", style = "color:blue")), 
                    h4("-- All SDPs: public primary, secondary, and tertiary level SDPs that are designated to serve the community + private SDPs within the community"),  
                    h4("-- Lower-level SDPs: all SDPs minus tertiary level hospitals."),  
                    br(),
                    h4("Figure 2.2. Availability of the five methods among SDPs,", strong("by level of SDPs")),
                    plotlyOutput("plot_UG_SDPtype_SDP"),
                    br(),
                    h4(strong("Key summary:"),"Method availability is lower among lower-level SDPs than among all SDPs. The magnitude of differences varies across countries (See Part 3)."),
                    br(),
                    h4("Figure 2.3. Women's access to the five methods,", strong("by definition of accessible SDPs")),
                    plotlyOutput("plot_UG_SDPtype_women"),                    
                    br(),
                    h4(strong("Key summary:")),
                    h4("1. Women's access to methods is higher than methods availability at the SDP-level."),
                    h4("2. Women's access to methods reduces when only lower-level SDPs are considered 'accessible'. The magnitude of differences varies across countries (See Part 3)."),
                    
                    hr(),
                    h6("FOOTNOTE"),
                    h6("*In certain settings, not all of the five essential methods have been introduced. In such case, specifically in Rajasthan, India, in this report, availability of the five methods is negligible. Thus, for the purpose of methodological exploration, we apply a modified definition of the essential methods (i.e., four methods: IUD, injectables, pills, and male condoms) to Rajasthan, India.")
                ),
                
                tabPanel("3. Latest levels",         
                    
                    h4("The following output is latest level in:"),
                    verbatimTextOutput("text_source"),
                    
                    hr(),
                    h4("Figure 3.1. Availability of methods among SDPs,", strong("by level of SDPs")),    
                    plotlyOutput("plot_SDP_level"), 
                    
                    hr(),
                    h4("Figure 3.1. Women's access to methods,", strong("by definition of accessible SDPs")),
                    plotlyOutput("plot_women_level") 
                ),
          
                tabPanel("4. Trends",           

                    h4("The following output is trends, using the five methods, in:"),
                    verbatimTextOutput("text_country"),
                    
                    hr(),
                    h4("Figure 4.1. Availability of the five methods among SDPs,", strong("by level of SDPs")),    
                    plotlyOutput("plot_SDP_trend"), 
                    
                    hr(),
                    h4("Figure 4.2. Women's access to the five methods,", strong("by definition of accessible SDPs")),
                    plotlyOutput("plot_women_trend") 
                )
            )
        )
    )
)


#******************************
# 2. SERVER
#******************************

server<-function(input, output) {

    ##### text output of inputs #####
    
    output$text_country <- renderText({
        paste(input$country) 
        })    
    output$text_methods <- renderText({
        paste(input$methods) 
        })        
    output$text_source <- renderText({
        #source<-dataselected()[1,]  
        #paste(source$country, source$type, source$year, sep = " ")
        paste(input$country, ", using ", input$methods, sep = "")
        })    

    ##### output: Part 2 #####
    
    output$plot_UG_methods <- renderPlotly({
        
        temp<-dtaSDP%>%filter(group=="All SDPs")%>%filter(latest==TRUE)%>%filter(country=="Uganda")
        
        temp1<-temp%>%select(ends_with("offer"))%>%
            gather(key=methods,value=offer)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 1))
        temp2<-temp%>%select(ends_with("curav"))%>%
            gather(key=methods,value=curav)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 1))
        temp3<-temp%>%select(ends_with("ready"))%>%
            gather(key=methods,value=ready)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 1))
        temp4<-temp%>%select(ends_with("rnoso"))%>%
            gather(key=methods,value=rnoso)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 1))
        
        dtafig<-join_all(list(temp1, temp2, temp3, temp4), by='methods', type='left')%>%
            mutate(
                xsurvey="",
                methods=ifelse(methods=="essential5", "Five methods", methods), 
                methods=ifelse(methods=="essential5ec", "Five methods + EC", methods) 
            )

        panel <- . %>% 
            plot_ly(x=~xsurvey, y=~offer, type = 'bar', 
                        name = "methods offered", 
                        marker = list(color = '#fdd0a2'),
                        text = ~offer, textfont = list(size = 10, color="black"), textposition = 'outside')%>%
            add_trace(y=~curav, 
                        name = "+ currently in-stock",
                        marker = list(color = '#fdae6b'),
                        text = ~curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~ready, 
                        name = "+ ready to insert/remove implants and IUDs", 
                        marker = list(color = '#f16913'),
                        text = ~ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~rnoso, 
                        name = "+ no stock-out in the past 3 months", 
                        marker = list(color = '#d94801'),
                        text = ~rnoso, textfont = list(size = 10) , textposition = 'outside') %>%    
            add_annotations(
                text = ~unique(methods),
                x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12))
                )
        
        dtafig%>%group_by(methods)%>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = TRUE)%>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12)),
                legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5))
    })    

    output$plot_UG_SDPtype_SDP <- renderPlotly({
        
        dtafig<-dtaSDP%>%filter(latest==TRUE)%>%filter(country=="Uganda")%>%mutate(xsurvey="")

        panel <- . %>% 
            plot_ly(x=~xsurvey, y=~essential5_offer, type = 'bar', 
                        name = "Five methods offered",
                        marker = list(color = '#fdd0a2'),
                        text = ~essential5_offer, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y=~essential5_curav, 
                        name = "+ currently in-stock",
                        marker = list(color = '#fdae6b'),
                        text = ~essential5_curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~essential5_ready, 
                        name = "+ ready to insert/remove implants and IUDs",  
                        marker = list(color = '#f16913'),
                        text = ~essential5_ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~essential5_rnoso, 
                        name = "+ no stock-out in the past 3 months",
                        marker = list(color = '#d94801'),
                        text = ~essential5_rnoso, textfont = list(size = 10) , textposition = 'outside') %>%     
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16) )%>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=8)),
                xaxis = list(title = "Survey year", tickfont = list(size=8))
                )
        
        dtafig%>%group_by(group)%>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = TRUE)%>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12)),
                legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5))
    })    
    
    output$plot_UG_SDPtype_women <- renderPlotly({
        
        dtafig<-dtaIR%>%filter(groupdemand==0)%>%
            filter(group=="All" & latestIRLINK==1)%>%
            filter(is.na(SDPall_essential5_noso)==FALSE)%>%
            filter(country=="Uganda")%>%mutate(xsurvey="")
        
        fig1UG<-dtafig%>% 
            plot_ly(x=~xsurvey, y=~SDPall_essential5_offer, type = 'bar', 
                        name = "Five methods offered",
                        marker = list(color = '#9ecae1'),
                        text = ~SDPall_essential5_offer, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y=~SDPall_essential5_curav, type = 'bar', 
                        name = "+ currently in-stock",
                        marker = list(color = '#6baed6'),
                        text = ~SDPall_essential5_curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~SDPall_essential5_ready, 
                              name = "+ ready to insert/remove implants and IUDs", 
                              marker = list(color = '#2171b5'),
                              text = ~SDPall_essential5_ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~SDPall_essential5_rnoso, 
                              name = "+ no stock-out in the past 3 months", 
                              marker = list(color = '#084594'),
                              text = ~SDPall_essential5_rnoso, textfont = list(size = 10) , textposition = 'outside') %>%  
            add_annotations(
                text = "All linked SDPs",
                x = 0.5, y = 0.92, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%
            layout(
                yaxis = list(title = "Percent of women",
                                     range = c(0, 110), tickfont = list(size=8)),
                xaxis = list(title = "Survey year", tickfont = list(size=8)),
                showlegend = FALSE 
                )
        
        fig2UG<-dtafig%>%
            plot_ly(x=~xsurvey, y=~SDPlow_essential5_offer, type = 'bar', 
                        name = "Five methods offered",
                        marker = list(color = '#9ecae1'),
                        text = ~SDPlow_essential5_offer, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y=~SDPlow_essential5_curav, type = 'bar', 
                        name = "+ currently in-stock",
                        marker = list(color = '#6baed6'),
                        text = ~SDPlow_essential5_curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~SDPlow_essential5_ready, 
                              name = "+ ready to insert/remove implants and IUDs", 
                              marker = list(color = '#2171b5'),
                              text = ~SDPlow_essential5_ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~SDPlow_essential5_rnoso, 
                              name = "+ no stock-out in the past 3 months", 
                              marker = list(color = '#084594'),
                              text = ~SDPlow_essential5_rnoso, textfont = list(size = 10) , textposition = 'outside') %>%     
            add_annotations(
                text = "Lower-level linked SDPs",
                x = 0.5, y = 0.92, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%
            layout(
                yaxis = list(title = "Percent of women",
                                     range = c(0, 110), tickfont = list(size=12)),
                xaxis = list(title = "Survey year", tickfont = list(size=12))
                )
        
        subplot(fig1UG, fig2UG, nrows=1, shareY = TRUE) %>%
            layout( 
                    yaxis = list(title = "Percent of women",
                                     range = c(0, 110), tickfont = list(size=12)),
                    legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5)
                    )

    })    

    ##### output: Part 3 #####
    
    output$plot_SDP_level <- renderPlotly({
        
        temp<-dtaSDP%>%filter(country==input$country)%>%filter(latest==TRUE)%>%arrange(group)
        
        temp1<-temp%>%select(ends_with("offer"))%>%
            gather(key=methods,value=offer)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 1))
        temp2<-temp%>%select(ends_with("curav"))%>%
            gather(key=methods,value=curav)%>%
            select(-methods)
        temp3<-temp%>%select(ends_with("ready"))%>%
            gather(key=methods,value=ready)%>%
            select(-methods)
        temp4<-temp%>%select(ends_with("rnoso"))%>%
            gather(key=methods,value=rnoso)%>%
            select(-methods)
        
        dtafig<-cbind(temp1, temp2, temp3, temp4)%>%
            select(methods, offer, curav, ready, rnoso)%>%
            mutate(
                methods=ifelse(methods=="essential5", "Five methods", methods), 
                methods=ifelse(methods=="essential5ec", "Five methods + EC", methods),
                xsurvey="",
                group="All SDPs"
            )
        
        dtafig$group[2]<-"Lower-level SDPs"
        dtafig$group[4]<-"Lower-level SDPs"
        
        dtafig<-dtafig%>%
            filter(methods==input$methods)
        
        panel <- . %>% 
            plot_ly(x=~xsurvey, y=~offer, type = 'bar', 
                        name = "methods offered", 
                        marker = list(color = '#fdd0a2'),
                        text = ~offer, textfont = list(size = 10, color="black"), textposition = 'outside')%>%
            add_trace(y=~curav, 
                        name = "+ currently in-stock",
                        marker = list(color = '#fdae6b'),
                        text = ~curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~ready, 
                        name = "+ ready to insert/remove implants and IUDs", 
                        marker = list(color = '#f16913'),
                        text = ~ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~rnoso, 
                        name = "+ no stock-out in the past 3 months", 
                        marker = list(color = '#d94801'),
                        text = ~rnoso, textfont = list(size = 10) , textposition = 'outside') %>%    
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12))
                )

        dtafig%>%group_by(group)%>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = TRUE)%>%
            layout(
                yaxis = list(title = "Percent of SDPs",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12)),
                legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5))
    })    
    
    output$plot_women_level <- renderPlotly({
                      
        panel <- . %>% 
            plot_ly(x=~xsurvey, y=~offer, type = 'bar', 
                        name = "methods offered", 
                        marker = list(color = '#9ecae1'),
                        text = ~offer, textfont = list(size = 10, color="black"), textposition = 'outside')%>%
            add_trace(y=~curav, 
                        name = "+ currently in-stock",
                        marker = list(color = '#6baed6'),
                        text = ~curav, textfont = list(size = 10) , textposition = 'outside') %>%
            add_trace(y = ~ready, 
                        name = "+ ready to insert/remove implants and IUDs", 
                        marker = list(color = '#2171b5'),
                        text = ~ready, textfont = list(size = 10) , textposition = 'outside') %>% 
            add_trace(y = ~rnoso, 
                        name = "+ no stock-out in the past 3 months", 
                        marker = list(color = '#084594'),
                        text = ~rnoso, textfont = list(size = 10) , textposition = 'outside') %>%   
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%
            layout(
                yaxis = list(title = "Percent of women",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12))
                )

        #All SDPS
        temp<-dtaIR%>%filter(country==input$country)%>%
            filter(groupdemand==0 & grouplabel=="All" & latestIRLINK==1)%>%
            filter(is.na(SDPall_essential5_noso)==FALSE)%>%
            select(starts_with("SDPall_"))
        
        temp1<-temp%>%select(ends_with("offer"))%>%
            gather(key=methods,value=offer)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 2))
        temp2<-temp%>%select(ends_with("curav"))%>%
            gather(key=methods,value=curav)%>%
            select(-methods)
        temp3<-temp%>%select(ends_with("ready"))%>%
            gather(key=methods,value=ready)%>%
            select(-methods)
        temp4<-temp%>%select(ends_with("rnoso"))%>%
            gather(key=methods,value=rnoso)%>%
            select(-methods)
        
        dtafig<-cbind(temp1, temp2, temp3, temp4)%>%
            select(methods, offer, curav, ready, rnoso)%>%
            mutate(
                methods=ifelse(methods=="essential5", "Five methods", methods), 
                methods=ifelse(methods=="essential5ec", "Five methods + EC", methods),
                xsurvey="",
                group="All SDPs"
            )

        dtafig<-dtafig%>%
            filter(methods==input$methods)
        
        fig1<-dtafig%>%group_by(group)%>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = TRUE)%>%
            layout(
                yaxis = list(title = "Percent of women",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12)),
                legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5))
        
        #Lower-level SDPS
        temp<-dtaIR%>%filter(country==input$country)%>%
            filter(groupdemand==0 & grouplabel=="All" & latestIRLINK==1)%>%
            filter(is.na(SDPall_essential5_noso)==FALSE)%>%
            select(starts_with("SDPlow_"))
        
        temp1<-temp%>%select(ends_with("offer"))%>%
            gather(key=methods,value=offer)%>%
            mutate(methods=sapply(strsplit(methods,"_"), `[`, 2))
        temp2<-temp%>%select(ends_with("curav"))%>%
            gather(key=methods,value=curav)%>%
            select(-methods)
        temp3<-temp%>%select(ends_with("ready"))%>%
            gather(key=methods,value=ready)%>%
            select(-methods)
        temp4<-temp%>%select(ends_with("rnoso"))%>%
            gather(key=methods,value=rnoso)%>%
            select(-methods)
        
        dtafig<-cbind(temp1, temp2, temp3, temp4)%>%
            select(methods, offer, curav, ready, rnoso)%>%
            mutate(
                methods=ifelse(methods=="essential5", "Five methods", methods), 
                methods=ifelse(methods=="essential5ec", "Five methods + EC", methods),
                xsurvey="",
                group="Lower-level SDPs"
            )

        dtafig<-dtafig%>%
            filter(methods==input$methods)
        
        fig2<-dtafig%>%group_by(group)%>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = TRUE)%>%
            layout(
                yaxis = list(title = "Percent of women",
                                     range = c(0, 100), tickfont = list(size=12)),
                xaxis = list(title = "", tickfont = list(size=12)),
                legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5))
        #subplot
        subplot(fig1, fig2, nrows=1, shareY = TRUE) %>%
            layout( 
                    yaxis = list(title = "Percent of women",
                                     range = c(0, 110), tickfont = list(size=12)),
                    legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5)
                    )
    })    
    
    ##### output: Part 4 #####
    
    output$plot_SDP_trend <-renderPlotly({
    
        panel <- . %>%
            plot_ly(x=~yearmonth, y=~essential5_offer, type = 'scatter', mode = 'lines',
                    name = "5 methods offered", 
                    marker = list(color = '#fdd0a2'),
                    line = list(color = '#fdd0a2')) %>%
            add_trace(y=~essential5_curav, type = 'scatter', mode = 'lines',
                    name = "+ currently in-stock",
                    marker = list(color = '#fdae6b'),
                    line = list(color = '#fdae6b')) %>%
            add_trace(y = ~essential5_ready, type = 'scatter', mode = 'lines',
                      name = "+ ready to insert/remove implants and IUDs", 
                      marker = list(color = '#f16913'),
                      line = list(color = '#f16913')) %>% 
            add_trace(y = ~essential5_rnoso, type = 'scatter', mode = 'lines',
                      name = "+ no stock-out in the past 3 months", 
                      marker = list(color = '#d94801'),
                      line = list(color = '#d94801')) %>% 
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.92, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%    
            layout(
                    yaxis = list(title = "Percent of SDPs",
                                 range = c(0, 100), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12))
                    )
        
        dtafig<-dtaSDP%>%filter(country==input$country)
        
        dtafig%>%
            group_by(group) %>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareX=TRUE, shareY = FALSE)%>%
            layout(
                    yaxis = list(title = "Percent of SDPs",
                                 range = c(0, 100), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                tickfont = list(size=12)),
                    legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5)
                )
    }) 
    
    output$plot_women_trend <-renderPlotly({
        ############### IR: SDPall
        
        panel <- . %>%
            plot_ly(x=~yearmonth, y=~SDPall_essential5_offer, type = 'scatter', mode = 'lines',
                    name = "5 methods offered",
                    marker = list(color = '#9ecae1'),
                    line = list(color = '#9ecae1')) %>%
            add_trace(y=~SDPall_essential5_curav, type = 'scatter', mode = 'lines',
                    name = "+ currently in-stock",  
                    marker = list(color = '#6baed6'),
                    line = list(color = '#6baed6')) %>%
            add_trace(y = ~SDPall_essential5_ready, type = 'scatter', mode = 'lines',
                      name = "+ read to insert/remove IUDs/implants", 
                      marker = list(color = '#2171b5'),
                      line = list(color = '#2171b5')) %>% 
            add_trace(y = ~SDPall_essential5_rnoso, type = 'scatter', mode = 'lines',
                      name = "+ no stock-out in the past 3 months", 
                      marker = list(color = '#084594'),
                      line = list(color = '#084594')) %>% 
            add_annotations(
                text = "All SDPs",
                x = 0.5, y = 0.92, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
                ) %>%    
            layout(
                    yaxis = list(title = "",
                                 range = c(0, 110), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12))
                    )
        
        dtafig<-dtaIR%>%filter(groupdemand==0)%>%
            filter(group=="All" )%>%
            filter(is.na(SDPall_essential5_noso)==FALSE)%>%
            filter(country==input$country)
        
        fig1<-dtafig%>%
            group_by(country) %>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = FALSE)%>%
            layout(
                    yaxis = list(title = "",
                                 range = c(0, 110), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12))                
            )
        
        ############### IR: SDPlow
        
        panel <- . %>%
            plot_ly(x=~yearmonth, y=~SDPlow_essential5_offer, type = 'scatter', mode = 'lines',
                    name = "5 methods offered",
                    marker = list(color = '#9ecae1'),
                    line = list(color = '#9ecae1')) %>%
            add_trace(y=~SDPlow_essential5_curav, type = 'scatter', mode = 'lines',
                    name = "+ currently in-stock",  
                    marker = list(color = '#6baed6'),
                    line = list(color = '#6baed6')) %>%
            add_trace(y = ~SDPlow_essential5_ready, 
                      name = "+ read to insert/remove IUDs/implants", 
                      marker = list(color = '#2171b5'),
                      line = list(color = '#2171b5')) %>% 
            add_trace(y = ~SDPlow_essential5_rnoso, 
                      name = "+ no stock-out in the past 3 months", 
                      marker = list(color = '#084594'),
                      line = list(color = '#084594')) %>% 
            add_annotations(
                text = "Lower-level SDPs",
                x = 0.5, y = 0.92, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 16)
                ) %>%    
            layout(
                    yaxis = list(title = "",
                                 range = c(0, 110), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12))
                    )
                
        dtafig<-dtaIR%>%filter(groupdemand==0)%>%
            filter(group=="All" )%>%
            filter(is.na(SDPlow_essential5_noso)==FALSE)%>%
            filter(country==input$country)
        
        fig2<-dtafig%>%
            group_by(country) %>%
            do(p = panel(.)) %>%
            subplot(nrows = 1, shareY = FALSE)%>%
            layout(
                    yaxis = list(title = "",
                                 range = c(0, 110), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12))
                    )
        
        #subplot
        subplot(fig1, fig2, nrows=1, shareX=TRUE, shareY = FALSE) %>%
            layout( 
                    yaxis = list(title = "Percent of women",
                                     range = c(0, 110), tickfont = list(size=12)),
                    xaxis = list(title = "Survey year", 
                                 range=c(as.character("2014-06-15"),as.character("2019-12-15")),
                                  tickfont = list(size=12)),
                    legend = list(font=list(size=12), 
                              orientation="v", 
                              xanchor = "left", yanchor = "center", 
                              x = 1.01, y = 0.5)
                    )
        
    })

}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)