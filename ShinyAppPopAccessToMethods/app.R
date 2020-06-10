library(shiny)

library(plyr)
library(dplyr)
library(tidyverse)
library(plotly)
library(lubridate)

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to display access indicators using PMA data 
# There are four parts in this document:
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP 

#******************************
# 0. Database update 
#******************************

#setwd("C:/Users/YoonJoung Choi/Dropbox/0 iSquared/iSquared_PMA/Effective Access/ShinyAppPopAccessToMethods")

dtaIR<-data.frame(read_csv("summary_Access_Indicators_IR.csv"))%>%
    mutate(yearmonth = ISOdate(year, month, 15),
           #yearmonth = as.POSIXct(as.numeric(yearmonth), origin="1970-01-01"), 
           #yearmonth = substr(as.character(yearmonth),0,7), 
           latestIRLINK=latestIR,
           latestIRLINK=ifelse(xsurvey=="BFR5", 1, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="BFR6", 0, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="BFR7", 0, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="KER7", 1, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="KER8", 0, latestIRLINK), 
           
           linkissue=(country=="Nigeria, Kano" | country=="Nigeria, Lagos" | country=="India, Rajasthan" | 
                      xsurvey=="NGKanoR3" | xsurvey=="BFR6"),
           SDPall_essential5_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready), 
           SDPall_essential5ec_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5ec_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5ec_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready) 
           
    )

dtaCR<-data.frame(read_csv("summary_Access_Indicators_CR.csv"))%>%
    mutate(yearmonth = ISOdate(year, month, 15)
           #yearmonth = substr(as.character(yearmonth),0,7)
    )

    table(dtaIR$xsurvey)
    table(dtaCR$xsurvey)
    
    table(dtaIR$group)
    table(dtaCR$group)
    
    table(dtaIR$grouplabel)
    table(dtaCR$grouplabel)    

    table(dtaIR$yearmonth)
    table(dtaCR$yearmonth)

dta<-rbind.fill(dtaIR, dtaCR)

    dim(dtaIR)
    dim(dtaCR)    
    dim(dta)
    
countrylist<-unique(as.vector(dta$country))

    #countrylist<-countrylist[!is.na(countrylist)]
    #countrylist<-c("Burkina Faso", "Ethiopia", "Kenya", "Uganda")    

surveylist<-unique(as.vector(dta$xsurvey))

grouplistIR<-c("HH wealth", "Education", "Residential area", "Education x Residence")
grouplistCR<-c("Education of clients", "Age of clients")

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    #headerPanel("Potential indicators for access"),

    # Title panel 
    titlePanel("Application to internally explore potential indicators for access (DO NOT CIRCULATE the link since it has non-public survey data)"),

    # Side panel: define input and output   
    sidebarLayout(
        fluid = TRUE,
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            style = "position:fixed;width:inherit;", 
            width = 3,
            br(),
            h5(strong("Provide input below")),                        
            br(),
            selectInput("country", 
                        "Select a country",
                        choices = countrylist, 
                        selected = "Kenya"),                  
            br(),
            h5("Sections A: Levels of indicators"),
            h5("(No input needed)"),
            br(),
            h5("Sections B: Trends of indicators"),    
            h5("(No input needed)"),
            br(),
            h5("Section C: Pattern of indicators"),
            selectInput("groupIR", 
                        "Select background characteristics to assess disparity among all women (all elements)",
                        choices = grouplistIR, 
                        selected = "Education"), 
            br(),
            h5("Section D: MCPR by indicators"),
            h5("(No input needed)")
            
        ),
        
        # Main page for output display 
        mainPanel(
            width = 8,
            
            tabsetPanel(type = "tabs",
                tabPanel("Introduction", 
                    h5("This interactive app presents levels, trends, and patterns of potential access indicators.",
                        "It also presents MCPR by status of each indicator."), 
                    br(), 
                    h5("Provide inputs on the left panel."), 
                    h5("Hover over figures for estimates and other functions"),  
                    
                    hr(),
                    h6("See", a("GitHub",href="https://github.com/yoonjoung/PMA_Access"),"for more information."),
                    h6("Application last updated on June 9, 2020"),
                    h6("For typos, errors, and questions:", a("contact me",href="https://www.isquared.global/YJ"))
                ),
                  
                tabPanel("5. Service Quality", 
                    h4(strong("5. Service quality")),    
                    h4(strong("5.1. Service quality - from female survey")),
                    h5(""), 
                    h5(" - Left panel: MII"),
                    h5(" - Right panel:", strong("geographic and administrative access to methods")), 
                    h5(""), 
                    h5("For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1IgCkBOL_nTEv2KDsYl7Wweeak8RbmVeu/view?usp=sharing")),
                    h5("*Five specific modern methods: IUD, implant, injectables, pills, & male condom (except in India, where implant is not yet widely available)"),    
                    h5("**Ready: having commodity, trained personnel, & equipment. Applicable only for IUD and implant"),    
                    h5(""),    
                    h5("***Percent of all women with one or more linked SDPs meeting the service environment criteria (geographic and administrative access to methods/readiness)"),  
                    h5(strong("Note 1"),": For these cluster-level service environment indicators, linked-EA variables are required.", 
                       "The linked-EA variables are being created for Phase 1 surveys currently, and thus all phase 2 surveys are not included for the indicators.", 
                       strong("Thus, latest surveys for MII indicators (green bars) and EA-level service-environment indicators (blue bars) can be different in a country.")),
                    h5(strong("Note 2"),": Several surveys (i.e., BFR6, KanoR3, NiameyR5) are being further assessed for EA-SDP linkage results", 
                       a("(see here)", href="https://rpubs.com/YJ_Choi/PMA_EA_SDP_Link"), ", and are excluded in this report."),
                        
                    br(),    
                    h5(strong("5.1.A. Level of indicators in the latest survey")),
                    plotlyOutput("plot_level_qualityIR"),   
                    
                    br(), 
                    h5(strong("5.1.B. Trends of indicators in the country")),
                    plotlyOutput("plot_trend_qualityIR"),   
        
                    br(), 
                    h5(strong("5.1.C. SES pattern of indicators in the latest survey")),    
                    h5("As we discussed earlier, there is not much typical differences by individual SES background, partially because these are either among current users (green bars) or cluster-level service environment (blue bars)."),      
                    h5("For year of the latest survey, see 5.1.A."),    
                    plotlyOutput("plot_pattern_qualityIR1"),   
                    plotlyOutput("plot_pattern_qualityIR2"), 
                    
                    br(), 
                    h5(strong("5.1.D. MCPR by indicators in the latest survey")),   
                    h5(strong("Question 1"),": reverse causality?"), 
                    h5(strong("Question 2"),": Quality as rights, then do we care for any association here?"), 
                    h5("For year of the latest survey, see 5.1.A."),    
                    plotlyOutput("plot_mcpr_qualityIR")   
                                               
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
    output$text_groupIR <- renderText({
        paste(input$groupIR) 
        })        
    output$text_groupCR <- renderText({
        paste(input$groupCR) 
        })        

    ##### output: Service quality: IR #####
    
    output$plot_level_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(xmii_side)==FALSE & group=="All" & groupdemand==0)%>%    
            filter(round==max(round))
    
        fig1<-plot_ly(dtafig, x=~year, y=~xmii_side, type = 'bar', 
                name = "MII: side effect", marker = list(color = 'rgb(199,233,192)'),
                text = ~xmii_side, textposition = 'outside') %>%
            add_trace(y = ~xmii_sidewhat, name = "MII: what to do", 
                      marker = list(color = 'rgb(161,217,155)'),
                      text = ~xmii_sidewhat, textposition = 'outside') %>% 
            add_trace(y = ~xmii_other, name = "MII: other methods", 
                      marker = list(color = 'rgb(116,196,118)'),
                      text = ~xmii_other, textposition = 'outside') %>% 
            add_trace(y = ~xmii_switch, name = "MII: switch (NEW)", 
                      marker = list(color = 'rgb(65,171,93)'),
                      text = ~xmii_switch, textposition = 'outside') %>% 
            add_trace(y = ~xmii3, name = "MII: all three items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      text = ~xmii3, textposition = 'outside') %>% 
            add_trace(y = ~xmii4, name = "MII: all four items (NEW)", 
                      marker = list(color = 'rgb(0,109,44)'),
                      text = ~xmii4, textposition = 'outside') %>% 
            layout(
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year")
            )
        
        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group=="All" & groupdemand==0)%>%
            filter(round==max(round))
        
        fig2<-plot_ly(dtafig, x=~year, y=~SDPall_essential5_noso, type = 'bar', 
                name = "5 specific methods* available currently/3-month", marker = list(color = 'rgb(158,202,225)'),
                text = ~SDPall_essential5_noso, textposition = 'outside') %>%
            add_trace(y = ~SDPall_essential5_ready, name = "Currently ready** to provide 5 specific methods*", 
                      marker = list(color = 'rgb(107,174,214)'),
                      text = ~SDPall_essential5_ready, textposition = 'outside') %>% 
            add_trace(y = ~SDPall_essential5_rnoso, name = "Meeting both conditions", 
                      marker = list(color = 'rgb(33,113,181)'),
                      text = ~SDPall_essential5_rnoso, textposition = 'outside') %>% 
            layout(
                yaxis = list(title = "Percent of all women with service environment***",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year")
            )
        
        subplot(fig1, fig2, nrows=1, margin=0.04, titleY = TRUE) %>%
        layout( legend=list(orientation = 'v'), 
                yaxis = list(range = c(0, 100)) )
        
        })
    
    output$plot_trend_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" & groupdemand==0)
    
        fig1<-dtafig%>%
            plot_ly(x=~round, y=~xmii_side, name = "MII: side effect", 
                marker = list(color = 'rgb(199,233,192)'),
                line = list(color = 'rgb(199,233,192)')) %>%
            add_lines()%>%
            add_lines(y = ~xmii_sidewhat, name = "MII: what to do", 
                      marker = list(color = 'rgb(161,217,155)'),
                      line = list(color = 'rgb(161,217,155)')) %>% 
            add_lines(y = ~xmii_other, name = "MII: other methods", 
                      marker = list(color = 'rgb(116,196,118)'),
                      line = list(color = 'rgb(116,196,118)')) %>% 
            add_lines(y = ~xmii_switch, name = "MII: switch (NEW)", 
                      marker = list(color = 'rgb(65,171,93)'),
                      line = list(color = 'rgb(65,171,93)')) %>% 
            add_lines(y = ~xmii3, name = "MII: all three items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      line = list(color = 'rgb(35,139,69)')) %>% 
            add_lines(y = ~xmii4, name = "MII: all four items (NEW)", 
                      marker = list(color = 'rgb(0,109,44)'),
                      line = list(color = 'rgb(0,109,44)')) %>% 
            layout(
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round", 
                             range = c(1,8) )
                )
        
        dtafig<-filter(dta, country==input$country & group=="All" & groupdemand==0)

        fig2<-plot_ly(dtafig, x=~round, y=~SDPall_essential5_noso, type = 'scatter', mode = 'lines',
                name = "5 specific methods* available currently/3-month", 
                marker = list(color = 'rgb(158,202,225)'),
                line = list(color = 'rgb(158,202,225)')) %>%
            add_trace(y = ~SDPall_essential5_ready, name = "Currently ready** to provide 5 specific methods*", 
                      marker = list(color = 'rgb(107,174,214)'),
                      line = list(color = 'rgb(107,174,214)')) %>% 
            add_trace(y = ~SDPall_essential5_rnoso, name = "Meeting both conditions", 
                      marker = list(color = 'rgb(33,113,181)'),
                      line = list(color = 'rgb(33,113,181)')) %>% 
            layout(
                yaxis = list(title = "Percent of all women with service environment***",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round",
                             range = c(1,8) )
            )
        
        subplot(fig1, fig2, nrows=1, margin=0.04, titleY = TRUE, shareX = TRUE) %>%
        layout( legend=list(orientation = 'v'),
                xaxis = list(title = "Survey round",
                             range = c(1,8) )
                )
        })

    output$plot_pattern_qualityIR1 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 & groupdemand==0) 
    
        plot_ly(dtafig, x=~grouplabel, y=~xmii_side, type = 'bar', 
                name = "MII: side effect", marker = list(color = 'rgb(199,233,192)'),
                text = ~xmii_side, textposition = 'outside') %>%
            add_trace(y = ~xmii_sidewhat, name = "MII: what to do", 
                      marker = list(color = 'rgb(161,217,155)'),
                      text = ~xmii_sidewhat, textposition = 'outside') %>% 
            add_trace(y = ~xmii_other, name = "MII: other methods", 
                      marker = list(color = 'rgb(116,196,118)'),
                      text = ~xmii_other, textposition = 'outside') %>% 
            add_trace(y = ~xmii_switch, name = "MII: switch (NEW)", 
                      marker = list(color = 'rgb(65,171,93)'),
                      text = ~xmii_switch, textposition = 'outside') %>% 
            add_trace(y = ~xmii3, name = "MII: all three items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      text = ~xmii3, textposition = 'outside') %>% 
            add_trace(y = ~xmii4, name = "MII: all four items (NEW)", 
                      marker = list(color = 'rgb(0,109,44)'),
                      text = ~xmii4, textposition = 'outside') %>% 
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v",x = 100, y = 0.5),
                showlegend=TRUE
            )
        
        })

    output$plot_pattern_qualityIR2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group==input$groupIR & groupdemand==0)%>%
            filter(round==max(round))

        plot_ly(dtafig, x=~grouplabel, y=~SDPall_essential5_noso, type = 'bar', 
                name = "5 specific methods* available currently/3-month", 
                marker = list(color = 'rgb(158,202,225)'),
                text = ~SDPall_essential5_noso, textposition = 'outside') %>%
            add_trace(y = ~SDPall_essential5_ready, name = "Currently ready** to provide 5 specific methods*", 
                      marker = list(color = 'rgb(107,174,214)'),
                      text = ~SDPall_essential5_ready, textposition = 'outside') %>% 
            add_trace(y = ~SDPall_essential5_rnoso, name = "Meeting both conditions", 
                      marker = list(color = 'rgb(33,113,181)'),
                      text = ~SDPall_essential5_rnoso, textposition = 'outside') %>% 
            layout(
                autosize = F, width = 1150, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v",x = 100, y = 0.5),
                showlegend=TRUE
            )
        
        })       

    output$plot_mcpr_qualityIR <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") &  groupdemand==0 & latestIRLINK==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By SDPall_essential5_noso"|
                       group=="By SDPall_essential5_ready"|
                       group=="By SDPall_essential5_rnoso")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By SDPall_essential5_noso", "By SDPall_essential5_ready", "By SDPall_essential5_rnoso"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v",x = 100, y = 0.5)
                
            )                

        })    
    
    

}     
    

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)