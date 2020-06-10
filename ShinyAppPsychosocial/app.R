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

#setwd("C:/Users/YoonJoung Choi/Dropbox/0 iSquared/iSquared_PMA/Effective Access")

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
           
           linkissue=(country=="Nigeria, Kano" | country=="Nigeria, Lagos" | 
                      country=="India, Rajasthan" | country=="Niger, Niamey" | 
                      xsurvey=="NGKanoR3" | xsurvey=="BFR6"),
           SDPall_essential5_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready), 
           SDPall_essential5ec_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5ec_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5ec_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready), 
           
           xmii_switch=ifelse(year<2019, NA, xmii_switch), 
           xmii4=ifelse(year<2019, NA, xmii4) 
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

dta<-rbind.fill(dtaIR, dtaCR)%>%
    filter(country=="Nigeria, Kano" | country=="Nigeria, Lagos" | country=="Burkina Faso" | country=="Kenya")

    dim(dtaIR)
    dim(dtaCR)    
    dim(dta)
    
countrylist<-unique(as.vector(dta$country))

    #countrylist<-countrylist[!is.na(countrylist)]
    #countrylist<-c("Burkina Faso", "Ethiopia", "Kenya", "Uganda")    

surveylist<-unique(as.vector(dta$xsurvey))

grouplistIR<-c("HH wealth", "Education", "Residential area", "Education x Residence", "Parity", "Union status")
grouplistCR<-c("Education of clients", "Age of clients")

#******************************
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    #headerPanel("Potential indicators for access"),

    # Title panel 
    titlePanel("Application to internally explore potential indicators for access (DO NOT CIRCULATE the link since it has non-public data)"),

    # Side panel: define input and output   
    sidebarLayout(
        fluid = TRUE,
        # Side panel for inputs: only ONE in this case
        sidebarPanel(
            style = "position:fixed;width:inherit;", 
            width = 3,
            br(),
            selectInput("country", 
                        "Select a country",
                        choices = countrylist, 
                        selected = "Burkina Faso"),                  
            br(),
            h5(strong("Sections A: Levels of indicators")),
            h5("(No input needed)"),
            br(),
            h5(strong("Sections B: Trends of indicators")),    
            h5("(No input needed)"),
            br(),
            h5(strong("Section C: Pattern of indicators")),
            selectInput("groupIR", 
                        "Select background characteristics to assess disparity among all women (all elements)",
                        choices = grouplistIR, 
                        selected = "Education"), 
            selectInput("groupCR", 
                        "Select background characteristics to assess disparity among all FP clients (only for service quality)",
                        choices = grouplistCR,
                        selected = "Residential area"),             
            br(),
            h5(strong("Section D: MCPR by indicators")),
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
                    h5("Potential indicators are organized by element of access and presented in each tab:"), 
                    h5("1. Psychosocial"),
                    h5("2. Cognitive accessibility,"), 
                    h5("3. Geographic accessibility,"), 
                    h5("4. Affordability, and"), 
                    h5("5. Service quality."), 
                    br(), 
                    h5(strong("For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1ok1htfOAVQGzMxOCNwA4NqFV4JDVng3V/view?usp=sharing"))),
                    br(), 
                    h5("Under each element, there are four sections: A-D."), 
                    h5(em("Provide inputs on the left panel.")), 
                    h5(em("Hover over figures for estimates and other functions")), 
                    hr(),
                    h6("See", a("GitHub",href="https://github.com/yoonjoung/PMA_Access"),"for more information, inluding Stata do fiels for estimation."),
                    h6("Application last updated on June 9, 2020"),
                    h6("For typos, errors, and questions:", a("contact me",href="https://www.isquared.global/YJ"))
                ), 
                tabPanel("1. Psychosocial",         
                    
                    #verbatimTextOutput("text_country"), 
                    
                    h4(strong("1. Psychosocial accessibility")),
                    
                    h5("Three groups of indicators under exploration:"),
                    h5("--decision making for using/not using contraception (older PMA2020 surveys do not have this)"),    
                    h5("--exercise of choice for pregnancy (NEW, relevant for only Phase 1 surveys)"),
                    h5("--exercise of choice for contraception (NEW, relevant for only Phase 1 surveys)"),
                    h5("For the new empowerment indicators, two different definitions were used."),
                    h5("--",em("Strongly agree"), " vs. the rest"),
                    h5("--",em("Strongly agree or agree"), " vs. the rest"),
                
                    h5(strong("1.A. Level of indicators in the latest survey: STRONGLY AGREE")),
                    plotlyOutput("plot_level_psychosocial"),  
                    h5(strong("1.A. Level of indicators in the latest survey: AGREE or STRONGLY AGREE")),    
                    plotlyOutput("plot_level_psychosocial2"),              
                    
                    h5(strong("1.B. Trends of indicators in the country")),
                    plotlyOutput("plot_trend_psychosocial"),   
                    
                    h5(strong("1.C. SES pattern of indicators in the latest survey: STRONGLY AGREE")),    
                    h5("For year of the latest survey, see 1.A."),
                    plotlyOutput("plot_pattern_psychosocial"),   
    
                    h5(strong("1.C. SES pattern of indicators in the latest survey: AGREE or STRONGLY AGREE")),    
                    h5("For year of the latest survey, see 1.A."),
                    plotlyOutput("plot_pattern_psychosocial2"),   
                
                    h5(strong("1.D. MCPR by indicators in the latest survey: STRONGLY AGREE")),    
                    h5("For year of the latest survey, see 1.A."),    
                    plotlyOutput("plot_mcpr_psychosocial"),               
                
                    h5(strong("1.D. MCPR by indicators in the latest survey: AGREE or STRONGLY AGREE")),    
                    h5("For year of the latest survey, see 1.A."),    
                    plotlyOutput("plot_mcpr_psychosocial2")               

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

    
    ##### output: Psychosocial accessiblity #####
    
    output$plot_level_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~year, y=~xdec_users, type = 'bar', 
                name = "decision by herself/jointly, among users", 
                marker = list(color = 'rgb(171,217,233)'),
                text = ~xdec_users, textposition = 'outside') %>%  
            add_trace(y=~xdec_nonusers, 
                name = "decision by herself/jointly, among non users", 
                marker = list(color = 'rgb(253,174,97)'),
                text = ~xdec_nonusers, textposition = 'outside') %>% 
            add_trace(y=~xdec, 
                name = "decision by herself/jointly, among all*", 
                marker = list(color = 'rgb(189,189,189)'),
                text = ~xdec, textposition = 'outside') %>%             
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_preg_exercise1, name = "can decide when to start having children (NEW)", 
                     marker = list(color = 'rgb(140,150,198)'),
                     text = ~xwge_preg_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise2, name = "can discuss when to start having children (NEW)", 
                      marker = list(color = 'rgb(140,107,177)'),
                      text = ~xwge_preg_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise3, name = "can negotiate when to stop having children (NEW)", 
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xwge_preg_exercise3, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise, name = "all three (NEW)", 
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xwge_preg_exercise, textposition = 'outside') %>% 
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_fp_exercise1, name = "can switch a method (NEW)", 
                      marker = list(color = 'rgb(203,201,226)'),
                      text = ~xwge_fp_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xwge_fp_exercise2, name = "can tell what's important (NEW)", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xwge_fp_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xwge_fp_exercise, name = "both (NEW)", 
                      marker = list(color = 'rgb(117,107,177)'),
                      text = ~xwge_fp_exercise, textposition = 'outside') %>%
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })
    
    output$plot_trend_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~round, y=~xdec_users, type = 'scatter', mode = 'lines',
                name = "decision by herself/jointly, among users", 
                marker = list(color = 'rgb(171,217,233)'),
                line = list(color = 'rgb(171,217,233)')) %>%  
            add_trace(y=~xdec_nonusers, 
                name = "decision by herself/jointly, among non users", 
                marker = list(color = 'rgb(253,174,97)'),
                line = list(color = 'rgb(253,174,97)')) %>% 
            add_trace(y=~xdec, 
                name = "decision by herself/jointly, among all*", 
                marker = list(color = 'rgb(189,189,189)'),
                line = list(color = 'rgb(189,189,189)')) %>%   
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'),
                      line = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_preg_exercise1, name = "can decide when to start having children (NEW)", 
                     marker = list(color = 'rgb(140,150,198)'),
                     line = list(color = 'rgb(140,150,198)')) %>% 
            add_trace(y = ~xwge_preg_exercise2, name = "can discuss when to start having children (NEW)", 
                      marker = list(color = 'rgb(140,107,177)'),
                      line =  list(color = 'rgb(140,107,177)')) %>% 
            add_trace(y = ~xwge_preg_exercise3, name = "can negotiate when to stop having children (NEW)", 
                      marker = list(color = 'rgb(136,65,157)'),
                      line = list(color = 'rgb(136,65,157)')) %>% 
            add_trace(y = ~xwge_preg_exercise, name = "all three (NEW)", 
                      marker = list(color = 'rgb(129,15,124)'),
                      line = list(color = 'rgb(129,15,124)')) %>% 
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'),
                      line = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_fp_exercise1, name = "can switch a method (NEW)", 
                      marker = list(color = 'rgb(203,201,226)'),
                      line =  list(color = 'rgb(203,201,226)')) %>% 
            add_trace(y = ~xwge_fp_exercise2, name = "can tell what's important (NEW)", 
                      marker = list(color = 'rgb(158,154,200)'),
                      line =  list(color = 'rgb(158,154,200)')) %>% 
            add_trace(y = ~xwge_fp_exercise, name = "both (NEW)", 
                      marker = list(color = 'rgb(117,107,177)'),
                      line = list(color = 'rgb(117,107,177)')) %>%
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8)),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })

    output$plot_pattern_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~grouplabel, y=~xdec_users, type = 'bar', 
                name = "decision by herself/jointly, among users", 
                marker = list(color = 'rgb(171,217,233)'),
                text = ~xdec_users, textposition = 'outside') %>%  
            add_trace(y=~xdec_nonusers, 
                name = "decision by herself/jointly, among non users", 
                marker = list(color = 'rgb(253,174,97)'),
                text = ~xdec_nonusers, textposition = 'outside') %>% 
            add_trace(y=~xdec, 
                name = "decision by herself/jointly, among all*", 
                marker = list(color = 'rgb(189,189,189)'),
                text = ~xdec, textposition = 'outside') %>%             
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_preg_exercise1, name = "can decide when to start having children (NEW)", 
                     marker = list(color = 'rgb(140,150,198)'),
                     text = ~xwge_preg_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise2, name = "can discuss when to start having children (NEW)", 
                      marker = list(color = 'rgb(140,107,177)'),
                      text = ~xwge_preg_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise3, name = "can negotiate when to stop having children (NEW)", 
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xwge_preg_exercise3, textposition = 'outside') %>% 
            add_trace(y = ~xwge_preg_exercise, name = "all three (NEW)", 
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xwge_preg_exercise, textposition = 'outside') %>%
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xwge_fp_exercise1, name = "can switch a method (NEW)", 
                      marker = list(color = 'rgb(203,201,226)'),
                      text = ~xwge_fp_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xwge_fp_exercise2, name = "can tell what's important (NEW)", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xwge_fp_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xwge_fp_exercise, name = "both (NEW)", 
                      marker = list(color = 'rgb(117,107,177)'),
                      text = ~xwge_fp_exercise, textposition = 'outside') %>%
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })
    
    output$plot_mcpr_psychosocial <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1 & groupdemand==0)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xwge_preg_exercise1"|group=="By xwge_preg_exercise2"|
                group=="By xwge_preg_exercise3"|group=="By xwge_preg_exercise"|
                group=="By xwge_fp_exercise1"|group=="By xwge_fp_exercise2"|group=="By xwge_fp_exercise")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xwge_preg_exercise1", "By xwge_preg_exercise2","By xwge_preg_exercise3", "By xwge_preg_exercise","By xwge_fp_exercise1", "By xwge_fp_exercise2", "By xwge_fp_exercise"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'outside',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "h", xanchor = "left", 
                              x = 0.1, y = 0.9)
            )                

        })        
    
    
    ##### output: Psychosocial accessiblity 2 AGREE or STRONGLY AGREE #####

    output$plot_level_psychosocial2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~year, y=~xdec_users, type = 'bar', 
                name = "decision by herself/jointly, among users", 
                marker = list(color = 'rgb(171,217,233)'),
                text = ~xdec_users, textposition = 'outside') %>%  
            add_trace(y=~xdec_nonusers, 
                name = "decision by herself/jointly, among non users", 
                marker = list(color = 'rgb(253,174,97)'),
                text = ~xdec_nonusers, textposition = 'outside') %>% 
            add_trace(y=~xdec, 
                name = "decision by herself/jointly, among all*", 
                marker = list(color = 'rgb(189,189,189)'),
                text = ~xdec, textposition = 'outside') %>%             
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
    
            add_trace(y = ~xxwge_preg_exercise1, name = "can decide when to start having children (NEW): AG + ST AG", 
                     marker = list(color = 'rgb(140,150,198)'),
                     text = ~xxwge_preg_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise2, name = "can discuss when to start having children (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(140,107,177)'),
                      text = ~xxwge_preg_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise3, name = "can negotiate when to stop having children (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xxwge_preg_exercise3, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise, name = "all three (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xxwge_preg_exercise, textposition = 'outside') %>% 
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xxwge_fp_exercise1, name = "can switch a method (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(203,201,226)'),
                      text = ~xxwge_fp_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_fp_exercise2, name = "can tell what's important (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xxwge_fp_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_fp_exercise, name = "both (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(117,107,177)'),
                      text = ~xxwge_fp_exercise, textposition = 'outside') %>%            
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=12), x=100)
            )
        })
    
    output$plot_pattern_psychosocial2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~grouplabel, y=~xdec_users, type = 'bar', 
                name = "decision by herself/jointly, among users", 
                marker = list(color = 'rgb(171,217,233)'),
                text = ~xdec_users, textposition = 'outside') %>%  
            add_trace(y=~xdec_nonusers, 
                name = "decision by herself/jointly, among non users", 
                marker = list(color = 'rgb(253,174,97)'),
                text = ~xdec_nonusers, textposition = 'outside') %>% 
            add_trace(y=~xdec, 
                name = "decision by herself/jointly, among all*", 
                marker = list(color = 'rgb(189,189,189)'),
                text = ~xdec, textposition = 'outside') %>%             
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xxwge_preg_exercise1, name = "can decide when to start having children (NEW): AG + ST AG", 
                     marker = list(color = 'rgb(140,150,198)'),
                     text = ~xxwge_preg_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise2, name = "can discuss when to start having children (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(140,107,177)'),
                      text = ~xxwge_preg_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise3, name = "can negotiate when to stop having children (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xxwge_preg_exercise3, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_preg_exercise, name = "all three (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xxwge_preg_exercise, textposition = 'outside') %>%
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y = ~xxwge_fp_exercise1, name = "can switch a method (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(203,201,226)'),
                      text = ~xxwge_fp_exercise1, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_fp_exercise2, name = "can tell what's important (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xxwge_fp_exercise2, textposition = 'outside') %>% 
            add_trace(y = ~xxwge_fp_exercise, name = "both (NEW): AG + ST AG", 
                      marker = list(color = 'rgb(117,107,177)'),
                      text = ~xxwge_fp_exercise, textposition = 'outside') %>%
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left")
            )
        })
    
    output$plot_mcpr_psychosocial2 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1 & groupdemand==0)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xxwge_preg_exercise1"|group=="By xxwge_preg_exercise2"|
                group=="By xxwge_preg_exercise3"|group=="By xxwge_preg_exercise"|
                group=="By xxwge_fp_exercise1"|group=="By xxwge_fp_exercise2"|group=="By xxwge_fp_exercise")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xxwge_preg_exercise1", "By xxwge_preg_exercise2","By xxwge_preg_exercise3", "By xxwge_preg_exercise","By xxwge_fp_exercise1", "By xxwge_fp_exercise2", "By xxwge_fp_exercise"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    
    ##### output: Psychosocial accessiblity 3 OPINION STRONGLY AGREE #####

    output$plot_level_psychosocial3 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~year, y=~xfp_self_pro, type = 'bar', 
                name = "promiscuous adolescents (NEW): ST DISAG", 
                marker = list(color = 'rgb(188,189,220)'),
                text = ~xfp_self_pro, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self_mar, name = "only for married (NEW): ST DISAG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xfp_self_mar, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self_nul, name = "only for those without kids (NEW): ST DISAG", 
                      marker = list(color = 'rgb(128,125,186)'),
                      text = ~xfp_self_nul, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self3, name = "all three (NEW)", 
                      marker = list(color = 'rgb(84,39,143)'),
                      text = ~xfp_self3, textposition = 'outside') %>%     
            add_trace(y = ~xfp_self_life, name = "better quality life (NEW): ST AG", 
                      marker = list(color = 'rgb(106,81,163)'),
                      text = ~xfp_self_life, textposition = 'outside') %>%          
            add_trace(y = ~xfp_self4, name = "all four (NEW)", 
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xfp_self4, textposition = 'outside') %>%          
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=12))
            )
        })
    
    output$plot_pattern_psychosocial3 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~grouplabel, y=~xfp_self_pro, type = 'bar', 
                name = "promiscuous adolescents (NEW): ST DISAG", 
                marker = list(color = 'rgb(188,189,220)'),
                text = ~xfp_self_pro, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self_mar, name = "only for married (NEW): ST DISAG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xfp_self_mar, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self_nul, name = "only for those without kids (NEW): ST DISAG", 
                      marker = list(color = 'rgb(128,125,186)'),
                      text = ~xfp_self_nul, textposition = 'outside') %>% 
            add_trace(y = ~xfp_self3, name = "all three (NEW)", 
                      marker = list(color = 'rgb(84,39,143)'),
                      text = ~xfp_self3, textposition = 'outside') %>%     
            add_trace(y = ~xfp_self_life, name = "better quality life (NEW): ST AG", 
                      marker = list(color = 'rgb(106,81,163)'),
                      text = ~xfp_self_life, textposition = 'outside') %>%          
            add_trace(y = ~xfp_self4, name = "all four (NEW)", 
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xfp_self4, textposition = 'outside') %>%        
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left")
            )
        })
    
    output$plot_mcpr_psychosocial3 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1 & groupdemand==0)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xfp_self_pro"|group=="By xfp_self_mar"|
                group=="By xfp_self_nul"|group=="By xfp_self_life"|
                group=="By xfp_self3" | group=="By xfp_self4")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xfp_self_pro", "By xfp_self_mar","By xfp_self_nul", "By xfp_self3", "xfp_self_life", "By xfp_self4"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    
    
    ##### output: Psychosocial accessiblity 4 OPINION STRONGLY AGREE/DISAGREE or Agree/disagree #####

    output$plot_level_psychosocial4 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~year, y=~xxfp_self_pro, type = 'bar', 
                name = "promiscuous adolescents (NEW): ST DISAG + DISAG", 
                marker = list(color = 'rgb(188,189,220)'),
                text = ~xxfp_self_pro, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self_mar, name = "only for married (NEW): ST DISAG + DISAG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xxfp_self_mar, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self_nul, name = "only for those without kids (NEW): ST DISAG + DISAG", 
                      marker = list(color = 'rgb(128,125,186)'),
                      text = ~xxfp_self_nul, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self3, name = "all three (NEW)", 
                      marker = list(color = 'rgb(84,39,143)'),
                      text = ~xxfp_self3, textposition = 'outside') %>%               
            add_trace(y = ~xxfp_self_life, name = "better quality life (NEW): ST AG + AG", 
                      marker = list(color = 'rgb(106,81,163)'),
                      text = ~xxfp_self_life, textposition = 'outside') %>%          
            add_trace(y = ~xxfp_self4, name = "all four (NEW)", 
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xxfp_self4, textposition = 'outside') %>%          
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=12))
            )
        })
    
    output$plot_pattern_psychosocial4 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 & groupdemand==0)%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~grouplabel, y=~xxfp_self_pro, type = 'bar', 
                name = "promiscuous adolescents (NEW): ST DISAG + DISAG", 
                marker = list(color = 'rgb(188,189,220)'),
                text = ~xxfp_self_pro, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self_mar, name = "only for married (NEW): ST DISAG + DISAG", 
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xxfp_self_mar, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self_nul, name = "only for those without kids (NEW): ST DISAG + DISAG", 
                      marker = list(color = 'rgb(128,125,186)'),
                      text = ~xxfp_self_nul, textposition = 'outside') %>% 
            add_trace(y = ~xxfp_self3, name = "all three (NEW)", 
                      marker = list(color = 'rgb(84,39,143)'),
                      text = ~xxfp_self3, textposition = 'outside') %>%               
            add_trace(y = ~xxfp_self_life, name = "better quality life (NEW): ST AG + AG", 
                      marker = list(color = 'rgb(106,81,163)'),
                      text = ~xxfp_self_life, textposition = 'outside') %>%          
            add_trace(y = ~xxfp_self4, name = "all four (NEW)", 
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xxfp_self4, textposition = 'outside') %>%         
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_psychosocial4 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1 & groupdemand==0)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xxfp_self_pro"|group=="By xxfp_self_mar"|
                group=="By xxfp_self_nul"|group=="By xxfp_self_life"|
                group=="By xxfp_self3"|group=="By xxfp_self4")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xxfp_self_pro", "By xxfp_self_mar","By xxfp_self_nul", "By xxfp_self3", "xxfp_self_life", "By xxfp_self4"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    
    
    
    
}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)