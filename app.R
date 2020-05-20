library(shiny)

library(plyr)
library(dplyr)
library(tidyverse)
library(plotly)

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
    mutate(cmcdate = ISOdate(year, month, 15),
           cmcdate=substr(as.character(cmcdate),0,7),
           latestIRLINK=latestIR,
           latestIRLINK=ifelse(xsurvey=="BFR5", 1, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="BFR6", 0, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="ETR5", 1, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="ETR6", 0, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="KER7", 1, latestIRLINK),
           latestIRLINK=ifelse(xsurvey=="KER8", 0, latestIRLINK)
    )

dtaCR<-data.frame(read_csv("summary_Access_Indicators_CR.csv"))%>%
    mutate(cmcdate = ISOdate(year, month, 15),
           cmcdate=substr(as.character(cmcdate),0,7)
    )

    table(dtaIR$group)
    table(dtaCR$group)
    
    table(dtaIR$grouplabel)
    table(dtaCR$grouplabel)    

    table(dtaIR$xsurvey)
    table(dtaCR$xsurvey)

    table(dtaIR$cmcdate)
    table(dtaCR$cmcdate)

dta<-rbind.fill(dtaIR, dtaCR)
    
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
    titlePanel("DO NOT CIRCULATE the link: application to internally explore potential indicators for access"),

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
                        selected = "Burkina Faso"),                  
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
            selectInput("groupCR", 
                        "Select background characteristics to assess disparity among all FP clients (only for service quality)",
                        choices = grouplistCR,
                        selected = "Education"),             
            br(),
            h5("Section D: MCPR by indicators"),
            h5("(No input needed)")
            
        ),
        
        # Main page for output display 
        mainPanel(
            width = 8,
            
            h5("This interactive app presents levels, trends, and patterns of potential access indicators.",
               "It also presents MCPR by status of each indicator."), 
            
            br(), 
            h5("Potential indicators are organized by element of access:"), 
            h5("1. psychosocial"),
            h5("2. cognitive accessibility,"), 
            h5("3. geographic accessibility,"), 
            h5("4. affordability, and"), 
            h5("5. service quality."), 
            h5("Under each element, there are four sections: A-D."), 
            h5(em("Provide inputs on the left panel.")), 
            h5(em("Hover over figures for estimates and other functions")), 

            br(),    
            h4(strong("1. Psychosocial accessibility")),
                h5("For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1ok1htfOAVQGzMxOCNwA4NqFV4JDVng3V/view?usp=sharing")),
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
                plotlyOutput("plot_mcpr_psychosocial2"),               

            h4(strong("1.1. Psychosocial accessibility??")),
                h5("--Opinion about FP (NEW, relevant for only Phase 1 surveys)"),
                h5("For the FP opinion indicators, two different definitions were used."),
                h5("--",em("Strongly agree/disagree"), " vs. the rest"),
                h5("--",em("Strongly agree/disagree or agree/disagree"), " vs. the rest"),
                h5("The higher, the more supportive/inclusive/favorable"),
            
                h5(strong("1.1.A. Level of indicators in the latest survey: STRONGLY DISAGREE/AGREE")),
                plotlyOutput("plot_level_psychosocial3"),  
                h5(strong("1.1.A. Level of indicators in the latest survey: DISAGREE/AGREE or STRONGLY DISAGREE/AGREE")),    
                plotlyOutput("plot_level_psychosocial4"),              
                
                h5(strong("1.1.C. SES pattern of indicators in the latest survey: STRONGLY DISAGREE/AGREE")),    
                h5("For year of the latest survey, see 1.A."),
                plotlyOutput("plot_pattern_psychosocial3"),   

                h5(strong("1.1.C. SES pattern of indicators in the latest survey: DISAGREE/AGREE or STRONGLY DISAGREE/AGREE")),    
                h5("For year of the latest survey, see 1.A."),
                plotlyOutput("plot_pattern_psychosocial4"),   
            
                h5(strong("1.1.D. MCPR by indicators in the latest survey: STRONGLY DISAGREE/AGREE")),    
                h5("For year of the latest survey, see 1.A."),    
                plotlyOutput("plot_mcpr_psychosocial3"),               
            
                h5(strong("1.1.D. MCPR by indicators in the latest survey: DISAGREE/AGREE or DISAGREE/STRONGLY AGREE")),    
                h5("For year of the latest survey, see 1.A."),    
                plotlyOutput("plot_mcpr_psychosocial4"),                             
            
            br(),                        
            h4(strong("2. Cognitive accessibility")),        
                h5("For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1IgCkBOL_nTEv2KDsYl7Wweeak8RbmVeu/view?usp=sharing")),
                h5("*Five specific modern methods: IUD, implant, injectables, pills, & male condom"),    
                h5("**Six specific modern methods: IUD, implant, injectables, pills, male condom & EC"),    
                h5(strong("2.A. Level of indicators in the latest survey")),
                plotlyOutput("plot_level_cognitive"),   
                
                h5(strong("2.B. Trends of indicators in the country")),
                plotlyOutput("plot_trend_cognitive"),   
                
                h5(strong("2.C. SES pattern of indicators in the latest survey")),    
                h5("For year of the latest survey, see 2.A."),
                plotlyOutput("plot_pattern_cognitive"),   
            
                h5(strong("2.D. MCPR by indicators in the latest survey")),    
                h5("For year of the latest survey, see 2.A."),    
                plotlyOutput("plot_mcpr_cognitive"),   
            
            br(),                        
            h4(strong("3. Geographic accessibility")),    
            
            br(),                        
            h4(strong("4. Affordability")),    
                h5("There is one potential indicator, but it is not specific. We will likely need to drop this, unless: there is a specific intervention linking health insurance and family planning service uptake, and the metric can become more specific to indicate that."),    
                h5("Applicable only for Phase 1 surveys (Currently, Burkina Faso, Kenya, and Nigeria states). For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
            
                h5(strong("4.A. Level of indicators in the latest survey")),
                plotlyOutput("plot_level_affordability"),   
            
                h5(strong("4.B. Trends of indicators in the country - not applicable yet")),
            
                h5(strong("4.C. SES pattern of indicators in the latest survey")), 
                h5("For year of the latest survey, see 4.A."),
                plotlyOutput("plot_pattern_affordability"),   
            
                h5(strong("4.D. MCPR by indicators in the latest survey")),    
                h5("For year of the latest survey, see 4.A."),    
                plotlyOutput("plot_mcpr_affordability"), 
            
            br(),                        
            h4(strong("5. Service quality")),    
            h4(strong("5.1. Service quality - from female survey")),    
                h5("For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1IgCkBOL_nTEv2KDsYl7Wweeak8RbmVeu/view?usp=sharing")),
                h5("*Five specific modern methods: IUD, implant, injectables, pills, & male condom"),    
                h5("**Ready: having commodity, trained personnel, & equipment. Applicable only for IUD and implant"),    
                h5(""),    
                h5("***Percent of all women with one or more linked SDPs meeting the service environment criteria (geographic and administrative access to methods/readiness)"),  
                h5(strong("(Note"),": For these cluster-level service environment indicators, linked-EA variables are required. The linked-EA variables are being created for Phase 1 surveys currently, and thus all phase 1 surveys are not included for the indicators. Additionally, several surveys (i.e., BFR6, ETR6, Nigeria states) are being further assessed for EA-SDP linkage results, and are excluded here.",
                   strong("Thus, latest surveys for MII indicators (green bars) and EA-level service-environment indicators (blue bars) can be different in a country"),".)"),  
            
                h5(strong("5.1.A. Level of indicators in the latest survey")),
                plotlyOutput("plot_level_qualityIR"),   
                
                h5(strong("5.1.B. Trends of indicators in the country")),
                plotlyOutput("plot_trend_qualityIR"),   
                
                h5(strong("5.1.C. SES pattern of indicators in the latest survey")),    
                h5("As we discussed earlier, there is not much typical differences by individual SES background, partially because these are either among current users (green bars) or cluster-level service environment (blue bars)."),      
                h5("For year of the latest survey, see 5.1.A."),    
                plotlyOutput("plot_pattern_qualityIR1"),   
                plotlyOutput("plot_pattern_qualityIR2"), 
            
                h5(strong("5.1.D. MCPR by indicators in the latest survey")),   
                h5("For year of the latest survey, see 5.1.A."),    
                plotlyOutput("plot_mcpr_qualityIR"),   

            br(),                        
            h4(strong("5.2. Service quality - from client exit survey")),    
                h5("Applicable only for Phase 1 surveys (Currently, Burkina Faso, Kenya, and Nigeria states). For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),    
                h5("Estimates based on a small sample size (i.e., <=20) were suppressed."),

                h5(strong("5.2.A. Level of indicators in the Phase 1 survey")),
                plotlyOutput("plot_level_qualityCR"),   

                h5(strong("5.2.B. Trends of indicators in the country - not applicable yet")),
            
                h5(strong("5.2.C. SES pattern of indicators in the Phase 1 survey")),   
                h5("For year of the latest survey, see 5.2.A."),    
                plotlyOutput("plot_pattern_qualityCR1"),   
                plotlyOutput("plot_pattern_qualityCR2"),   
            
            hr(),
            h6("See", a("GitHub",href="https://drive.google.com/file/d/1IgCkBOL_nTEv2KDsYl7Wweeak8RbmVeu/view?usp=sharing"),"for more information."),
            h6("Application last updated on April 28, 2020"),
            h6("For typos, errors, and questions:", a("contact me",href="https://www.isquared.global/YJ"))
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

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All")%>%
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
                autosize = F, width = 800, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=10))
            )
        })
    
    output$plot_trend_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All")%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~year, y=~xdec_users, type = 'scatter', mode = 'lines',
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
                autosize = F, width = 800, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round"),
                legend = list(orientation = 'v')
            )
        })

    output$plot_pattern_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1)%>%
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
                legend = list(font=list(size=10), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_psychosocial <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xwge_preg_exercise1"|group=="By xwge_preg_exercise2"|
                group=="By xwge_preg_exercise3"|group=="By xwge_preg_exercise"|
                group=="By xwge_fp_exercise1"|group=="By xwge_fp_exercise2"|group=="By xwge_fp_exercise")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xwge_preg_exercise1", "By xwge_preg_exercise2","By xwge_preg_exercise3", "By xwge_preg_exercise","By xwge_fp_exercise1", "By xwge_fp_exercise2", "By xwge_fp_exercise"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    ##### output: Psychosocial accessiblity 2 AGREE or STRONGLY AGREE #####

    output$plot_level_psychosocial2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All")%>%
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
                autosize = F, width = 850, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=10))
            )
        })
    
    output$plot_pattern_psychosocial2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1)%>%
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
                legend = list(font=list(size=10), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_psychosocial2 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xxwge_preg_exercise1"|group=="By xxwge_preg_exercise2"|
                group=="By xxwge_preg_exercise3"|group=="By xxwge_preg_exercise"|
                group=="By xxwge_fp_exercise1"|group=="By xxwge_fp_exercise2"|group=="By xxwge_fp_exercise")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xxwge_preg_exercise1", "By xxwge_preg_exercise2","By xxwge_preg_exercise3", "By xxwge_preg_exercise","By xxwge_fp_exercise1", "By xxwge_fp_exercise2", "By xxwge_fp_exercise"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    
    ##### output: Psychosocial accessiblity 3 OPINION STRONGLY AGREE #####

    output$plot_level_psychosocial3 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All")%>%
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
                autosize = F, width = 850, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=10))
            )
        })
    
    output$plot_pattern_psychosocial3 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1)%>%
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
                legend = list(font=list(size=10), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_psychosocial3 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xfp_self_pro"|group=="By xfp_self_mar"|
                group=="By xfp_self_nul"|group=="By xfp_self_life"|
                group=="By xfp_self3" | group=="By xfp_self4")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xfp_self_pro", "By xfp_self_mar","By xfp_self_nul", "By xfp_self3", "xfp_self_life", "By xfp_self4"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    ##### output: Psychosocial accessiblity 4 OPINION STRONGLY AGREE/DISAGREE or Agree/disagree #####

    output$plot_level_psychosocial4 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All")%>%
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
                autosize = F, width = 850, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v', font=list(size=10))
            )
        })
    
    output$plot_pattern_psychosocial4 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1)%>%
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
                legend = list(font=list(size=10), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_psychosocial4 <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xxfp_self_pro"|group=="By xxfp_self_mar"|
                group=="By xxfp_self_nul"|group=="By xxfp_self_life"|
                group=="By xxfp_self3"|group=="By xxfp_self4")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xxfp_self_pro", "By xxfp_self_mar","By xxfp_self_nul", "By xxfp_self3", "xxfp_self_life", "By xxfp_self4"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })        
    
    ##### output: Cognitive accessiblity ##### 
    
    output$plot_level_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All") 
    
        plot_ly(dtafig, x=~year, y=~xheard_5, type = 'bar', 
                name = "heard of 5+ any modern methods", marker = list(color = 'rgb(252,174,145)'),
                text = ~xheard_5, textposition = 'outside') %>%
            add_trace(y = ~xheard_7, name = "heard of 7+ any modern methods", 
                      marker = list(color = 'rgb(251,106,74)'),
                      text = ~xheard_7, textposition = 'outside') %>% 
            add_trace(y = ~xheard_10, name = "heard of 10+ any modern methods", 
                      marker = list(color = 'rgb(222,45,38)'),
                      text = ~xheard_10, textposition = 'outside') %>% 
            add_trace(y = ~xheard_select5, name = "heard of 5 specific modern methods*", 
                      marker = list(color = 'rgb(221,52,151)'),
                      text = ~xheard_select5, textposition = 'outside') %>% 
            add_trace(y = ~xheard_select6, name = "heard of 6 specific modern methods**", 
                      marker = list(color = 'rgb(174,1,126)'),
                      text = ~xheard_select6, textposition = 'outside') %>% 
            layout(
                autosize = F, width = 800, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v')
            )
        })
    
    output$plot_trend_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All") 
    
        plot_ly(dtafig, x=~round, y=~xheard_5, type = 'scatter', mode = 'lines',
                name = "heard of 5+ any modern methods", 
                marker = list(color = 'rgb(252,174,145)'),
                line = list(color = 'rgb(252,174,145)')) %>%
            add_trace(y = ~xheard_7, name = "heard of 7+ any modern methods", 
                      marker = list(color = 'rgb(251,106,74)'),
                      line = list(color = 'rgb(251,106,74)')) %>% 
            add_trace(y = ~xheard_10, name = "heard of 10+ any modern methods", 
                      marker = list(color = 'rgb(222,45,38)'),
                      line = list(color = 'rgb(222,45,38)')) %>% 
            add_trace(y = ~xheard_select5, name = "heard of 5 specific modern methods*", 
                      marker = list(color = 'rgb(221,52,151)'),
                      line = list(color = 'rgb(221,52,151)')) %>% 
            add_trace(y = ~xheard_select6, name = "heard of 6 specific modern methods**", 
                      marker = list(color = 'rgb(174,1,126)'),
                      line = list(color = 'rgb(174,1,126)')) %>% 
            layout(
                autosize = F, width = 800, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round"),
                legend = list(orientation = 'v')
            )
        })

    output$plot_pattern_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1) 
        #dtafig<-filter(dta, country=="Kenya" & group=="Residential area" & latestIR==1) 

        plot_ly(dtafig, x=~grouplabel, y=~xheard_5, type = 'bar', 
                name = "heard of 5+ any modern methods", 
                marker = list(color = 'rgb(252,174,145)'),
                text = ~xheard_5, textposition = 'outside') %>%
            add_trace(y = ~xheard_7, name = "heard of 7+ any modern methods", 
                      marker = list(color = 'rgb(251,106,74)'),
                      text = ~xheard_7, textposition = 'outside') %>% 
            add_trace(y = ~xheard_10, name = "heard of 10+ any modern methods", 
                      marker = list(color = 'rgb(222,45,38)'),
                      text = ~xheard_10, textposition = 'outside') %>% 
            add_trace(y = ~xheard_select5, name = "heard of 5 specific modern methods*", 
                      marker = list(color = 'rgb(221,52,151)'),
                      text = ~xheard_select5, textposition = 'outside') %>% 
            add_trace(y = ~xheard_select6, name = "heard of 6 specific modern methods**", 
                      marker = list(color = 'rgb(174,1,126)'),
                      text = ~xheard_select6, textposition = 'outside') %>% 
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_cognitive <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xheard_5"|group=="By xheard_7"|group=="By xheard_10"|
                       group=="By xheard_select5"|group=="By xheard_select6")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xheard_5", "By xheard_7", "By xheard_10", "By xheard_select5","By xheard_select6"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })    
    
    
    ##### output: Affordability #####
    
    output$plot_level_affordability <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All") 
        #dtafig<-filter(dta, country=="Kenya" & latestIR==1 & group=="All") 

        plot_ly(dtafig, x=~year, y=~xinsurance, type = 'bar', 
                name = "Have health insurance (NEW)", 
                marker = list(color = 'rgb(253,174,107)'), 
                text = ~xinsurance, textposition = 'outside')%>% 
            layout(
                autosize = F, width = 400, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = "v"), 
                showlegend=TRUE
            )
            #add_text(text=~xinsurance, hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black")) %>%
        })
    
    output$plot_pattern_affordability <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1) 
        #dtafig<-filter(dta, country=="Kenya" & group=="Residential area" & latestIR==1) 
    
        plot_ly(dtafig, x=~grouplabel, y=~xinsurance, type = 'bar', 
                name = "Have health insurance (NEW)",
                marker = list(color = 'rgb(253,174,107)'),
                text = ~xinsurance, textposition = 'outside')%>% 
            layout(
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })

    output$plot_mcpr_affordability <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & latestIR==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xinsurance")

        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v")
            )                

        })    
    
    
    ##### output: Service quality: IR #####
    
    output$plot_level_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(xmii_side)==FALSE & group=="All")%>%    
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
        
        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group=="All")%>%
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
        
        subplot(fig1, fig2, nrows=1, titleY = TRUE) %>%
        layout( legend=list(orientation = 'v'), 
                yaxis = list(range = c(0, 100)) )
        
        })
    
    output$plot_trend_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All")
    
        fig1<-plot_ly(dtafig, x=~round, y=~xmii_side, type = 'scatter', mode = 'lines',
                name = "MII: side effect", 
                marker = list(color = 'rgb(199,233,192)'),
                line = list(color = 'rgb(199,233,192)')) %>%
            add_trace(y = ~xmii_sidewhat, name = "MII: what to do", 
                      marker = list(color = 'rgb(161,217,155)'),
                      line = list(color = 'rgb(161,217,155)')) %>% 
            add_trace(y = ~xmii_other, name = "MII: other methods", 
                      marker = list(color = 'rgb(116,196,118)'),
                      line = list(color = 'rgb(116,196,118)')) %>% 
            add_trace(y = ~xmii_switch, name = "MII: switch (NEW)", 
                      marker = list(color = 'rgb(65,171,93)'),
                      line = list(color = 'rgb(65,171,93)')) %>% 
            add_trace(y = ~xmii3, name = "MII: all three items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      line = list(color = 'rgb(35,139,69)')) %>% 
            add_trace(y = ~xmii4, name = "MII: all four items (NEW)", 
                      marker = list(color = 'rgb(0,109,44)'),
                      line = list(color = 'rgb(0,109,44)')) %>% 
            layout(
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 100)),
                xaxis = list(title = "Survey round")
                )
        
        dtafig<-filter(dta, country==input$country & group=="All")

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
                xaxis = list(title = "Survey year")
            )
        
        subplot(fig1, fig2, nrows=1, titleY = TRUE) %>%
        layout( legend=list(orientation = 'v')
                )
        
        })

    output$plot_pattern_qualityIR1 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1) 
    
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
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })

    output$plot_pattern_qualityIR2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group==input$groupIR)%>%
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
                yaxis = list(title = "Percent of all women",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })       

    output$plot_mcpr_qualityIR <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="Yes" | grouplabel=="No") & 
                                 latestIRLINK==1)%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By SDPall_essential5_noso"|
                       group=="By SDPall_essential5_ready"|
                       group=="By SDPall_essential5_rnoso")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By SDPall_essential5_noso", "By SDPall_essential5_ready", "By SDPall_essential5_rnoso"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'auto',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of women in the group",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v")
                
            )                

        })    
    
    
    
    ##### output: Service quality: CR #####
    
    output$plot_level_qualityCR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestCR==1 & group=="All clients")%>%
            mutate(
                dummy1=0,
                dummy2=0
            )
    
        plot_ly(dtafig, x=~year, y=~ywant, type = 'bar', 
                name = "Received what she wanted", 
                marker = list(color = 'rgb(252,146,114)'),
                text = ~ywant, textposition = 'outside') %>%
            add_trace(y=~dummy1, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
                      
            add_trace(y=~ycounsel, name = "couseled on all items",
                      marker = list(color = 'rgb(204,236,230)'),
                      text = ~ycounsel , textposition = 'outside') %>%
            add_trace(y= ~yexplain_all, name = "explained on all items", 
                      marker = list(color = 'rgb(153,216,201)'),
                      text = ~yexplain_all , textposition = 'outside' ) %>% 
            add_trace(y= ~ydiscuss_all4, name = "discussed all 4 items", 
                      marker = list(color = 'rgb(102,194,164)'),
                      text = ~ydiscuss_all4 , textposition = 'outside' ) %>% 
            add_trace(y= ~ydiscuss_procon, name = "discussed pros/cons of the method", 
                      marker = list(color = 'rgb(65,174,118)'),
                      text = ~ydiscuss_procon , textposition = 'outside' ) %>%             
            add_trace(y= ~ydiscuss_all5, name = "discussed all 5 items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      text = ~ydiscuss_all5 , textposition = 'outside' ) %>%             
            add_trace(y=~dummy1, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            
            add_trace(y=~ycommunication_all, name = "comunications met all 3 points",
                      marker = list(color = 'rgb(67,162,202)'),
                      text = ~ycommunication_all , textposition = 'outside') %>%
            add_trace(y= ~ypolite, name = "polite or very polite", 
                      marker = list(color = 'rgb(198,219,239)'),
                      text = ~ypolite , textposition = 'outside' ) %>% 
            add_trace(y= ~ypolitevery, name = "very polite", 
                      marker = list(color = 'rgb(158,202,225)'),
                      text = ~ypolitevery , textposition = 'outside' ) %>% 
            add_trace(y= ~ysatisfied, name = "satisfied or very satisfied", 
                      marker = list(color = 'rgb(107,174,214)'),
                      text = ~ysatisfied , textposition = 'outside' ) %>%             
            add_trace(y= ~ysatisfiedvery, name = "very satisfied", 
                      marker = list(color = 'rgb(66,146,198)'),
                      text = ~ysatisfiedvery , textposition = 'outside' ) %>%    
            add_trace(y= ~yrefer, name = "would refer", 
                      marker = list(color = 'rgb(33,113,181)'),
                      text = ~yrefer , textposition = 'outside' ) %>%
            add_trace(y= ~yreturn, name = "would return", 
                      marker = list(color = 'rgb(8,81,156)'),
                      text = ~yreturn , textposition = 'outside' ) %>%
            add_trace(y= ~yreferreturn, name = "would refer and would return", 
                      marker = list(color = 'rgb(8,48,107)'),
                      text = ~yreferreturn , textposition = 'outside' ) %>%
            layout(
                autosize = F, width = 800, height = 400, 
                yaxis = list(title = "Percent of all FP clients",
                             range = c(0, 100) ),
                xaxis = list(title = "Survey year"), 
                legend = list(orientation = 'v')  
            )        
        
        })
    
    output$plot_pattern_qualityCR1 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestCR==1 & group==input$groupCR)%>% 
            mutate(dummy1=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("none", "primary", "secondary", "college+",
                                    "15-19",'20-29',"30-39","40 or above" ))
        
        plot_ly(dtafig, x=~grouplabel, y=~ywant, type = 'bar', 
                name = "Received what she wanted", 
                marker = list(color = 'rgb(252,146,114)'),
                text = ~ywant , textposition = 'outside') %>%
            #add_trace(y= ~ywantrec, name = "Received what she wanted or recommended for clinical reasons*", 
                      #marker = list(color = 'rgb(222,45,38)') ) %>% 
            add_trace(y=~dummy1, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%
            add_trace(y=~ycounsel, name = "couseled on all items",
                      marker = list(color = 'rgb(204,236,230)'),
                      text = ~ycounsel , textposition = 'outside') %>%
            add_trace(y= ~yexplain_all, name = "explained on all items", 
                      marker = list(color = 'rgb(153,216,201)'),
                      text = ~yexplain_all , textposition = 'outside' ) %>% 
            add_trace(y= ~ydiscuss_all4, name = "discussed all 4 items", 
                      marker = list(color = 'rgb(102,194,164)'),
                      text = ~ydiscuss_all4 , textposition = 'outside' ) %>% 
            add_trace(y= ~ydiscuss_procon, name = "discussed pros/cons of the method", 
                      marker = list(color = 'rgb(65,174,118)'),
                      text = ~ydiscuss_procon , textposition = 'outside' ) %>%             
            add_trace(y= ~ydiscuss_all5, name = "discussed all 5 items", 
                      marker = list(color = 'rgb(35,139,69)'),
                      text = ~ydiscuss_all5 , textposition = 'outside' ) %>%  
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all FP clients",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })
    
    output$plot_pattern_qualityCR2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestCR==1 & group==input$groupCR)%>% 
            mutate(dummy1=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("none", "primary", "secondary", "college+",
                                    "15-19",'20-29',"30-39","40 or above" ))
        
        plot_ly(dtafig, x=~grouplabel, y=~ycommunication_all, type = 'bar', 
                name = "comunications met all 3 points",
                marker = list(color = 'rgb(67,162,202)'),
                text = ~ycommunication_all , textposition = 'outside') %>%
            add_trace(y= ~ypolite, name = "polite or very polite", 
                      marker = list(color = 'rgb(198,219,239)'),
                      text = ~ypolite , textposition = 'outside' ) %>% 
            add_trace(y= ~ypolitevery, name = "very polite", 
                      marker = list(color = 'rgb(158,202,225)'),
                      text = ~ypolitevery , textposition = 'outside' ) %>% 
            add_trace(y= ~ysatisfied, name = "satisfied or very satisfied", 
                      marker = list(color = 'rgb(107,174,214)'),
                      text = ~ysatisfied , textposition = 'outside' ) %>%             
            add_trace(y= ~ysatisfiedvery, name = "very satisfied", 
                      marker = list(color = 'rgb(66,146,198)'),
                      text = ~ysatisfiedvery , textposition = 'outside' ) %>%    
            add_trace(y= ~yrefer, name = "would refer", 
                      marker = list(color = 'rgb(33,113,181)'),
                      text = ~yrefer , textposition = 'outside' ) %>%
            add_trace(y= ~yreturn, name = "would return", 
                      marker = list(color = 'rgb(8,81,156)'),
                      text = ~yreturn , textposition = 'outside' ) %>%
            add_trace(y= ~yreferreturn, name = "would refer and would return", 
                      marker = list(color = 'rgb(8,48,107)'),
                      text = ~yreferreturn , textposition = 'outside' ) %>%
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all FP clients",
                             range = c(0, 100)),
                xaxis = list(title = " "),
                legend = list(font=list(size=10), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })    

}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)