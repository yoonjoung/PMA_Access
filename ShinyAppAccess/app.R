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

# OLD VERSION using June 15 data, before the call with Caroline

#******************************
# 0. Database update 
#******************************

#setwd("C:/Users/YoonJoung Choi/Dropbox/0 iSquared/iSquared_PMA/Effective Access/ShinyAppAccess")

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
                      country=="Niger, Niamey" | 
                      xsurvey=="NGKanoR3" | xsurvey=="BFR6"),
           SDPall_essential5_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready), 
           SDPall_essential5ec_rnoso=ifelse(linkissue==1, NA, SDPall_essential5_rnoso), 
           SDPall_essential5ec_noso=ifelse(linkissue==1, NA, SDPall_essential5_noso), 
           SDPall_essential5ec_ready=ifelse(linkissue==1, NA, SDPall_essential5_ready), 
           
           xmii_switch=ifelse(xsurvey=="BFR6", NA, xmii_switch),  
           xmii4=ifelse(xsurvey=="BFR6", NA, xmii4)  
             
    )%>% 
    mutate_at(vars(starts_with("xwge")), funs(round(., 1)) )


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
            h5(strong("Sections C: Pattern of indicators by status based on FP demand, contraceptive use, and sexual activity")),    
            h5("(No input needed)"),
            br(),
            h5(strong("Section D: Pattern of indicators by SES background")),
            selectInput("groupIR", 
                        "Select background characteristics to assess disparity among all women (all elements)",
                        choices = grouplistIR, 
                        selected = "Education"), 
            selectInput("groupCR", 
                        "Select background characteristics to assess disparity among all FP clients (only for service quality)",
                        choices = grouplistCR,
                        selected = "Education"),             
            br(),
            h5(strong("Section E: MCPR by indicators")),
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
                    h5("1. Psychosocial,"),
                    h5("2. Cognitive accessibility,"), 
                    h5("3. Geographic accessibility,"), 
                    h5("4. Affordability, and"), 
                    h5("5. Service quality."), 
                    br(), 
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(), 
                    h5("Under each element, there are five sections: A-E."), 
                    h5(em("Provide inputs on the left panel.")), 
                    h5(em("Hover over figures for estimates and other functions")), 
                    br(),
                    h5(strong("Before checking indicators, see profile of women based on demand for FP, contraceptive use, and sexual activity.")),
                    hr(),
                    h6("See", a("GitHub",href="https://github.com/yoonjoung/PMA_Access"),"for more information, inluding Stata do fiels for estimation."),
                    h6("Application last updated on July 1, 2020"),
                    h6("For typos, errors, and questions:", a("contact me",href="https://www.isquared.global/YJ"))
                ), 
                
                tabPanel("0. Profile of women",                    
                    h4(strong("First, see female population composition by demand for FP and contraceptive use.")),    
                    hr(),
                    h5("Access to contraception is an issue in a different way among each group of women."),  
                    h5("  1. Those who currently use", strong("(dark orange)")),
                    h5("  ===> need to ensure current method is based on her full choice, since discontinuation while in need is a problem."), 
                    h5("  2. Those who have demand but do not use", strong("(light orange)")), 
                    h5("  --- Those who have demand, but do not use and intend to use"), 
                    h5("  ===> need to identify/address barriers"),
                    h5("  --- Those who have demand, but do not use and do not intend to use - ", 
                       "access to contraception is mostly NOT an issue. However,",
                       strong("if their intention to use"), 
                       "is based on incomplete information, past negative experience, etc."),
                    h5("  ===> need to identify/address those barriers"), 
                    h5("  3. Those who are fecund and do not have demand", strong("(light blue)"), 
                       " - access to contraception is mostly NOT an issue. However,", 
                       strong("if their fertility intention"), 
                       "is likely to change upon other changing factors (e.g., MCH/HTSP, delayed marriage, education, women's labor participation)"),
                    h5("  ===> FP at the program level should be ready to accommodate potentially increasing demand for FP (e.g., forecasting - short & long term)"), 
                    h5("  4. Those who are infecund or menopausal", strong("(mid blue) - "), 
                       "access to contraception is mostly NOT an issue. However,",
                       strong("in a case of infertility")), 
                    h5("  ===> completely different type of reproductive programs are needed, though it is a question of prioritizing public resources in most low-resource settings"), 
                    h5("  5. Those who are sexually not active", strong("(dark blue) - "),
                       "mostly young unmarried women, though not always. Access to contraception is NOT an immediate issue. However,"), 
                    h5("  ===> empowerment for contraception and broader SRH"), 
                    
                    br(),
                    h4(strong("Among all women, composition by survey.")),    
                    plotlyOutput("plot_denominator"),   
                    
                    br(),
                    h4(strong("Also, the composition in terms of pop pyramid, using the latest survey.")),    
                    plotlyOutput("plot_denominatorPyramid")      
                    
                ),
                
                tabPanel("1. Psychosocial",         
                    
                    #verbatimTextOutput("text_country"), 
                    
                    h4(strong("1. Psychosocial accessibility")),
                    
                    h5("Following discussion with Caroline on 6-24-2020"),
                    h5(" -- WGE: existence of choice for pregnancy (NEW, relevant for only Phase 1 surveys)"),
                    h5(" -- WGE: exercise of choice for contraception (NEW, relevant for only Phase 1 surveys)"),
                    h5(" -- WGE pregnancy questions under further investigation. Do not use it yet"),
                    h5(" -- decision making to use/not use contraception (older PMA2020 surveys do not have this, but it is available in DHS)"),    
                    h5(" -- And, social norms question to be explored with intention to use contraception"),
                    h5(""),
                    
                    br(),
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(),
                    h5(strong("1.A. Level of indicators in the latest survey - focusing on WGE, only in Phase 1 surveys")),
                    plotlyOutput("plot_level_psychosocial"),  
                    
                    h5(strong("1.B. Trends of indicators in the country - only contraceptive decision")),
                    h5("Percent of women whose decision to use/not use contraception was by either 'herself' or 'herself & her partner'"),
                    plotlyOutput("plot_trend_psychosocialDECISION"),   
                    
                    h5(strong("1.C. Pattern of indicators in the latest survey - by fertility intention, contraceptive use, and sexual activity")),    
                    h5("For year of the latest survey, see 1.A."),
                    plotlyOutput("plot_DEMANDpattern_psychosocial"),
                    
                    h5(strong("1.D. Pattern of indicators in the latest survey - by SES background")),    
                    h5("For year of the latest survey, see 1.A."),
                    plotlyOutput("plot_SESpattern_psychosocial"),   
                
                    h5(strong("1.E. MCPR by indicators in the latest survey")),    
                    h5("TO be explored more for this domain...")    
                    #plotlyOutput("plot_mcpr_psychosocial"),               
    
                ),
          
                tabPanel("2. Cognitive",           
                    
                    #verbatimTextOutput("text_country"),      
                    
                    h4(strong("2. Cognitive accessibility")),  
                    h5("*Five specific modern methods: IUD, implant, injectables, pills, & male condom"),    
                    br(),
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(),
                    h5(strong("2.A. Level of indicators in the latest survey")),
                    plotlyOutput("plot_level_cognitive"),   
                    
                    h5(strong("2.B. Trends of indicators in the country")),
                    plotlyOutput("plot_trend_cognitive"),  
                    
                    h5(strong("2.C. Pattern of indicators in the latest survey - by fertility intention, contraceptive use, and sexual activity")),    
                    plotlyOutput("plot_DEMANDpattern_cognitive"),   
                    
                    h5(strong("2.D. Pattern of indicators in the latest survey - by SES background")),    
                    h5("For year of the latest survey, see 2.A."),
                    plotlyOutput("plot_SESpattern_cognitive"),   
                    
                    h5(strong("2.E. MCPR by indicators in the latest survey")),    
                    h5("For year of the latest survey, see 2.A."),    
                    plotlyOutput("plot_mcpr_cognitive")   
                       
                ),
                
                tabPanel("3. Geographic",                    
                    h4(strong("3. Geographic accessibility")),    
                    h5("TBD")     
                ),
                
                tabPanel("4. Affordability",           
                    
                    #verbatimTextOutput("text_country"),      
                    
                    h4(strong("4. Affordability")),  
                        
                    h5("There is one potential indicator, but it is not specific. We will likely need to drop this, unless: there is a specific intervention linking health insurance and family planning service uptake, and the metric can become more specific to indicate that."),    
                    h5("Applicable only for Phase 1 surveys (Currently, Burkina Faso, Kenya, and Nigeria states)."),
                    br(),
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(),
                    h5(strong("4.A. Level of indicators in the latest survey")),
                    plotlyOutput("plot_level_affordability"),   
                
                    h5(strong("4.B. Trends of indicators in the country - not applicable yet")),
                
                    h5(strong("4.C. Pattern of indicators in the latest survey - by fertility intention, contraceptive use, and sexual activity")),    
                    plotlyOutput("plot_DEMANDpattern_affordability"),   
                    
                    h5(strong("4.D. Pattern of indicators in the latest survey - by SES background")), 
                    h5("For year of the latest survey, see 4.A."),
                    plotlyOutput("plot_SESpattern_affordability"),   
                
                    h5(strong("4.E. MCPR by indicators in the latest survey")),    
                    h5("For year of the latest survey, see 4.A."),    
                    plotlyOutput("plot_mcpr_affordability") 
                ),
                
                tabPanel("5. Service quality - FQ",    
                    
                    #verbatimTextOutput("text_country"),      
                    h4(strong("5. Service quality")),    
                    h4(strong("5.1. Service quality - from female survey")),    
                    h5("*Five specific modern methods: IUD, implant, injectables, pills, & male condom (except in India, where implant is not yet widely available)"),    
                    h5("**Ready: having commodity, trained personnel, & equipment. Applicable only for IUD and implant"),    
                    h5(""),    
                    h5("***Percent of all women with one or more linked SDPs meeting the service environment criteria (geographic and administrative access to methods/readiness)"),  
                    h5(strong("(Note:"),"For these cluster-level service environment indicators, linked-EA variables are required. The linked-EA variables are being created for Phase 1 surveys currently, and thus all phase 1 surveys are not included for the indicators.", 
                       "Additionally, several surveys (i.e., BFR6, KanoR3, NiameyR5) are being further assessed for EA-SDP linkage results (", 
                       a("see here", href="https://rpubs.com/YJ_Choi/PMA_EA_SDP_Link"), "), and are excluded in this report.",
                       strong("Thus, latest surveys for MII indicators (green bars) and EA-level service-environment indicators (blue bars) can be different in a country.)")),  
                
                    br(),
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(),
                    h5(strong("5.1.A. Level of indicators in the latest survey")),
                    plotlyOutput("plot_level_qualityIR"),   
                    
                    h5(strong("5.1.B. Trends of indicators in the country")),
                    plotlyOutput("plot_trend_qualityIR"),   
                
                    h5(strong("5.1.C. Pattern of indicators in the latest survey - by fertility intention, contraceptive use, and sexual activity")),    
                    h5("Not really relevant here"), 
                    
                    h5(strong("5.1.D. Pattern of indicators in the latest survey - by SES background")),    
                    h5("As we discussed earlier, there is not much typical differences by individual SES background, partially because these are either among current users (green bars) or cluster-level service environment (blue bars)."),      
                    h5("For year of the latest survey, see 5.1.A."),    
                    plotlyOutput("plot_SESpattern_qualityIR1"),   
                    plotlyOutput("plot_SESpattern_qualityIR2"), 
                    
                    h5(strong("5.1.E. MCPR by indicators in the latest survey")),   
                    h5(strong("Question 1"),": reverse causality?"), 
                    h5(strong("Question 2"),": Quality as rights, then do we care for any association here?"), 
                    h5("For year of the latest survey, see 5.1.A."),    
                    plotlyOutput("plot_mcpr_qualityIR")   
                    
                ),
                
                tabPanel("5. Service quality - CEI",          
                    
                    #verbatimTextOutput("text_country"),           
                    
                    h4(strong("5. Service quality")),             
                    h4(strong("5.2. Service quality - from client exit survey")),    
                    h5("Applicable only for Phase 1 surveys (Currently, Burkina Faso, Kenya, and Nigeria states). For definitions and data sources, see", a("here.",href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),    
                    h5("Estimates based on a small sample size (i.e., <=20) were suppressed."),
                    br(),
                    h5(a(strong("For definitions and data sources, see here."),href="https://drive.google.com/file/d/1a7lPpJUk2ArJjb7hr8zdxqKKKJeiCnqb/view?usp=sharing")),
                    br(),
                    h5(strong("5.2.A. Level of indicators in the Phase 1 survey")),
                    plotlyOutput("plot_level_qualityCR"),   
    
                    h5(strong("5.2.B. Trends of indicators in the country - not applicable yet")),
                
                    h5(strong("5.2.D. Pattern of indicators in the Phase 1 survey - by client's SES background")),   
                    h5("For year of the latest survey, see 5.2.A."),    
                    plotlyOutput("plot_SESpattern_qualityCR1"),   
                    plotlyOutput("plot_SESpattern_qualityCR2")   
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


    ##### output: Denominator #####
    
    output$plot_denominator <- renderPlotly({
        
        dtafig<-filter(dta, group=="All" )%>%
            mutate(dummy=0)%>%
            mutate(n=xd_use+ xd_unmet+ xd_nodemand_fecund+ xd_nodemand_infecund+ xd_notSA,
                   xd_use=round(100*xd_use/n, 1),
                   xd_unmet=round(100*xd_unmet/n, 1),
                   xd_nodemand_fecund=round(100*xd_nodemand_fecund/n, 1), 
                   xd_nodemand_infecund=round(100*xd_nodemand_infecund/n, 1),
                   xd_notSA=round(100*xd_notSA/n, 1)
                )

        plot_ly(dtafig, x = ~xsurvey, y = ~xd_use, type = "bar",
                name = "demand, using", 
                marker = list(color = 'rgb(244,109,67)'),
                text = ~xd_use) %>% 
            add_trace(y = ~xd_unmet, type = 'bar', 
                      name = "demand, not using", 
                      marker = list(color = 'rgb(253,174,97)'),  
                      text = ~xd_unmet) %>%   
            add_trace(y = ~xd_nodemand_fecund, type = 'bar', 
                      name = "no demand, fecund", 
                      marker = list(color = 'rgb(171,217,233)'),  
                      text = ~xd_nodemand_fecund) %>%               
            add_trace(y = ~xd_nodemand_infecund, type = 'bar', 
                      name = "no demand, infecund", 
                      marker = list(color = 'rgb(116,173,209)'),  
                      text = ~xd_nodemand_infecund) %>%                           
            add_trace(y = ~xd_notSA, type = 'bar', 
                      name = "not sexually active", 
                      marker = list(color = 'rgb(69,117,180)'),  
                      text = ~xd_notSA) %>%                           
            layout(
                title ="Female population by demand for FP and contraceptive use",
                yaxis = list(title = "Percent of women",
                             titlefont=list(size=12)), 
                legend = list(font=list(size=12)),
                barmode = 'stack'
                ) 
    })    

    output$plot_denominatorSA <- renderPlotly({
        
        dtafig<-filter(dta, group=="All" )%>%
            mutate(dummy=0)%>%
            mutate(n=xd_use+ xd_unmet+ xd_nodemand_fecund+ xd_nodemand_infecund,
                   xd_use=round(100*xd_use/n, 1),
                   xd_unmet=round(100*xd_unmet/n, 1),
                   xd_nodemand_fecund=round(100*xd_nodemand_fecund/n, 1), 
                   xd_nodemand_infecund=round(100*xd_nodemand_infecund/n, 1)
                )

        plot_ly(dtafig, x = ~xsurvey, y = ~xd_use, type = "bar",
                name = "demand, using", 
                marker = list(color = 'rgb(244,109,67)'),
                text = ~xd_use) %>% 
            add_trace(y = ~xd_unmet, type = 'bar', 
                      name = "demand, not using", 
                      marker = list(color = 'rgb(253,174,97)'),  
                      text = ~xd_unmet) %>%   
            add_trace(y = ~xd_nodemand_fecund, type = 'bar', 
                      name = "no demand, fecund", 
                      marker = list(color = 'rgb(171,217,233)'),  
                      text = ~xd_nodemand_fecund) %>%               
            add_trace(y = ~xd_nodemand_infecund, type = 'bar', 
                      name = "no demand, infecund", 
                      marker = list(color = 'rgb(116,173,209)'),  
                      text = ~xd_nodemand_infecund) %>%                           
            layout(
                title ="Female, sexually active population by demand for FP and contraceptive use",
                yaxis = list(title = "Percent of women",
                             titlefont=list(size=12)), 
                legend = list(font=list(size=12)),
                barmode = 'stack'
                ) 
    })            
    
    output$plot_denominatorPyramid <- renderPlotly({
        
        dtafig<-filter(dta, group=="By xagegroup5" & latestIR==1 )%>%
            mutate(dummy=0)%>%
            mutate(n=xd_use+ xd_unmet+ xd_nodemand_fecund+ xd_nodemand_infecund+ xd_notSA,
                   xd_use=round(obs*xd_use/n, 1),
                   xd_unmet=round(obs*xd_unmet/n, 1),
                   xd_nodemand_fecund=round(obs*xd_nodemand_fecund/n, 1), 
                   xd_nodemand_infecund=round(obs*xd_nodemand_infecund/n, 1),
                   xd_notSA=round(obs*xd_notSA/n, 1)
                )
        panel <- . %>% 
            plot_ly(y = ~grouplabel)%>% 
            add_trace(x = ~xd_use, type = "bar",
                    name = "demand, using", 
                    marker = list(color = 'rgb(244,109,67)'),
                    text = ~xd_use) %>% 
            add_trace(x = ~xd_unmet, type = 'bar', 
                      name = "demand, not using", 
                      marker = list(color = 'rgb(253,174,97)'),  
                      text = ~xd_unmet) %>%   
            add_trace(x = ~xd_nodemand_fecund, type = 'bar', 
                      name = "no demand, fecund", 
                      marker = list(color = 'rgb(171,217,233)'),  
                      text = ~xd_nodemand_fecund) %>%               
            add_trace(x = ~xd_nodemand_infecund, type = 'bar', 
                      name = "no demand, infecund", 
                      marker = list(color = 'rgb(116,173,209)'),  
                      text = ~xd_nodemand_infecund) %>%                           
            add_trace(x = ~xd_notSA, type = 'bar', 
                      name = "not sexually active", 
                      marker = list(color = 'rgb(69,117,180)'),  
                      text = ~xd_notSA) %>%                           
            add_annotations(
                    text = ~unique(country),
                    x = 0.5, y = 1, xref = "paper", yref = "paper",    
                    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                    font = list(size = 12) )%>%
            layout(
                title ="Weighted number of sampled women by status* in each age group",
                xaxis = list(title = "", titlefont=list(size=12)), 
                yaxis = list(title = "Age group", titlefont=list(size=12)), 
                showlegend=FALSE,
                barmode = 'stack'
                ) 
        
        dtafig%>%
            group_by(country) %>%
            do(p = panel(.)) %>%
            subplot(nrows = 3, shareX = FALSE, shareY = TRUE, margin = 0.05)%>%
                layout(autosize = F, width = 1000, height = 800)
                
    })   
    
    ##### output: Psychosocial accessiblity #####
    
    output$plot_level_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" )%>%
            mutate(dummy=0)

        plot_ly(dtafig, x=~year,
                y = ~xwge_fp_existence1, type = 'bar', 
                name = "FP existence of choice 1 (NEW)", 
                marker = list(color = 'rgb(188,189,220)'),
                text = ~xwge_fp_existence1, textposition = 'outside') %>%              
            add_trace(y = ~xwge_fp_existence2, type = 'bar', 
                      name = "FP existence of choice 2 (NEW)",
                      marker = list(color = 'rgb(158,154,200)'),
                      text = ~xwge_fp_existence2, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_existence3, type = 'bar', 
                      name = "FP existence of choice 3 (NEW)",
                      marker = list(color = 'rgb(128,125,186)'),
                      text = ~xwge_fp_existence3, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_existence4, type = 'bar', 
                      name = "FP existence of choice 4 (NEW)",
                      marker = list(color = 'rgb(106,81,163)'),
                      text = ~xwge_fp_existence4, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_existence5, type = 'bar', 
                      name = "FP existence of choice 5 (NEW)",
                      marker = list(color = 'rgb(84,39,143)'),
                      text = ~xwge_fp_existence5, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_existence, type = 'bar', 
                      name = "FP existence of choice AVERAGE",
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xwge_fp_existence, textposition = 'outside') %>%  
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%

            add_trace(y = ~xwge_fp_exercise1, type = 'bar', 
                      name = "FP exercise of choice 1 (NEW)",
                      marker = list(color = 'rgb(140,150,198)'),
                      text = ~xwge_fp_exercise1, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_exercise2, type = 'bar', 
                      name = "FP exercise of choice 2 (NEW)",
                      marker = list(color = 'rgb(140,107,177)'),
                      text = ~xwge_fp_exercise2, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_exercise, type = 'bar', 
                      name = "FP exercise of choice AVERAGE",
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xwge_fp_exercise, textposition = 'outside') %>%  
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'))%>%            
            
            add_trace(y = ~xwge_fp, type = 'bar', 
                      name = "FP existence AND exercise AVERAGE",
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xwge_fp, textposition = 'outside') %>%            
            
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "WGE score (0-5)",
                             range = c(0, 5)),
                xaxis = list(title = "Survey year"),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })

    output$plot_trend_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" )%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~round, y = ~xdec, name = "contraceptive decision")%>% 
            add_trace(y = ~xwge_fp_existence1, name = "FP existence of choice 1 (NEW)", 
                     marker = list(color = 'rgb(188,189,220)'),
                     line = list(color = 'rgb(188,189,220)')) %>% 
            add_trace(y = ~xwge_fp_existence2, name = "FP existence of choice 2 (NEW)",
                      marker = list(color = 'rgb(158,154,200)'),
                      line =  list(color = 'rgb(158,154,200)')) %>% 
            add_trace(y = ~xwge_fp_existence3, name = "FP existence of choice 3 (NEW)",
                      marker = list(color = 'rgb(128,125,186)'),
                      line = list(color = 'rgb(128,125,186)')) %>% 
            add_trace(y = ~xwge_fp_existence4, name = "FP existence of choice 4 (NEW)",
                      marker = list(color = 'rgb(106,81,163)'),
                      line = list(color = 'rgb(106,81,163)')) %>% 
            add_trace(y = ~xwge_fp_existence5, name = "FP existence of choice 5 (NEW)",
                      marker = list(color = 'rgb(84,39,143)'),
                      line = list(color = 'rgb(84,39,143)')) %>%   
            add_trace(y = ~xwge_fp_existence, name = "FP existence of choice AVERAGE",
                      marker = list(color = 'rgb(63,0,125)'),
                      line = list(color = 'rgb(63,0,125)')) %>%               
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'),
                      line = list(color = 'rgb(255,255,255)'))%>%

            add_trace(y = ~xwge_fp_exercise1, name = "FP exercise of choice 1 (NEW)",
                      marker = list(color = 'rgb(140,150,198)'),
                      line =  list(color = 'rgb(140,150,198)')) %>% 
            add_trace(y = ~xwge_fp_exercise2, name = "FP exercise of choice 2 (NEW)",
                      marker = list(color = 'rgb(140,107,177)'),
                      line =  list(color = 'rgb(140,107,177)')) %>% 
            add_trace(y = ~xwge_fp_exercise, name = "FP exercise of choice AVERAGE",
                      marker = list(color = 'rgb(136,65,157)'),
                      line = list(color = 'rgb(136,65,157)')) %>%
            add_trace(y=~dummy, name = " ",
                      marker = list(color = 'rgb(255,255,255)'),
                      line = list(color = 'rgb(255,255,255)'))%>%            
            
            add_trace(y = ~xwge_fp, name = "FP existence AND exercise AVERAGE",
                      marker = list(color = 'rgb(129,15,124)'),
                      line =  list(color = 'rgb(129,15,124)')) %>% 
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "WGE score (0-5)",
                             range = c(0, 5)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8)),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })
    
    output$plot_trend_psychosocialDECISION <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" )%>%
            mutate(dummy=0)
    
        plot_ly(dtafig, x=~round, y = ~xdec, name = "contraceptive decision",
                marker = list(color = '#1f77b4'),
                line = list(color = '#1f77b4'))%>% 
            layout(
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8)),
                legend = list(font=list(size=12), 
                              orientation = "v", x=1.05, xanchor = "left", y=1, yanchor="top")
            )
        })    

    output$plot_DEMANDpattern_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="By xdenominator" & latestIR==1 )%>%
            mutate(dummy=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("demand, use", "demand, unmet", 
                                    "no demand, fecund", "no demand, infecund", 
                                    "not sexually active"))

        plotwge<-plot_ly(dtafig, x=~grouplabel,
                      y = ~xwge_fp_existence, type = 'bar', 
                      name = "FP existence of choice AVERAGE",
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xwge_fp_existence, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_exercise, type = 'bar', 
                      name = "FP exercise of choice AVERAGE",
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xwge_fp_exercise, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp, type = 'bar', 
                      name = "FP existence AND exercise AVERAGE",
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xwge_fp, textposition = 'outside') %>%      
            layout(
                yaxis = list(title = "WGE score (0-5)", range = c(0, 5)),
                xaxis = list(title = " ") )
        
        plotdec<-plot_ly(dtafig, x=~grouplabel,
                      y = ~xdec, type = 'bar', 
                      name = "Decision for contraception (%)",
                      marker = list(color = '#1f77b4'),
                      text = ~xdec, textposition = 'outside') %>%  
            layout(
                yaxis = list(title = "percent", range = c(0, 110)),
                xaxis = list(title = " ") )
        
        subplot(plotwge, plotdec, shareY = FALSE)%>%  
            layout(
                autosize = F, width = 1000, height = 400, 
                legend = list(font=list(size=10), 
                              orientation = "h", 
                              x=0.5, xanchor = "center", 
                              y=-0.3, yanchor="center"))
        
        })
    
    output$plot_SESpattern_psychosocial <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 )%>%
            mutate(dummy=0)
        
        plotwge<-plot_ly(dtafig, x=~grouplabel,
                      y = ~xwge_fp_existence, type = 'bar', 
                      name = "FP existence of choice AVERAGE",
                      marker = list(color = 'rgb(63,0,125)'),
                      text = ~xwge_fp_existence, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp_exercise, type = 'bar', 
                      name = "FP exercise of choice AVERAGE",
                      marker = list(color = 'rgb(136,65,157)'),
                      text = ~xwge_fp_exercise, textposition = 'outside') %>%  
            add_trace(y = ~xwge_fp, type = 'bar', 
                      name = "FP existence AND exercise AVERAGE",
                      marker = list(color = 'rgb(129,15,124)'),
                      text = ~xwge_fp, textposition = 'outside') %>%      
            layout(
                yaxis = list(title = "WGE score (0-5)", range = c(0, 5)),
                xaxis = list(title = " ") )
        
        plotdec<-plot_ly(dtafig, x=~grouplabel,
                      y = ~xdec, type = 'bar', 
                      name = "Decision for contraception (%)",
                      marker = list(color = '#1f77b4'),
                      text = ~xdec, textposition = 'outside') %>%  
            layout(
                yaxis = list(title = "percent", range = c(0, 110)),
                xaxis = list(title = " ") )
        
        subplot(plotwge, plotdec, shareY = FALSE)%>%  
            layout(
                autosize = F, width = 1000, height = 400, 
                legend = list(font=list(size=10), 
                              orientation = "h", 
                              x=0.5, xanchor = "center", 
                              y=-0.1, yanchor="center"))
        
        })
    
    
    ##### output: Cognitive accessiblity ##### 
    
    output$plot_level_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" ) 
    
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
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = 'v')
            )
        })
    
    output$plot_trend_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" ) 
    
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
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8)),
                legend = list(orientation = 'v')
            )
        })

    output$plot_DEMANDpattern_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="By xdenominator" & latestIR==1 )%>%
            mutate(dummy=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("demand, use", "demand, unmet", 
                                    "no demand, fecund", "no demand, infecund", 
                                    "not sexually active"))

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
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v")
            )
        })
    
    output$plot_SESpattern_cognitive <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 ) 

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
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v")
            )
        })
    
    output$plot_mcpr_cognitive <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="1" | grouplabel=="0") & latestIR==1 )%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xheard_5"|group=="By xheard_7"|group=="By xheard_10"|
                       group=="By xheard_select5"|group=="By xheard_select6")
        
        dtafig$group<-fct_relevel(factor(dtafig$group, levels = unique(dtafig$group)),
                                  c("By xheard_5", "By xheard_7", "By xheard_10", "By xheard_select5","By xheard_select6"))
        
        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "h", xanchor = "left", 
                              x = 0, y = 1)
            )                

        })    
   
    
    ##### output: Affordability #####
    
    output$plot_level_affordability <- renderPlotly({

        dtafig<-filter(dta, country==input$country & latestIR==1 & group=="All" ) 
        
        plot_ly(dtafig, x=~year, y=~xinsurance, type = 'bar', 
                name = "Have health insurance (NEW)", 
                marker = list(color = 'rgb(253,174,107)'), 
                text = ~xinsurance, textposition = 'outside')%>% 
            layout(
                autosize = F, width = 400, height = 400, 
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = "Survey year"),
                legend = list(orientation = "v"), 
                showlegend=TRUE
            )
            #add_text(text=~xinsurance, hoverinfo='none', textposition = 'top', showlegend = FALSE, textfont=list(size=10, color="black")) %>%
        })
    
    output$plot_DEMANDpattern_affordability <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="By xdenominator" & latestIR==1 )%>%
            mutate(dummy=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("demand, use", "demand, unmet", 
                                    "no demand, fecund", "no demand, infecund", 
                                    "not sexually active"))
        
        plot_ly(dtafig, x=~grouplabel, y=~xinsurance, type = 'bar', 
                name = "Have health insurance (NEW)",
                marker = list(color = 'rgb(253,174,107)'),
                text = ~xinsurance, textposition = 'outside')%>% 
            layout(
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })
    
    output$plot_SESpattern_affordability <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 ) 
        
        plot_ly(dtafig, x=~grouplabel, y=~xinsurance, type = 'bar', 
                name = "Have health insurance (NEW)",
                marker = list(color = 'rgb(253,174,107)'),
                text = ~xinsurance, textposition = 'outside')%>% 
            layout(
                yaxis = list(title = "Percent of all women",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })

    output$plot_mcpr_affordability <- renderPlotly({

        dtafig<-dta%>%filter(country==input$country & (grouplabel=="1" | grouplabel=="0") & latestIR==1 )%>%
            select(xsurvey, mcp, group, grouplabel)%>%
            filter(group=="By xinsurance")

        plot_ly(dtafig, x=~group, y=~mcp, type = 'bar', 
                color = ~grouplabel                )%>% 
            add_annotations(text = ~mcp, textposition = 'top',arrowhead = 0,
                  arrowsize = 0 )%>%
            layout(
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v")
            )                

        })    
    
    
    ##### output: Service quality: IR #####
    
    output$plot_level_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(xmii_side)==FALSE & group=="All" )%>%    
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
                             range = c(0, 110)),
                xaxis = list(title = "Survey year")
            )
        
        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group=="All" )%>%
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
                             range = c(0, 110)),
                xaxis = list(title = "Survey year")
            )
        
        subplot(fig1, fig2, nrows=1, margin=0.04, shareX=TRUE, titleY = TRUE) %>%
        layout( legend=list(orientation = 'v'), 
                yaxis = list(range = c(0, 110)) )
        
        })
    
    output$plot_trend_qualityIR <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="All" )
    
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
                             range = c(0, 110)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8))
                )
        
        dtafig<-filter(dta, country==input$country & group=="All" )

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
                             range = c(0, 110)),
                xaxis = list(title = "Survey round",
                             range = c(1, 8))
            )
        
        subplot(fig1, fig2, nrows=1, margin=0.04, shareX=TRUE, titleY = TRUE) %>%
        layout( legend=list(orientation = 'v'),
                xaxis = list(title = "Survey round",
                             range = c(1, 8))
                )
        
        })

    output$plot_DEMANDpattern_qualityIR1 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="By xdenominator" & latestIR==1 )%>%
            mutate(dummy=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("demand, use", "demand, unmet", 
                                    "no demand, fecund", "no demand, infecund", 
                                    "not sexually active"))
    
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
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })

    output$plot_DEMANDpattern_qualityIR2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group=="By xdenominator" & latestIR==1 )%>%
            mutate(dummy=0)
        
        dtafig$grouplabel<-fct_relevel(factor(dtafig$grouplabel, levels = unique(dtafig$grouplabel)),
                                  c("demand, use", "demand, unmet", 
                                    "no demand, fecund", "no demand, infecund", 
                                    "not sexually active"))

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
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })       
    
    output$plot_SESpattern_qualityIR1 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & group==input$groupIR & latestIR==1 ) 
    
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
                yaxis = list(title = "Percent of modern method (except LAM) users",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })

    output$plot_SESpattern_qualityIR2 <- renderPlotly({

        dtafig<-filter(dta, country==input$country & is.na(SDPall_essential5_noso)==FALSE & group==input$groupIR )%>%
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
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })       

    output$plot_mcpr_qualityIR <- renderPlotly({

        dtafig<-dta%>%filter((country==input$country & linkissue==0)& 
                                 (grouplabel=="1" | grouplabel=="0") &  
                                 latestIRLINK==1)%>%
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
                yaxis = list(title = "Percent of women in the group using modern methods",
                             range = c(0, 110)),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
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
                autosize = F, width = 1000, height = 400, 
                yaxis = list(title = "Percent of all FP clients",
                             range = c(0, 110) ),
                xaxis = list(title = "Survey year"), 
                legend = list(orientation = 'v')  
            )        
        
        })
    
    output$plot_SESpattern_qualityCR1 <- renderPlotly({

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
                             range = c(0, 110) ),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })
    
    output$plot_SESpattern_qualityCR2 <- renderPlotly({

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
                             range = c(0, 110) ),
                xaxis = list(title = " "),
                legend = list(font=list(size=12), 
                              orientation = "v"), 
                showlegend=TRUE
            )
        
        })    

}       

#******************************
# 3. CREATE APP 
#******************************

 shinyApp(ui = ui, server = server)