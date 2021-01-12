#libs ----
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(DT)

#import dataset ----
sgc_data <- read.table("https://raw.githubusercontent.com/bbalint642/South_German_Credit_Dataset/main/SouthGermanCredit.asc", header = TRUE)


#data preparation - set up ----

# a numerikus valtozok kivetelevel minden valtozo faktor
for (i in setdiff(1:21, c(2,4,5,13)))
  sgc_data[[i]] <- factor(sgc_data[[i]])
# hitel celja -> faktor
sgc_data[[4]] <- factor(sgc_data[[4]], levels=as.character(0:10))


# szintek megadasa

#hitel kockazat
levels(sgc_data$credit_risk) <- c("rossz", "jó")

#fizetes
levels(sgc_data$status) = c("nincs folyószámla",
                            " < 0 DM",
                            "0<=  < 200 DM",
                            ">= 200 DM / min. 1 éve")

#hiteltorténet
levels(sgc_data$credit_history) <- c(
  "késedelmes fizetés a múltban",
  "kritikus számla / egyéb hitelek máshol",
  "nincs felvett hitel / az összes hitelt megfelelően visszafizették",
  "a meglévő hiteleket eddig megfelelően visszafizették",
  "az összes hitel megfelelőlen visszafizetve ennek a banknak")

#hitelek celjai
levels(sgc_data$purpose) <- c(
  "egyéb",
  "autó (új)",
  "autó (használt)",
  "bútor/berendezések",
  "rádió/televízió",
  "háztartási készülékek",
  "javítások",
  "oktatás", 
  "üdülés",
  "átképzés",
  "üzleti")

#megtakaritasok
levels(sgc_data$savings) <- c("ismeretlen/nincs megtakarítási számla",
                              "... <  100 DM", 
                              "100 <= ... <  500 DM",
                              "500 <= ... < 1000 DM", 
                              "... >= 1000 DM")
#alkalmazott
levels(sgc_data$employment_duration) <- 
  c(  "munkanélküli", 
      "< 1 év", 
      "1 <= ... < 4 év",
      "4 <= ... < 7 év", 
      ">= 7 év")

#torleszto reszlet
sgc_data$installment_rate <- ordered(sgc_data$installment_rate)
levels(sgc_data$installment_rate) <- c(">= 35", 
                                       "25 <= ... < 35",
                                       "20 <= ... < 25", 
                                       "< 20")
#egyeb adosok
levels(sgc_data$other_debtors) <- c(
  "nincs",
  "társ-pályázó",
  "kezes"
)

#nemek
# megozvegyult noket nem emlitett az ertelmezo tabla
levels(sgc_data$personal_status_sex) <- c(
  "férfi : elvált",
  "nő : nem egyedülálló vagy férfi : egyedülálló",
  "férfi : házas/özvegy",
  "nő : egyedülálló")


#lakhely
sgc_data$present_residence <- ordered(sgc_data$present_residence)
levels(sgc_data$present_residence) <- c("< 1 év", 
                                        "1 <= ... < 4 év", 
                                        "4 <= ... < 7 év", 
                                        ">= 7 év")

#legertekesebb tulajdonok
levels(sgc_data$property) <- c(
  "ismeretlen / nincs tulajdon",
  "autó vagy egyéb",
  "építő társaságok megtakarítási megállapodása/életbiztosítás", 
  "ingatlan")

#egyeb reszletfizetesi tervek
levels(sgc_data$other_installment_plans) <- c(
  "bank",
  "boltok",
  "nincs")

#lakhatas modja
levels(sgc_data$housing) <- c("ingyenesen", "bérlés", "saját tulajdon")

#hitelek
sgc_data$number_credits <- ordered(sgc_data$number_credits)
levels(sgc_data$number_credits) <- c("1", "2-3", "4-5", ">= 6")

#munka tipusa
levels(sgc_data$job) <- c(
  "munkanélküli / szakképzetlen - nem rezidens",
  "szakképzetlen - rezidens",
  "képzett alkalmazott / tisztviselő",
  "menedzser / önálló alkalmazású / magasan képzett munkavállaló")

#anyagilag fuggok szama
levels(sgc_data$people_liable) <- c("3 vagy tobb", "0 - 2")

#vezetekes telefon
levels(sgc_data$telephone) <- c("nincs", "van (ügyfél neve alatt)")

#kulfoldi munkavallalo
levels(sgc_data$foreign_worker) <- c("igen", "nem")



#plots ----
# kockázat - foglalkoztatás ideje és folyószámla állapota alapjan

# Jó és rossz adósok aránya
good_and_bad <- ggplot(sgc_data, aes(x = credit_risk, fill = credit_risk)) +
  theme_linedraw() +
  geom_bar() +
  labs(x="", y="Adósok (fő):", title = "Jó es rossz adósok aránya")



risk_plot1 <- ggplot(sgc_data, aes(x = status, fill = credit_risk)) +
  theme_bw() +
  facet_wrap(~ employment_duration) +
  geom_bar() +
  labs(x="kockázat", y="adósok szama:", title = "Hitelkockázat a fizetés és foglalkoztatás időtartamának függvényében")

# anyagilag függők arányai (összes): 
liable_risk <- ggplot(sgc_data, aes(x = people_liable, fill = people_liable)) +
  theme_linedraw() +
  geom_bar() +
  labs(x="", y="Adósok (fő):", title = "Az adósoktól anyagilag függők arányai")

#anyagilag függők + status + kockázat
dependet_ppl <- ggplot(sgc_data, aes(x = status, fill = credit_risk)) +
  theme_bw() +
  facet_wrap(~ people_liable) +
  geom_bar() +
  labs(x="anyagilag függők", y="adósok szama:", title = "Kockázat az anyagilag függők és a fizetés függvényében")

#külföldi dologzók
foreign_workers_plot <- ggplot(sgc_data, aes(x = foreign_worker, fill = credit_risk)) +
  theme_linedraw() +
  geom_bar() +
  labs(x="", y="Adósok (fő):", title = "Kockázat a külföldi munkavállalók esetében")

#nem és status
sex_status <- ggplot(sgc_data, aes(x = status, fill = personal_status_sex)) +
  theme_bw() +
  facet_wrap(~ credit_risk) +
  geom_bar() +
  labs(x="anyagilag függők", y="adósok szama:", title = "Kockázat aránya a státusz és a fizetés függvényében")


## plotly ----

#interaktív áttekintőbe tartozó plotok 


factor(sgc_data$status)
status_to_table = table(sgc_data$status)
table_status = as.data.frame(status_to_table)
names(table_status)[1] = 'status'
table_status


statuszok <- plot_ly(
  x = c("nincs folyószámla", "< 0 DM", "0<=  < 200 DM", ">= 200 DM / min. 1 éve"),
  y = c(274, 269, 63, 394),
  name = "status",
  type = "bar"
) %>%
  layout(title="Folyószámla státusza / havi bevétel")
statuszok


#frequency count of multiple variables:

apply(sgc_data[c("status", "credit_risk", "employment_duration")], 2, table)


labels = c('munkanélküli', '< 1 év', '1 <= ... < 4 év', '4 <= ... < 7 év', '>= 7 év')
values = c(62, 172, 339, 174, 253)

pie_chart1 <- plot_ly(type='pie', labels=labels, values=values, 
                      textinfo='label+percent',
                      insidetextorientation='radial') %>%
  layout(title="Alkalmazásban eltöltött idő")
pie_chart1


#státusz és credit_risk

figure1 <- sgc_data
figure1 <- figure1 %>% count(status, credit_risk)
figure1 <- figure1 %>% plot_ly(x = ~status, y = ~n, color = ~credit_risk) %>%
  layout(title="Státusz és kockázat")

figure1




#shiny code ----

#user interface
ui <- dashboardPage( skin = "green",
                     
                     #header                        
                     dashboardHeader(title = "South German Credit"), #dashboardHeader()
                     
                     dashboardSidebar(
                       
                       sidebarMenu(
                         # Setting id makes input$tabs give the tabName of currently-selected tab
                         id = "tabs",
                         menuItem("Áttekintő", tabName = "dashboard", icon = icon("dashboard")), #menuitem(áttekintő)
                         
                         menuItem("Interakítv áttekintő", tabName = "interactive_dashboard", icon =icon("dashboard")), #interaktív ábrák
                         
                         
                         menuItem("Filter", tabName = "dataselect1", icon = icon("filter")), #menuitem(dataselect2)
                         
                         menuItem("Dolgozók, nem és státusz", icon = icon("bar-chart-o"),
                                  menuSubItem("Külföldi dolgozó", tabName = "subitem1"),
                                  menuSubItem("Nem és státusz", tabName = "subitem2")
                         ), #menuItem(Dolgozók)
                         
                         
                         
                         menuItem("Táblázat", icon = icon("list"), tabName = "dataset") #menuItem(táblázat)       
                         
                         
                       ) #sidebarMenu()
                       
                     ), #dashboardSidebar()
                     
                     #body      
                     dashboardBody(
                       
                       
                       tabItems(
                         tabItem("dashboard",
                                 div(h1("Áttekintő"),
                                     
                                     box(plotOutput("plot1"), width = 9), #plot1
                                     box(plotOutput("plot2"), width = 3), #plot2
                                     box(plotOutput("plot3"), width = 9), #plot3
                                     box(plotOutput("plot4"), width = 3) #plot4
                                 ), #div(p(Áttekintő))
                                 
                         ), #tabItem(dashboard)
                         
                         tabItem("interactive_dashboard",
                                 div(h1("Interakítv áttekintő"),
                                     box(plotlyOutput("plot7"), width = 7),#plot7
                                     box(plotlyOutput("plot8"), width = 5), #plot8
                                     box(plotlyOutput("plot9"), width = 7) #plot9
                                     
                                     
                                     
                                 ), #div(p(interaktív áttekintő))
                                 
                         ), #tabItem(dashboard)                         
                         
                         
                         tabItem("dataselect1",
                                 div(h1("Különböző értékek a külföldi dolgozók esetében"),
                                     box(plotOutput("sgc_plot1"), width = 5),
                                     box(selectInput("adatok", "Futamidő/Összeg/Életkor:",
                                                     c("duration", "amount", "age")), width = 2)
                                     
                                 ), #div(p())
                                 
                         ), #tabItem(dashboard)
                         
                         tabItem("subitem1",
                                 h1("Külföldi dolgozók"),
                                 box(plotOutput("plot5"), width = 3) #plot5
                                 
                                 
                         ), #subitem1
                         
                         tabItem("subitem2",
                                 h1("Nem és státusz"),
                                 box(plotOutput("plot6"), width = 7)#plot6
                         ), #subitem2
                         
                         
                         
                         tabItem("dataset",
                                 fluidPage(
                                   fluidRow(
                                     
                                     h1("South German Credits adathalmaz"), #h1
                                     selectInput("SelectColumns", "Változók választása:", choices = names(sgc_data), multiple = TRUE),
                                     dataTableOutput("sg_credit")
                                     
                                   )
                                   
                                 ) #fluidPage()
                                 
                         ) #dataset
                       ) #tabItems()
                       
                       
                     ) #dashboardBody()
                     
                     
) #dashboardPage()

#server
server <- function(input, output) {
  
  #rendering plots ---- 
  output$plot1 <- renderPlot(risk_plot1)
  output$plot2 <- renderPlot(good_and_bad)
  output$plot3 <- renderPlot(dependet_ppl)
  output$plot4 <- renderPlot(liable_risk)
  output$plot5 <- renderPlot(foreign_workers_plot)
  output$plot6 <- renderPlot(sex_status)
  output$plot7 <- renderPlotly(statuszok)
  output$plot8 <- renderPlotly(pie_chart1)
  output$plot9 <- renderPlotly(figure1)
  
  #válogató1
  
  output$sgc_plot1 <- renderPlot({
    plot(sgc_data$foreign_worker, sgc_data[[input$adatok]],
         xlab= "Külföldi dolgozó", ylab = "Érték")
  })
  
  
  
  
  output$sg_credit <- renderDataTable(
    sgc_data %>% select(!!! rlang::syms(input$SelectColumns)),
    options = list(scrollX = TRUE)
  )
  
}

#shinyapp function call ----

shinyApp(ui, server)

