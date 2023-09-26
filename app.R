library(shiny)
library(patchwork)
library(tidyverse)

dat <- readxl::read_xlsx("Concrete_Data.xlsx")

binder <- sum(
  c(dat$Cement,dat$FlyAsh,dat$BlastFurnaceSlag,dat$Superplasticizer,dat$CoarseAggregate, dat$FineAggregate)
)
dat$Water.Cement.Ratio <- dat$Water / dat$Cement
dat$Water.Binder.Ratio <- dat$Water / binder
dat$Str_Lab <- ifelse(dat$ConcreteStr >= 55, 1, 0)

toCorr <- names(dat[,c(1:8,10:11)])
test <- list()
for (i in toCorr){
  test[[i]] <- cor.test(dat[[i]], dat$ConcreteStr)
}

res.stats <- lapply(test, "[", c("estimate", "p.value"))
stats <- do.call(rbind, lapply(res.stats, unlist))
stats <- as_tibble(stats)
stats$var <- names(test)
stats <- stats[,c(3,1,2)]

stats$p_lab <- NA
stats$p_lab <- ifelse(stats$p.value < .001, "***", "")

stats$var[stats$var == "Age_Log"] <- "Age"

str <- dat %>% filter(ConcreteStr >= 55)
nom <- dat %>% filter(ConcreteStr < 55)

#### FOR MISSING CHARTS ####
filtered <- list()
filtered$noBFS <- dat %>% filter(BlastFurnaceSlag == 0 & if_all(c(1,3:10), ~. != 0))
filtered$noFA <- dat %>% filter(FlyAsh == 0 & if_all(c(1:2,4:10), ~. != 0))
filtered$noSP <- dat %>% filter(Superplasticizer == 0 & if_all(c(1:4, 6:10), ~. != 0)) #only 5 rows
filtered$all <- dat %>% filter(if_all(c(1:10), ~. != 0))

filtered$noBFS_FA <- dat %>% filter(if_all(c(2,3), ~. == 0) & if_all(c(1,4:10), ~. != 0))
filtered$noBFS_SP <- dat %>% filter(if_all(c(2,5), ~. == 0) & if_all(c(1,3:4, 6:10), ~. != 0))
filtered$noFA_SP <- dat %>% filter(if_all(c(3,5), ~. == 0) & if_all(c(1,2,4, 6:10), ~. != 0))
filtered$noBFS_FA_SP <- dat %>% filter(if_all(c(2,3, 5), ~. == 0) & if_all(c(1, 4, 6:10), ~. != 0))

fltrChart <- list()
fltrChart$noBFS <- filtered$noBFS %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#3A506B")+
  geom_boxplot(fill = "#3A506B", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Blast Furnace Slag",
       subtitle = paste(nrow(filtered$noBFS), "rows"))

fltrChart$noFA <- filtered$noFA %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#9A8F97")+
  geom_boxplot(fill = "#9A8F97", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Fly Ash",
       subtitle = paste(nrow(filtered$noFA), "rows"))

fltrChart$noSP <- filtered$noSP %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#A11692")+
  geom_boxplot(fill = "#A11692", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Superplasticizer",
       subtitle = paste(nrow(filtered$noSP), "rows"))

fltrChart$noBFS_FA <- filtered$noBFS_FA %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#99D19C")+
  geom_boxplot(fill = "#99D19C", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Blast Furnace Slag & Fly Ash",
       subtitle = paste(nrow(filtered$noBFS_FA), "rows"))

fltrChart$noBFS_SP <- filtered$noBFS_SP %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#E7EB90")+
  geom_boxplot(fill = "#E7EB90", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Blast Furnace Slag & Superplasticizer",
       subtitle = paste(nrow(filtered$noBFS_SP), "rows"))

fltrChart$noFA_SP <- filtered$noFA_SP %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#F7A399")+
  geom_boxplot(fill = "#F7A399", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Fly Ash & Superplasticizer",
       subtitle = paste(nrow(filtered$noFA_SP), "rows"))

fltrChart$noBFS_FA_SP <- filtered$noBFS_FA_SP %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin(fill = "#88AB75")+
  geom_boxplot(fill = "#88AB75", alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "No Blast Furnace Slag, Fly Ash, & Superplasticizer",
       subtitle = paste(nrow(filtered$noBFS_FA_SP), "rows"))

fltrChart$all <- filtered$all %>% ggplot(aes(x = ConcreteStr, y = ""))+
  geom_violin()+
  geom_boxplot(alpha = .5)+
  stat_summary(fun = mean, geom= "point", shape = 21, size = 3, color = "black", fill = "red")+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"),
        text = element_text(size = 14))+
  scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
  labs(title = "All",
       subtitle = paste(nrow(filtered$all), "rows"))

#### UI ####
ui <- fluidPage(
  titlePanel("Concrete Project" %>% strong() %>% h1()),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      h3(strong("Regression:")),
      p("The regression model significantly predicted for compressive strength of HPC,", em(HTML(paste0("R", tags$sup("2")))), "= .62,", em("F"), "(7,1022) = 204.3,", em("p"), "< 2.2e-16"),
      br(),
      verbatimTextOutput("regression"),
      
      h3(strong("Logistic Regression:")),
      p("The logistic regression model significantly predicted for the probability of up-to-spec compressive strength of HPC (>= 55 MPa),", em(HTML(paste0("X", tags$sup("2")))), "= 431.37 with 8 degree of freedom,", em("p"), "< 0.001"),
      br(),
      verbatimTextOutput("logit"),
      
      h3(strong("3W")),
      h4(strong("What Went Right")),
      p("Relatively straightforward"),
      p("Information is readily available"),
      p("Shiny Dashboard"),
      h4(strong("What Did Not Go Right")),
      p("Couldn't implement Neural Network and custom tool tips"),
      p("Still don't know how to calculate water-to-binder ratio"),
      h4(strong("Improvement")),
      p("Implement Neural Network"),
      p("Incorporate both PowerPoint and dashboard into my presentation")
    ),
    
    mainPanel(
      column(12, h2(strong("Distribution"))),
      
      column(5, plotOutput("distribution")),
      column(7, plotOutput("scatterplot")),
      
      column(12, 
             selectInput(inputId = "selectBox", 
                         label =  "Select Variable:",
                         choices = c(
                           "ConcreteStr",
                           "Age",
                           "Cement",
                           "BlastFurnaceSlag",
                           "FlyAsh",
                           "Water",
                           "Superplasticizer",
                           "CoarseAggregate",
                           "FineAggregate",
                           "Water.Cement.Ratio",
                           "Water.Binder.Ratio"),
                         selected = "Cement")
             ),
      
      column(5, h3(strong("Characteristics of High Strength HPC"))),
      column(7, h2(strong("Correlation"))),
      
      column(4, plotOutput("boxComp")),
      column(8, plotOutput("corr")),
      
      column(12, h2(strong("Special Cases"))),
      column(12, plotOutput("missing", height = '500px'))
      )
    )
)

#### SERVER ####
server <- function(input, output)({
  output$scatterplot <- renderPlot({
    ggplot(dat, aes_string(x = input$selectBox))+
      geom_point(aes(y = ConcreteStr, color = Age))+
      facet_wrap(.~Age)+
      theme_bw()+
      theme(plot.title = element_text(face = "bold"),
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(title = paste("Relationship Between Concrete Strength and", input$selectBox, "grouped by Age"))
  })
  
  output$distribution <- renderPlot({
    ggplot(data = dat)+
      geom_histogram(aes_string(x = input$selectBox), fill = "#822A8D", color = "#E2A3C7")+
      theme_bw()+
      theme(plot.title = element_text(face = "bold"),
            text = element_text(size = 14))+
      labs(title = paste(input$selectBox, "Distribution"))
  })
  
  output$corr <- renderPlot({
    ggplot(stats, aes(fct_reorder(var, estimate.cor, .desc = T), estimate.cor, label = p_lab, fill = estimate.cor))+
      geom_hline(yintercept = c(-0.4, -0.2, 0, 0.2, 0.4),
                 linetype = c("dashed", "dashed", "solid", "dashed", "dashed"))+
      geom_bar(stat = "identity")+
      scale_fill_gradient2(low = "#BF3100",
                           mid = "#E6E6EA",
                           high = "#009FB7",
                           limits = c(-1, 1))+
      geom_text(hjust = -.1)+
      theme_bw()+
      coord_flip()+
      theme(axis.title.y = element_blank(),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            text = element_text(size = 14))+
      scale_y_continuous(name = "Correlation", breaks = scales::pretty_breaks(n = 7))+
      labs(title = "How Each Element Correlates with Concrete Strength",
           caption = "Number of Asterisks Indicate Level of Significance")
  })
  
  scale <- reactive({
    switch(
      input$selectBox,
      "ConcreteStr" = scale_x_continuous(limits = c(0, 100), breaks = seq(0,100,20)),
      "Cement" = scale_x_continuous(limits = c(90, 550), breaks = seq(90, 550, 100)),
      "BlastFurnaceSlag" = scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 100)),
      "FlyAsh" = scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, 50)),
      "Water" = scale_x_continuous(limits = c(100, 250), breaks = seq(100, 250, 50)),
      "Superplasticizer" = scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)),
      "CoarseAggregate" = scale_x_continuous(limits = c(800, 1200), breaks = seq(800, 1200, 100)),
      "FineAggregate" = scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)),
      "Water.Cement.Ratio" = scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, .2)),
      "Water.Binder.Ratio" = scale_x_continuous(limits = c(0.00001, 0.00015), breaks = seq(0.00001, 0.00015, 0.00002))
    )
  })
  
  output$boxComp <- renderPlot({
    str.plot <- ggplot(str, aes(!! sym(input$selectBox)))+
      geom_boxplot(fill = "#0F8B8D")+
      theme_bw()+
      theme(plot.title = element_text(face = "bold"))+
      labs(title = "Compressive Strength >= 55Mpa")+
      scale()
    
    nom.plot <- ggplot(nom, aes(!! sym(input$selectBox)))+
      geom_boxplot(fill = "#468221")+
      theme_bw()+
      theme(plot.title = element_text(face = "bold"))+
      labs(title = "Compressive Strength < 55Mpa")+
      scale()
    
    nom.plot + str.plot + plot_layout(ncol = 1)
  })
  
  output$regression <- renderPrint({
    summary(lm(data = dat, ConcreteStr ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + CoarseAggregate + FineAggregate + Age))
  })
  
  output$logit <- renderPrint({
    summary(glm(data = dat, 
        Str_Lab ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + CoarseAggregate + FineAggregate + Age, 
        family = "binomial"))
  })
  
  output$missing <- renderPlot({
    fltrChart$noBFS + fltrChart$noFA + 
      fltrChart$noSP + fltrChart$noBFS_FA +
      fltrChart$noBFS_SP + fltrChart$noFA_SP +
      fltrChart$noBFS_FA_SP + fltrChart$all
  })
})

#### APP ####
shinyApp(ui, server, options = list(display.mode = "showcase"))
