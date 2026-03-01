version<-"0.0.1"


library(shiny)
library(tidyverse)
library(rstan) 
#options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



probs<-"data/HN2016.csv" |>
  read.csv() |>
  arrange(rnorm(n())) |>
  mutate(t = 1:n()) |>
  pivot_longer(
    cols  = prob1L:prob3R,
    names_to = "name",
    values_to = "prob"
  ) |>
  mutate(
    lottery = ifelse(grepl("L",name),"Left","Right"),
    prize = name |> parse_number()
  )


model<-"EUT_CARA.stan" |>
  stan_model()

#-------------------------------------------------------------------------------
# User interface
#-------------------------------------------------------------------------------

ui<- fluidPage(
  tags$head(tags$style(".shiny-options-group { display: flex; gap: 16px; }")),
  uiOutput("page")
)


#-------------------------------------------------------------------------------
# SERVER
#-------------------------------------------------------------------------------

server <- function(input, output) {
  
  page <- reactiveVal("start")
  
  output$page <- renderUI({
    if (page()=="start"){
      tagList(
        h1(paste0("Estimating your risk preferences")),
        p(paste0("Version ",version)),
        p("This app will estimate your risk preferences based on some data you submit"),
        p("First, we will need to know the range of outcomes (in dollars, pounds, rupees, bragging rights,
          or whatever domain you're interested in) that we will be focusing on. Please type the worst and best possible
          outcomes in the boxes below."),
        inputPanel(
          numericInput("worst","The worst possible outcome", 10, min = -Inf, max = Inf, width = "150%"),
          numericInput("best","The best possible outcome", 30, min = -Inf, max = Inf, width = "150%")
        ),
        textOutput("check_prizes"),
        mainPanel(
          actionButton("start", "Start")
        )
      )
      
    } else if (page() == "elicitation") {
      tagList(
        h1("Estimating your risk preferences"),
        p(paste0("Version ",version)),
        p(paste0("You will be choosing between two lotteries. 
                  Each lottery is a probability disribution over three prizes: $", prizes[1], ", $",prizes[2],", and $",prizes[3],". These prizes will remain constant for the entire elicitation.
                  What will change are the probabilities associated with these prizes. 
                  For each lottery pair, you will indicate which of two lotteries you would prefer: the 'Left' or the 'Right' lottery.
                  Please treat these decisions as if you are making them in isolation.
                  That is, decide as if you are going to be paid the outcome of one of these decisions, not all of them  
                  ")),
        p("Please select either the Left or Right lottery for each of these decisions. "),
        
        mainPanel(
          uiOutput("all_plots"),
          actionButton("submit", "Submit")
        )
      )
    } else if (page() == "running") {
      tagList(
        h1("Estimating your risk preferences"),
        p(paste0("Version",version)),
        p("Stan is estimating your risk preferences")
      )
      
      } else {
      tagList(
        h1("Estimating your risk preferences"),
        p(paste0("Version ",version)),
        h2("Here is your risk preference report!"),
        p("What you will see below is a Bayesian estimate of your risk preferences. As such, you won't 
        get a single number. Instead, you will get a posterior distribution. This is a quantification
        of the uncertainty associated with your risk preference parameter after observing your
          "),
        h3("Your CARA coefficient"),
        p("First, let's take a look at our estimate of your constant absolute risk aversion (CARA) coefficient. 
          This measures how risk averse you are. If it's positive you're risk averse, and if it's negative you're risk loving
          "),
        mainPanel(
          plotOutput("CARA_posterior")
        ),
        h3("A bit about this app"),
        p("The probability distributions for the lotteries were taken from the following economic experiment:"),
        p('Harrison, Glenn W., and Jia Min Ng. "Evaluating the expected welfare gain from insurance." Journal of Risk and Insurance 83, no. 1 (2016): 91-120.'),
        p("Please contact James Bland at james dot bland at UToledo dot edu for any suggestions, corrections, and so on"),
        p("This app is provided 'as is' with no warranty or guarantee of accuracy. If  you want to take this seriously, incentivize yourself and check your HMC diagnostics."),
        actionButton("restart", "Start Over")
      )
    }
  })
  
  
  output$CARA_posterior<-renderPlot({
    
    posterior<-"Fit.csv" |> read.csv()
    
    (
      ggplot(posterior, aes(x=r))
      +geom_density()
      +theme_bw()
    )
    
  })
  
  
  
  
  output$check_prizes <- renderText({
    
    prizes<-c(input$worst,0.5*(input$worst+input$best),input$best ) 
    ifelse(input$worst>=input$best, "Please fix the lottery outcomes so that the best outcome is greater than the worst",
           "Please click 'start' to continue"
           )
           
  })
  
  output$all_plots <- renderUI({
    plot_output_list <- lapply(1:(max(probs$t)), function(i) {
      plotOutput(paste0("plot_", i))
    })
    do.call(tagList, plot_output_list)
  })
  
  # Render each plot dynamically
  observe({
    
    
    
    lapply(1:(max(probs$t)), function(i) {
      local({
        
        prizes<-c(input$worst,0.5*(input$worst+input$best),input$best ) 
        
        local_i <- i  # capture i in a new scope — this is the critical part
        output[[paste0("plot_", local_i)]] <- renderPlot({
          # your plotting code using local_i as the index
          (
            ggplot(probs |> 
                     filter(t==local_i) |>
                     mutate(prize_txt = paste0("$",prizes)[prize])
                   , aes(y=prob, x=prize_txt))
            +geom_bar(stat="identity")
            +geom_text(aes(y = prob+0.05, label = paste0(round(prob*100),"%")))
            +coord_cartesian(ylim = c(0,1.1))
            +facet_wrap(~lottery)
            +theme_bw()
            +ylab("probability")
            +xlab("")
            +labs(title = paste0("Choice ", local_i," of ", max(probs$t),": Choose the Left or Right lottery"))
          )
        })
      })
    })
  })
  
  
  
  output$all_plots <- renderUI({
    plot_rows <- lapply(1:max(probs$t), function(i) {
      tagList(
      fluidRow(
        column(9, plotOutput(paste0("plot_", i))),
        column(3, radioButtons(
          inputId  = paste0("radio_", i),
          label    = paste("Choice", i,"of",max(probs$t)),
          choices  = c("Left lottery", "Right lottery"),
          selected = character(0),
          inline = TRUE,
          width="200%"
        ))
      ),
      hr()
      )
    })
    do.call(tagList, plot_rows)
  })
  
  observeEvent(
    input$start,
    if (input$worst<input$best) {
      page("elicitation")
    }
  )
  
  observeEvent(input$restart,
               
                 page("start")

  )
  
  observeEvent(input$submit, {
    results <- data.frame(
      t    = 1:max(probs$t),
      answer  = sapply(1:max(probs$t), function(i) {
        val <- input[[paste0("radio_", i)]]
        if (is.null(val)) NA else val
      })
    ) |>
      full_join(
        probs |> pivot_wider(id_cols = t, names_from = name, values_from = prob),
        by = "t"
      ) |>
      mutate(
        prize1 = input$worst,
        prize2 = 0.5*(input$best+input$worst),
        prize3 = input$best
      )
    write.csv(results, "choice_data.csv", row.names = FALSE)
    
    
    page("running")
    
    d<-"choice_data.csv" |>
      read.csv() |>
      filter(!is.na(answer))
    
    dStan<-list(
      N = dim(d)[1],
      choiceR = 1*(d$answer=="Right lottery"),
      prizes = d |> select(prize1:prize3),
      probL = d |> select(prob1L:prob3L),
      probR = d |> select(prob1R:prob3R)
    )
    
    
    
    Fit<-model |>
      sampling(
        data = dStan,
        iter=5000
        )
    
    Fit |>
      as.data.frame() |>
      write.csv("Fit.csv")
    
    page("end")
    
  })
  
  
}





shinyApp(ui = ui, server = server)



