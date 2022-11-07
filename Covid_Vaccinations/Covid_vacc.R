# cleaning the working directory
rm(list = ls())

# required packages
suppressMessages(library(openxlsx))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
require(utils)

# importing data
COVID19_data <- read.xlsx("assignment2.xlsx") %>% 
  mutate(date = as.Date(date, origin = "1899-12-30"))

# data summary
COVID19_data %>% select(-Number, -date) %>% summary()


bestFitFun <- function(){
  while (TRUE) {
    # message prompt
    exponential_fit <- menu(c("Exponential Fit", "Quit"), title = "MENU")
    
    if (exponential_fit == 1){
      file_name <- readline("Please enter the name of the file to open: ")
      
      if (!file.exists(file_name)){
        cat("I coud not find the file. Starting over!\n\n")
        next() # starting over
      } else {
        cat("Successful data importing!\n")
        # importing the file after getting the name
        COVID19_data <- read.xlsx(file_name) %>% 
          mutate(date = as.Date(date, origin = "1899-12-30"))
      }
      
      start_date <- readline("Please enter the start date (dd/mm/yyyy): ")
      start_date <- as.Date(start_date, format = "%d/%m/%Y")
      end_date <- readline("Please enter the end date (dd/mm/yyyy): ")
      end_date <- as.Date(end_date, format = "%d/%m/%Y")
      
      # the start date should not be less than 1/2/2021
      if (start_date < '2021-02-01' | end_date > "2021-05-01"){
        cat("The dates must fall between 01/02/2021 to 01/05/2021\n\n")
      } else if (end_date - start_date < 60){
        cat("The end date must be greater than the start date by 60 days\n\n")
      } else {
        # data select from the date range
        selected_df <- COVID19_data %>% 
          filter(date %in% seq.Date(start_date, end_date, by = 1))
        
        # linear regression
        lin_reg <- lm(log(total_vaccinations) ~ date, data = selected_df)
        cat("Estimated coefficient: \n\n", 
            "a = ", exp(coef(lin_reg)[[1]]), "\n",
            "b = ", coef(lin_reg)[[2]], "\n\n")
        
        # interpolation menu
        Extrapolation <- menu(c("Extrapolation", "Main Menu"), title = "MENU")
        if(Extrapolation == 1){
          date <- readline("Please enter the date to extrapolate to (dd/mm/yyyy): ")
          date <- data.frame(date = as.Date(date, format = "%d/%m/%Y"))
          
          pred <- predict(lin_reg, newdata = date)
          cat("The estimated total vaccinations on that date is:\n\n",
              "Est. total vacc. = ", exp(pred), "\n\n")
          
          # add the predictions to selected data
          selected_df$preds <- exp(predict(lin_reg))
          
          p <- selected_df %>% ggplot(aes(date, total_vaccinations)) + 
            geom_point(col = "navyblue") + 
            geom_line(aes(date, preds), col = "red") + 
            xlab("Date") + 
            ylab("Total vaccinations")
          
          # saving the ggplot
          suppressMessages(ggsave("total_vacc.pdf", plot = p))
          cat("The plot has been produced and saved into the working directory!\n\n")
        } else {
          # if the user enters 2, we assign the 1 to exponential_fit to allow
          # the while loop to start over.
          exponential_fit = 1
        }
      }
      
    } else {
      cat("Good bye !")
      break()
    }
  }
}


# testing the function
bestFitFun()

