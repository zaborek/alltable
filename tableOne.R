library(scales)
library(dplyr)
library(tidyr)


# generate debug data
set.seed(123)
myDat <- mtcars %>%
  mutate(Transmission = ifelse(am, "Manual", "Automatic"),
         Engine = ifelse(vs, "Straight", "V-Shaped"),
         Yamagata = rbinom(nrow(.), size = 1, prob = runif(1)),
         Zika = rbinom(nrow(.), size = 1, prob = runif(1)),
         Hanta = rbinom(nrow(.), size = 1, prob = runif(1)),
         Cough = rbinom(nrow(.), size = 1, prob = runif(1)),
         Malaise = rbinom(nrow(.), size = 1, prob = runif(1)),
         `Sore throat` = rbinom(nrow(.), size = 1, prob = runif(1))) %>%
  mutate_at(vars(Yamagata, Zika, Hanta, Cough, Malaise, `Sore throat`), list(~as.logical(.)))

data = myDat
varsCat = c("Engine", "cyl", "gear", "PCR results", "Symptoms")
varsNumerical = c("qsec", "mpg", "disp", "hp", "drat", "wt")
preserveOrder = c("cyl", "gear")
checkboxes = list("PCR results" = c("Yamagata", "Zika", "Hanta"),
                  "Symptoms" = c("Cough", "Malaise", "Sore throat"))
format="%.2f"



##################################

prettyPval <- function (x) { ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))}

tableOne <- function(checkboxes = list(NULL),
                     varsCat,
                     varsNumerical = NULL,
                     preserveOrder,
                     data,
                     showMissing = FALSE, # need to implement
                     IQR = FALSE, # need to implement
                     format = "%.2f") {
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    varsCatSelect <- varsCat[-which(varsCat %in% names(checkboxes))]
  } else {
    varsCatSelect <- varsCat
  }
  row1 <- data.frame(Variable = "Total", Subcategory = NA, Value = comma(nrow(data)), type = "n", stringsAsFactors = FALSE)
  
  if (length(varsNumerical) >= 1) {
    row2 <- data %>% select(one_of(varsNumerical)) %>%
      gather(Variable, value) %>%
      filter(!is.na(value)) %>%
      group_by(Variable) %>%
      summarise_at(vars(value), funs(mean, sd)) %>%
      mutate(Value = paste0(sprintf(format, mean), " (", sprintf(format, sd), ")"),
             type = "mean (sd)",
             Subcategory = NA) %>%
      select(Variable, Subcategory, Value, type)
  } else {
    row2 <- data.frame()
  }
  

  row3 <- data %>% select(one_of(varsCatSelect)) %>%
      gather(Variable, Subcategory) %>%
      mutate(Subcategory = ifelse(is.na(Subcategory), "Missing", Subcategory)) %>%
      count(Variable, Subcategory) %>%
      group_by(Variable) %>%
      mutate(order2 = rank(-n),
             Value = paste0(comma(n), " (", sprintf(format, n/sum(n)*100), ")")) %>%
      rowwise %>%
      mutate(order2 = ifelse(Variable %in% preserveOrder,
                             which(Subcategory == levels(pull(data, Variable))),
                             order2),
             order2 = ifelse(Subcategory == "Missing", Inf, order2)) %>% ungroup
  
  row4 <- data.frame()
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    for (i in 1:length(checkboxes)) {
    row4 <- data %>% select(one_of(checkboxes[[i]])) %>%
      gather(Variable, Subcategory) %>%
      filter(!is.na(Subcategory)) %>%
      count(Variable, Subcategory) %>%
      group_by(Variable) %>%
      mutate(order2 = rank(-n),
             Value = paste0(comma(n), " (", sprintf(format, n/sum(n)*100), ")")) %>% ungroup %>%
      filter(Subcategory == "TRUE") %>%
      mutate(Subcategory = Variable,
             Variable = names(checkboxes)[i]) %>%
      bind_rows(row4)
    }
  }

  headers <- data.frame(Variable = varsCat, order2 = 0, type = "n (%)", stringsAsFactors = FALSE) %>%
    rowwise %>%
    mutate(order1 = which(Variable == varsCat)) %>% ungroup
    
  bind_rows(row3, row4) %>%
    rowwise %>%
    mutate(order1 = which(Variable == varsCat)) %>% ungroup %>%
    full_join(headers) %>%
    arrange(order1, order2) %>%
    bind_rows(row1, row2, .) %>%
    mutate(Subcategory = ifelse(is.na(Subcategory), type, Subcategory),
           Variable = ifelse(Variable == lag(Variable, default = ""), "", Variable),
           hline = order2 == 0) %>%
    select(-order1, -order2, -type, -n)
}


