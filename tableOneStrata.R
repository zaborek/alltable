library(scales)
library(dplyr)
library(tidyr)

prettyPval <- function (x) { ifelse(x < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", x)))}
prettyPval2 <- function (x) { ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))}

tableOneStrata <- function(checkboxes = list(NULL),
                     varsCat,
                     varsNumerical = NULL,
                     preserveOrder = NULL,
                     MWWtest = NULL,
                     data,
                     showMissing = FALSE, # need to implement
                     IQR = FALSE, # need to implement
                     format = "%.2f",
                     strata) {
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    varsCatSelect <- varsCat[-which(varsCat %in% names(checkboxes))]
  } else {
    varsCatSelect <- varsCat
  }
  
  firstHalf <- data.frame()
  lastHalf <- data.frame()
  for (stratum in levels(factor(pull(data, strata)))) {
    temp <- data[pull(data, strata) == stratum,]
    
    row1 <- data.frame(Variable = "Total", Subcategory = NA,
                       Value = paste0(comma(nrow(temp)), " (", sprintf(format, nrow(temp)/nrow(data)*100), ")"),
                       type = "n (%)", stringsAsFactors = FALSE)
    
    if (length(varsNumerical) >= 1) {
      row2 <- temp %>% select(one_of(varsNumerical)) %>%
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

    row3 <- temp %>% select(one_of(varsCatSelect)) %>%
      gather(Variable, Subcategory) %>%
      mutate(Subcategory = ifelse(is.na(Subcategory), "Missing", Subcategory)) %>%
      count(Variable, Subcategory) %>%
      group_by(Variable) %>%
      mutate(Value = paste0(comma(n), " (", sprintf(format, n/sum(n)*100), ")"))
    
    row4 <- data.frame()
    if (length(which(varsCat %in% names(checkboxes))) > 0) {
      for (i in 1:length(checkboxes)) {
        row4 <- temp %>% select(one_of(checkboxes[[i]])) %>%
          gather(Variable, Subcategory) %>%
          filter(!is.na(Subcategory)) %>%
          count(Variable, Subcategory) %>%
          group_by(Variable) %>%
          mutate(Value = paste0(comma(n), " (", sprintf(format, n/sum(n)*100), ")")) %>% ungroup %>%
          filter(Subcategory == "TRUE") %>%
          mutate(Subcategory = Variable,
                 Variable = names(checkboxes)[i]) %>%
          bind_rows(row4)
      }
    }
    
    temp <- bind_rows(row3, row4) %>% select(-n)
    names(temp)[which(names(temp) == "Value")] <- stratum
    if (nrow(lastHalf) >= 1) {
      lastHalf <- full_join(lastHalf, temp)
    } else {
      lastHalf <- temp
    }
    
    
    temp <- bind_rows(row1, row2)
    names(temp)[which(names(temp) == "Value")] <- stratum
    if (nrow(firstHalf) >= 1) {
      firstHalf <- full_join(firstHalf, temp)
    } else {
      firstHalf <- temp
    }
  }
  
  headers <- data.frame(Variable = varsCat, order2 = 0, type = "n (%)", stringsAsFactors = FALSE) %>%
    rowwise %>%
    mutate(order1 = which(Variable == varsCat)) %>% ungroup
  
  orders <- data %>% select(one_of(varsCatSelect)) %>%
    gather(Variable, Subcategory) %>%
    count(Variable, Subcategory) %>%
    group_by(Variable) %>%
    mutate(order2 = rank(-n, ties.method = "first"),
           Subcategory = ifelse(is.na(Subcategory), "Missing", Subcategory),
           order2 = ifelse(Subcategory == "Missing", Inf, order2)) %>%
    ungroup %>% select(-n)
  
  if (length(preserveOrder) >= 1) {
    orders %<>% rowwise %>%
      mutate(order2 = ifelse(Variable %in% preserveOrder,
                             which(Subcategory == levels(pull(data, Variable))),
                             order2)) %>% ungroup
  }
  
  results <- lastHalf %>% full_join(orders) %>% full_join(headers %>% select(Variable, order1)) %>%
    bind_rows(headers) %>% arrange(order1, order2) %>%
    bind_rows(firstHalf, .)
  
  # numerical pvalues
  if (length(varsNumerical) >= 1) {
    temp <- data %>% select(one_of(c(varsNumerical, strata))) %>%
      gather(Variable, Subcategory, -one_of(strata))
    names(temp)[which(names(temp) == strata)] <- "strata"
    pval2 <- temp %>%
      group_by(Variable) %>%
      summarise(pval = wilcox.test(Subcategory ~ strata)$p.value)
  } else {
    pval2 <- data.frame()
  }

  # categorical pvalues
  temp <- data %>% select(one_of(c(varsCatSelect, strata))) %>%
    gather(Variable, Subcategory, -one_of(strata))
  names(temp)[which(names(temp) == strata)] <- "strata"

  pval3 <- temp %>%
    filter(!(Variable %in% MWWtest)) %>%
    group_by(Variable) %>%
    summarise(pval = chisq.test(table(Subcategory, strata), correct = FALSE)$p.value)
  
  if (length(MWWtest) >= 1) {
    for (i in MWWtest) {
      pval3 <- temp %>%
        filter(Variable == i) %>%
        group_by(Variable) %>%
        mutate(Subcategory = as.numeric(factor(Subcategory, levels = levels(pull(data, i))))) %>%
        summarise(pval = wilcox.test(Subcategory ~ strata)$p.value) %>%
        bind_rows(pval3)
    }
  }


  # checkboxes
  pval4 <- data.frame()
  if (length(which(varsCat %in% names(checkboxes))) > 0) {
    for (i in 1:length(checkboxes)) {
      temp <- data %>% select(one_of(c(checkboxes[[i]], strata))) %>%
        gather(Variable, Subcategory, -one_of(strata))
      names(temp)[which(names(temp) == strata)] <- "strata"
      pval4 <- temp %>%
        group_by(Variable) %>%
        summarise(pval = chisq.test(table(Subcategory, strata), correct = FALSE)$p.value) %>%
        rename(Subcategory = Variable) %>%
        mutate(Variable = names(checkboxes)[i]) %>%
        bind_rows(pval4)
      
    }
  }

  pvals <- bind_rows(pval2, pval3, pval4)
  
  results %>%
    full_join(pvals) %>%
    mutate(pval = prettyPval2(pval),
           Subcategory = ifelse(is.na(Subcategory), type, Subcategory),
           Variable = ifelse(Variable == lag(Variable, default = ""), "", Variable),
           hline = order2 == 0) %>%
    select(-type, -order1, -order2)
}


