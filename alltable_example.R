source("alltable.R")

# generate example data
set.seed(328)

data = mtcars
data$Transmission = ifelse(data$am, "Manual", "Automatic")
data$Engine = ifelse(data$vs, "Straight", "V-Shaped")

data$Yamagata = as.logical(rbinom(nrow(data), size = 1, prob = runif(1)))
data$Zika = rbinom(nrow(data), size = 1, prob = runif(1))
data$Hanta = rbinom(nrow(data), size = 1, prob = runif(1))
data$Cough = rbinom(nrow(data), size = 1, prob = runif(1))
data$Malaise = rbinom(nrow(data), size = 1, prob = runif(1))
data$`Sore throat` = rbinom(nrow(data), size = 1, prob = runif(1))
data$trt = sample(c("A","B"), nrow(data), replace=T)




# vanilla case, with no vars specified
allTable(data=data)

# easy case with no strata
allTable(data = data,
         varsCat = c("Engine", "cyl", "gear", "PCR results", "Symptoms"),
         varsNumerical = c("qsec", "mpg", "disp", "hp", "drat", "wt"),
         preserveOrder = c("cyl", "gear"),
         checkboxes = list("PCR results" = c("Yamagata", "Zika", "Hanta"),
                           "Symptoms" = c("Cough", "Malaise", "Sore throat")))


# harder case, add strata variable
allTable(data = data,
         varsCat = c("Engine", "cyl", "gear", "PCR results", "Symptoms"),
         varsNumerical = c("qsec", "mpg", "disp", "hp", "drat", "wt"),
         preserveOrder = c("cyl", "gear"),
         checkboxes = list("PCR results" = c("Yamagata", "Zika", "Hanta"),
                           "Symptoms" = c("Cough", "Malaise", "Sore throat")),
         strata="trt")

# other options to remove range, include sd, modify format
allTable(data = data, strata="trt", inc.sd=T, inc.range = T, format = "%.1f")

