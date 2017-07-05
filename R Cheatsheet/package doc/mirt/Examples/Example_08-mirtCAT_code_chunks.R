library("mirtCAT")
options(stringsAsFactors = FALSE)

### Create simple non-adaptive interface ###

## Potential options for each item
options <- matrix(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),
    nrow = 3, ncol = 5, byrow = TRUE)

questions <- c("Building CATs with mirtCAT is difficult.",
               "mirtCAT requires a substantial amount of coding.",
               "I would use mirtCAT in my research.")

df <- data.frame(Question = questions, Option = options, Type = "radio")
df

## Run the mirtCAT web interface and store results
results <- mirtCAT(df = df)

# --------------------------------------------------------------------

### Define population parameters for CAT ###

## Define population parameters
a <- c(1, 1.2, 0.9, 0.8, 1.1, 1.2, 0.8, 0.7, 0.5, 1)
d <- c(-1, 1.5, 0, 0.5, -0.5, -1, 0, 0.1, 1.1, -0.2)
g <- rep(0.2, 10)
pars <- data.frame(a1 = a, d = d, g = g)
lc <- matrix(2)

## Generate mirt_object for CAT session
mirt_object <- generate.mirt_object(pars, itemtype = "3PL", latent_covariance = lc)
coef(mirt_object, simplify = TRUE)

## generate random response pattern
set.seed(1)
pattern <- generate_pattern(mirt_object, Theta = 1)
print(pattern)

result <- mirtCAT(mo = mirt_object, local_pattern = pattern,
                  start_item = "MI", criteria = "MI")
print(result)
summary(result)
plot(result)

responses <- summary(result, sort = FALSE)$scored_responses
fscores(mirt_object, response.pattern = responses, method = "ML")

# --------------------------------------------------------------------

## Appendix A
set.seed(1234)
nitems <- 120
itemnames <- paste0("Item.", 1:nitems)
a <- matrix(c(rlnorm(nitems/2, 0.2, 0.3), rnorm(nitems/4, 0, 0.3), numeric(nitems/2),
    rnorm(nitems/4, 0, 0.3), rlnorm(nitems/2, 0.2, 0.3)), nitems)
d <- matrix(rnorm(nitems))
pars <- data.frame(a, d)
colnames(pars) <- c("a1", "a2", "d")
trait_cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)

# create mirt_object
mod <- generate.mirt_object(pars, itemtype = "2PL", latent_covariance = trait_cov)

# math items definitions addition for one factor and multiplication for the other
questions <- answers <- character(nitems)
options <- matrix("", nitems, 5)
spacing <- floor(d - min(d)) + 1  #easier items have more variation

for (i in 1:nitems) {
    if (i < 31) {
        # addition
        n1 <- sample(1:100, 1)
        n2 <- sample(101:200, 1)
        ans <- n1 + n2
        questions[i] <- paste0(n1, " + ", n2, " = ?")
    } else if (i < 61) {
        # addition and multiplication
        n1 <- sample(1:50, 1)
        n2 <- sample(51:100, 1)
        m1 <- sample(1:10, 1)
        m2 <- sample(1:10, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(n1, " + ", n2, " + ", m1, " * ", m2, " = ?")
    } else if (i < 91) {
        # multiplication and addition
        n1 <- sample(1:10, 1)
        n2 <- sample(1:10, 1)
        m1 <- sample(1:25, 1)
        m2 <- sample(1:25, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(m1, " * ", m2, " + ", n1, " + ", n2, " = ?")
    } else {
        # multiplication
        m1 <- sample(1:50, 1)
        m2 <- sample(1:50, 1)
        ans <- n1 + n2 + m1 * m2
        questions[i] <- paste0(m1, " * ", m2, " = ?")
    }
    answers[i] <- as.character(ans)
    ch <- ans + sample(c(-5:-1, 1:5) * spacing[i, ], 5)
    ch[sample(1:5, 1)] <- ans
    options[i, ] <- as.character(ch)
}

# load list of items and their answers
df <- data.frame(Question = questions, Option = options, Answer = answers, Type = "radio")

# -------------------------------------------------------------------------
# Analysis chunks of Appendix A
plot(mod, type = 'info', theta_lim = c(-3,3))
plot(mod, type = 'SE', theta_lim = c(-3,3))

set.seed(1)
pat <- generate_pattern(mo = mod, Theta = c(-0.5, 0.5), df = df)
head(pat)

# Set min_SEM to be very small so every item is administered
result <- mirtCAT(df = df, mo = mod, local_pattern = pat, design = list(min_items = 120))
print(result)
# summary(result)
plot(result, scales = list(x = list(at = NULL)))

MCATresult <- mirtCAT(df = df, mo = mod, criteria = "Drule", local_pattern = pat,
    start_item = "Drule", design = list(min_SEM = 0.4))
print(MCATresult)
summary(MCATresult)
plot(MCATresult)

# -------------------------------------------------------------------------

# Further customizations for Appendix A GUI

type <- c("radio_inline", "select")
questions <- c("", "Which equation in the above table is INCORRECT?")
options <- rbind(c("1236", "1238", "1240", "1242", "1244"), c("A)", "B)", "C)", "D)",
    "E)"))
answers <- c("1236", "D)")
stem_locations <- c("Math-stem.html", "Table-stem.html")
twoitems <- data.frame(Question = questions, Option = options, Answer = answers,
    Stem = stem_locations, Type = type, stringsAsFactors = FALSE)

library(plyr)
df2 <- rbind.fill(twoitems, df)
GUIresults <- mirtCAT(df = df2, design = list(max_items = 4))

# --------------------------------------------------------------------

# More complete GUI example

# maximum and minimum number of items to administer, SEM criteria, and sampling exposure control
design_list <- list(max_items = 30, min_items = 10, min_SEM = 0.4, exposure = rep(3, 120))

# in pre-CAT stage select 5 items using DPrule and use EAP estimates
preCAT_list <- list(max_items = 5, criteria = "DPrule", method = "EAP")

# change aesthetics of GUI, including title, authors, header, and initial message
title <- "Example Test"
authors <- "I. M. D. Author"
firstpage <- list(h2("Example Test"), h5("Please answer each item to the best of your ability.\n                     The results of this test will remain completely anonymous\n                     and are only used for research purposes."))
lastpage <- list(h3("Thank you for completing the test. Please click 'Next' to\n                    save your results."))
demographics <- list(textInput(inputId = "occupation", label = "What is your occupation?",
    value = ""), selectInput(inputId = "gender", label = "Please select your gender.",
    choices = c("", "Male", "Female", "Other"), selected = ""))
shinyGUI_list <- list(title = title, authors = authors, demographics = demographics,
    demographics_inputIDs = c("occupation", "gender"), firstpage = firstpage, lastpage = lastpage)

# run the customized GUI interface
GUIresults <- mirtCAT(df = df, mo = mod, criteria = "Drule", start_item = "DPrule",
    shinyGUI = shinyGUI_list, design = design_list, preCAT = preCAT_list)


# --------------------------------------------------------------------

# unidimensional simulation

nitems <- 1000
N <- 5000
Theta <- matrix(rnorm(N))
a <- matrix(rlnorm(nitems, .2, .3), nitems)
d <- rnorm(nitems)
pars <- data.frame(a1 = a, d = d, g = 0.2)
mirt_object <- generate.mirt_object(pars, '3PL')
responses <- generate_pattern(mirt_object, Theta = Theta)

library(parallel)
cl <- makeCluster(detectCores())
design <- list(min_SEM = .25, min_items = 10, max_items = 50)
mirtCAT_results <- mirtCAT(mo = mirt_object, local_pattern = responses,
                           start_item = 'MI', method = 'EAP',
                           criteria = 'MI', design = design, cl = cl)

# --------------------------------------------------------------------

### Appendix B simulation
library('mirtCAT')
library('plyr')
library('mvtnorm')

nitems <- 360
N <- 1000
a <- matrix(c(rlnorm(nitems, .2, .3),
              rlnorm(nitems/3, -1, .2), numeric(nitems),
              rlnorm(nitems/3, -1, .2), numeric(nitems),
              rlnorm(nitems/3, -1, .2)), nitems, 4)
d <- data.frame(d=rnorm(nitems/2))
g <- c(rep(.2, nitems/2), rep(NA, nitems/2))
u <- c(rep(.95, nitems/2), rep(NA, nitems/2))
ds <- data.frame(matrix(rnorm(nitems*2, 0, 2), nitems/2, 4))
ints <- rbind.fill(d, ds)

pars <- data.frame(a, g, u, ints)
colnames(pars) <- c(paste0('a', 1:4), 'g', 'u', 'd', paste0('d', 1:4))
itemtype <- c(rep('4PL', nitems/2), rep('gpcm', nitems/2))
mo <- generate.mirt_object(pars, itemtype)
Theta <- rmvnorm(N, sigma = diag(4))
resp <- generate_pattern(mo, Theta)

SH <- rep(1, nitems)
SH <- ifelse(a[,1] > 1, .9, SH)
SH <- ifelse(a[,1] > 2, .6, SH)
SH <- ifelse(a[,1] > 2.5, .3, SH)
content <- rep(c('4PL', 'gpcm'), each = nitems/2)
content_prop <- c("4PL" = .70, "gpcm" = .30)
design <- list(min_SEM = c(.2, Inf, Inf, Inf), weights = c(.85, .05, .05, .05),
               max_items = 50, exposure = SH, content = content,
               content_prop = content_prop)

library('parallel')
cl <- makeCluster(detectCores())
result <- mirtCAT(mo = mo, local_pattern = resp, start_item = 'Wrule',
                  criteria = 'Wrule', design = design, cl = cl)
est.Theta1 <- laply(result, function(x) x$thetas[1])

ave_nans <- mean(laply(result, function(x) length(x$items_answered)))
empirical_contents <- ldply(result, function(x, content)
    table(content[x$items_answered]) / length(x$items_answered),
    content=content)
empirical_props <- colMeans(empirical_contents)
r <- cor(Theta[,1], est.Theta1)
bias <- mean(Theta[,1] - est.Theta1)
RMSD <- sqrt(mean((Theta[,1] - est.Theta1)^2))

# --------------------------------------------------------------------
## final chunk
n_items <- laply(result, function(x) length(x$items_answered))
xyplot(n_items ~ Theta[,1], xlab = expression(theta[g]),
       panel = function(x, y) {
           panel.xyplot(x, y)
           panel.loess(x, y, span = 0.6, col = 'red')
       },
       ylab = 'Number of items administered',
       main = 'Number of items administered by general ability')
