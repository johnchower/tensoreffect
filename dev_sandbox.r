library(dplyr)
library(readr)
library(forcats)
library(ggplot2)
library(causaleffect)
library(igraph)
library(tensorA)
source(glue::glue("{here::here()}/helper_functions.r"))
source(glue::glue("{here::here()}/get_expression.r"))
library(data.tree)
library(latex2exp)

g <- graph.formula(
  y -+ x
, y -+ z
, x -+ z
)
plot(g)

effect <- causal.effect(
  y = "y"
, x = "z"
, G = g
, primes = TRUE
, simp = TRUE
, prune = TRUE
, expr = FALSE
)
cat(get.expression(effect), "\n")

cat(get.operation(effect), "\n")
TeX(glue::glue("${get.expression(effect)}$"))

g <- graph.formula(
  smoke -+ tar
, tar -+ cancer
, hidden -+ smoke
, hidden -+ cancer
)
plot(g)

effect <- causal.effect(
  y = "cancer"
, x = "smoke"
, G = g
, primes = TRUE
, simp = TRUE
, prune = TRUE
, expr = FALSE
)
cat(get.expression(effect, primes = TRUE), "\n")

effect_tree <- convert_probability_to_data_tree(effect)

# UC Berkeley data

g <- graph.formula(
  Gender -+ Admit
, Dept -+ Gender
, Dept -+ Admit
)
plot(g)

effect <- causal.effect(
  y = "Admit"
, x = c("Gender")
, G = g
, primes = TRUE
, simp = TRUE
, prune = TRUE
, expr = FALSE
)
get.expression(effect)
effect_tree <- convert_probability_to_data_tree(effect)

data(UCBAdmissions) 

UCBAdmissions_tensor <- UCBAdmissions %>% {
  out <- to.tensor(as.vector(.), dim(.), ndimnames = dimnames(.))
  dimnames(out) <- dimnames(.)
  return(out)
}
UCBAdmissions_tensor

UCBAdmissions_tensor %>% {
term_1 <- conditional_probability.tensor(
  .
, var_set = "Admit"
, cond_set = c("Gender", "Dept")
)
term_2 <- conditional_probability.tensor(
  .
, var_set = "Dept"
)
term_1 %e% term_2
}

margin.tensor(UCBAdmissions_tensor, i = c("Admit", "Gender", "Dept"))
UCBAdmissions_tensor %>% ftable

# Adapt the get.expression function to tensor operations
# Write tests for each of the possibilities:
# Just a single probability statement
# A single conditional probability statement
# A product of two distributions
# A einstien summation
# A division
# A remaining do() (throws error; unidentifiable)
# Something with primes in it
# An initial test to make sure it returns a function which acts on tensors, and
# throws a warning when the input tensor has the wrong indices
