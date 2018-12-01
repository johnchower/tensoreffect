conditional_probability.tensor <- function(
  tensor # tensor object, must have named indices
, var_set # subset of indices to compute the (possibly conditional) pmf
, cond_set = c() # subset of indices to condition on
){
  margin_prob.tensor(tensor, by = union(var_set, cond_set)) /
  margin_prob.tensor(tensor, by = cond_set)
}

margin_prob.tensor <- function(
  tensor # tensor object, must have named indices
, by # subset of indices to leave after marginalization. can be empty c() or full names(tensor)
){
  full_set <- names(tensor)
  if (setequal(full_set, by)){
    out <- tensor
  } else {
    by_complement <- setdiff(full_set, by)
    out <- tensorA::margin.tensor(tensor, i = by_complement)
  }
  return(out)
}

convert_probability_to_data_tree <- function(
  effect # a probability object created by causal.effect
){
  out <- as(effect, "list")
  names(out) <- names(effect)
  as.Node(
    out
  , mode = "explicit"
  , childrenName = "children"
  )
}
