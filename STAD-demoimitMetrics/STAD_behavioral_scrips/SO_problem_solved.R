Hello. I am looking for a tidyverse solution of populating a column based on some condition by iterating over a vector. 
I would like a tidy solution, as my data is large and nested, unlike the following minimal example.  

Column `prev` represents previous state of a system, `new` represents the new state. `prev` and `new` are always in sequence, but some of them may loop, like in the example below.  
`0` and `100` values are start and finish states, the states represented here as letters are the important ones. 

For certain combination of `prev` and `new` (i.e. satisfying both `prev_condition` and `new_condition`), I need to iterate over a larger vector `vec` (it has more elements than possible combinations of state) and place the values in order in column `to_do`

I would like to add that combinations of `prev` and `new` do not need to be unique, this is why I added column `change` to indicate every state change that took place. 

Here is a minimal example and a solution in base R. I am really hoping for an elegant tidyverse solution. Thank you. 


library(tidyverse)

# reprex::reprex({
# Minimal reproducible data
df <- data.frame(prev = c("0", rep(letters[1:3], 2), rep(letters[4:10], 3)),
                 new = c(rep(letters[1:3], 2), rep(letters[4:10], 3), "100"),
                 change = 1:28,
                 to_do = c(rep("bla", 7), rep(NA, 21)))

# Vector for iteration
set.seed(101)
vec <- sample(LETTERS, size = 30, replace = TRUE)
vec

# Conditions
prev_condition <- c(letters[4:6])   # prev state must be any of: "d" "e" "f"
new_condition <- c(letters[5:7])    # new state must be any of: "e" "f" "g"

# base R solution
df_solved <- df
n_row <- length(df[df$prev %in% prev_condition & df$new %in% new_condition, "to_do"])
df_solved[df$prev %in% prev_condition & df$new %in% new_condition, "to_do"] <- vec[1:n_row]

df_solved
# }, venue = "SO")



df_good <- 
  df %>% 
    mutate(
      to_do = replace(to_do,
                      prev %in% prev_condition & new %in% new_condition, 
                      vec[seq_len(sum(prev %in% prev_condition & new %in% new_condition))]
    ))

identical(df_good, df_solved)





         
         
         
         

