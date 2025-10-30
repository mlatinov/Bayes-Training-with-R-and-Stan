
## Function to make a DAG for Howell dataset ##
howell_make_dag <- function(data){

# First DAG Modeling only the effect of weight on the height
dag_one <- dagitty(
  'dag {
bb="0,0,1,1"
age [pos="0.399,0.247"]
height [outcome,pos="0.521,0.389"]
sex [pos="0.394,0.544"]
weight [exposure,pos="0.284,0.389"]
weight -> height
}'
  )
# Define exposure and outcome
exposures(dag_one) <- "weight"
outcomes(dag_one) <- "height"
minimal_sets_1 <- adjustmentSets(dag_one, type = "minimal")

# Second DAG taking into account the age influence over weight and height
dag_two <- dagitty(
' dag {
    bb="0,0,1,1"
    age [pos="0.399,0.247"]
    height [outcome,pos="0.521,0.389"]
    sex [pos="0.394,0.544"]
    weight [exposure,pos="0.284,0.389"]
    age -> height
    age -> weight
    weight -> height
  }'
)
# Define exposure and outcome
exposures(dag_two) <- "weight"
outcomes(dag_two) <- "height"
# Minimal adjustment  Set (Expect age)
minimal_sets_2 <- adjustmentSets(dag_two, type = "minimal")

# Third DAG taking into account the sex influence over weight and height
dag_three <- dagitty(
  'dag {
bb="0,0,1,1"
age [pos="0.399,0.247"]
height [outcome,pos="0.568,0.386"]
sex [pos="0.394,0.544"]
weight [exposure,pos="0.284,0.389"]
age -> height
age -> weight
sex -> height
sex -> weight
weight -> height
}'
)
# Define exposure and outcome
exposures(dag_three) <- "weight"
outcomes(dag_three) <- "height"
# Minimal adjustment  Set (Expect age and sex)
minimal_sets_3 <- adjustmentSets(dag_three, type = "minimal")

# Return DAGs and Minimal sets
return(list(
  dag_model_level_one = list(
    dag_plot = plot(dag_one),
    minimal_set = minimal_sets_1
    ),
  dag_model_level_two = list(
    dag_plot = plot(dag_two),
    minimal_set = minimal_sets_2
  ),
  dag_model_level_three = list(
    dag_plot = plot(dag_three),
    minimal_sets = minimal_sets_3
    )
  ))
}
