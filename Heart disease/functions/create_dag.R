
#### Function to Create DAG ####
create_dag <- function(){

  # Libraries
  library(dagitty)

  # Create DAG
dag <- dagitty(
    '
    dag {
      CHEST_PAIN [pos="0.724,2.825"]
      Exercise_Angina [pos="2.745,-1.247"]
      Fasting_Blood_Sugar [pos="-3.758,1.781"]
      MAX_HEART_RATE [pos="0.824,-3.242"]
      Number_of_Vessels [pos="3.879,-0.431"]
      REST_ECG [pos="-2.021,3.323"]
      RESTING_BP [pos="-3.696,-0.426"]
      ST_DEPRESSION [pos="1.797,3.587"]
      ST_SLOPE [pos="2.170,0.876"]
      AGE [pos="-1.773,-2.337"]
      CHOLESTEROL [pos="-1.290,-4.016"]
      DIAGNOSIS [outcome,pos="3.838,1.999"]
      GENDER [pos="-3.349,-2.530"]
      ISCHEMIA [latent,pos="-0.383,-0.064"]

      CHEST_PAIN -> DIAGNOSIS
      Exercise_Angina -> DIAGNOSIS
      Fasting_Blood_Sugar -> ISCHEMIA
      Number_of_Vessels -> DIAGNOSIS
      RESTING_BP -> ISCHEMIA
      ST_DEPRESSION -> DIAGNOSIS
      ST_SLOPE -> DIAGNOSIS
      AGE -> MAX_HEART_RATE
      AGE -> RESTING_BP
      AGE -> CHOLESTEROL
      AGE -> ISCHEMIA
      CHOLESTEROL -> ISCHEMIA
      GENDER -> CHOLESTEROL
      GENDER -> ISCHEMIA
      ISCHEMIA -> CHEST_PAIN
      ISCHEMIA -> Exercise_Angina
      ISCHEMIA -> MAX_HEART_RATE
      ISCHEMIA -> Number_of_Vessels
      ISCHEMIA -> REST_ECG
      ISCHEMIA -> ST_DEPRESSION
      ISCHEMIA -> ST_SLOPE
    }
    ')

#### Estemands to Estimate and Adjustments Sets  ####

## Q1.0 What is the Direct effect of Cholesterol on Diagnosis ?
q_1_0 <- adjustmentSets(x = dag,exposure = "CHOLESTEROL",outcome = "DIAGNOSIS",type = "minimal",effect = "direct")

## Q1.1 What is the Total effect of Cholesterol on Diagnosis ?
q_1_1 <- adjustmentSets(x = dag,exposure = "CHOLESTEROL",outcome = "DIAGNOSIS",type = "minimal",effect = "total")

## Q2.0 What is the Direct Effect of Age on Diagnosis ?
q_2_0 <- adjustmentSets(x = dag,exposure = "AGE",outcome = "DIAGNOSIS",type = "minimal",effect = "direct")

## Q2.1 What is the Total Effect of Age on Diagnosis ?
q_2_1 <- adjustmentSets(x = dag,exposure = "AGE",outcome = "DIAGNOSIS",type = "minimal",effect = "total")

## Q3.0 What is the Direct Effect of Resting Blood Pressure on Diagnosis ?
q_3_0 <- adjustmentSets(x = dag,exposure = "RESTING_BP",outcome = "DIAGNOSIS",type = "minimal",effect = "direct")

## Q3.1 What is the Total Effect of Resting Blood Pressure on Diagnosis  ?
q_3_1 <- adjustmentSets(x = dag,exposure = "RESTING_BP",outcome = "DIAGNOSIS",type = "minimal",effect = "total")

## Q4 Does Gender causally affect Diagnosis after adjusting for risk factors?
q_4 <- adjustmentSets(x = dag,exposure = "GENDER",outcome = "DIAGNOSIS",type = "minimal",effect = "direct")

## Q5 Effect of Fasting Blood Sugar on Diagnosis ?
q_5 <- adjustmentSets(x = dag,exposure = "Fasting_Blood_Sugar",outcome = "DIAGNOSIS",type = "minimal",effect = "direct")

# Return
return(list(
  question_1 = list(
    question = "What is the Direct effect of Cholesterol on Diagnosis",
    adjustment_set = q_1_0
  ),
  question_2 = list(
    question = " What is the Total effect of Cholesterol on Diagnosis ?",
    adjustment_set = q_1_1
  ),
  question_3 = list(
    question = "What is the Direct Effect of Age on Diagnosis",
    adjustment_set = q_2_0
  ),
  question_4 = list(
    question = "What is the Total Effect of Age on Diagnosis",
    adjustment_set = q_2_1
  ),
  question_5 = list(
    question = "What is the Direct Effect of Resting Blood Pressure on Diagnosis",
    adjustment_set = q_3_0
  ),
  question_6 = list(
    question = "What is the Total Effect of Resting Blood Pressure on Diagnosis",
    adjusment_set = q_3_1
  ),
  question_7 = list(
    question = "Does Gender causally affect Diagnosis after adjusting for risk factors?",
    adjustment_set = q_4
  ),
  question_8 = list(
    question = "what is the effect of Fasting Blood Sugar on Diagnosis",
    adjustment_set = q_5
  )))
}
