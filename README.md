# mph_thesis_ml
The original paper used logistic regression using backwards selection on a number of binary variables
to attempt to predict the risk of childhood mortality. It created a simple binary risk score to aid
in calculating a child's probability of death.

My work attempts to extend this analysis by using machine learning analyses to validate this approach
and compare it to other analytical methods. In particular, I aim to produce some decision trees to aid in 
decision-making.

01_clean_data.do: 
	* Formats all variables the same way
	* Decide what to do with “9” values 
	* Exclude variables that don’t matter
	* Create variable lists: signs, symptoms, treatment, diagnoses, test results, outcomes
	* Turn all variables into binary indicators
		* May need Herbie’s guidance on indicators not included in the original analysis

02_prep_data.R
	* Apply different methods of imputation or observation-dropping
	* Output table of missing variables for all variables of interest
	* Descriptive table of primary patient characteristics
	* Any other descriptive stats

analysis_functions.R
	* Specify one function for each method — assume same data structure, and take in arguments for formula etc.
	* Logistic regression with backwards selection
		* Define variable importance cutoffs for selection (or default)
	* Decision trees
		* Define ideal tree breakdown — pruning characteristics etc.
	* Random forests
		* Source up-sampling/down-sampling methods
	* Define sampling parameters, number of cv runs, etc.
	* Output predictions, graphical representations, ROC analyses.
	* Save graphs, predictions, and ROC analyses to flat files (how to toggle by source?)

03_apply_analysis.R
	* Source all analysis functions
	* Separate data into train and test data
	* Apply analysis functions and get/plot results
