# Analysis of consumer financial product complaints data, to predict whether consumer will be disputed or not?
This dataset contains complaints registered by financial product consumers. It has various attributes such as financial products, issue description, company, states, consumer disputed, etc. The CFPB does not verify the accuracy of all facts alleged in the complaints, but takes steps to confirm a commercial relationship between the consumer and the identified company exists.
For study I have considered only five attributes Product, Company, States, Issue, Tags and Consumer Disputed. To identify whether consumer will be disputed or not?

## Models used for prediction
I have used classification model to predict whether consumer will be disputed or not?. My study includes implementation of Decision, Regression trees (using rpart and Random Forest packages) and Naïve Bayes Classifier. To work with this Dataset , it is divided into training and test data.

## Dataset
Dataset has total 889556 records and 18 column attributes such as date,financial product, conusmer complaint, company name, address, consumer complaint details, company reponse, consumer disputed. We considered consumer disputed column for classification as its categorical column with Yes and No Label. After analysis, found out that dataset is imbalanced dataset, necessary steps taken to avoid bias in results, explained in approach followed.

## Process followed
1.Data Analysis for targeted columns, in my case we were looking whether consumer will be disputed or not? so identification of dependant and independant variables is important.
2.Data cleaning and filling missing values, unwanted data was removed and cleaned required column data to match required format. For missing values column accepted range of values identified and missing data is generated via approximation.
3.Dataset division to Train, Test and Validation set, dataset is randomly sampled into all three sets.
4.Imbalanced dataset issue handled by undersampling and oversampling either of it can be used, I had used both the approaches and verified my results.
5.Model implementation, identify whether its classification or clustering based on that flow will be designed and library functions will be added to project. For this project decision trees, bayseain classifier and regression trees used.
6.Testing and validation, developed model will be tested on test dataset.After achiveing required results model executed on valid dataset to check its accuracy.
7.Visualization, results are represented as confusion matrix- with false positive percentage for each model and ROC curves with respect to each model.
