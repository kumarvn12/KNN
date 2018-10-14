getwd()

prc <- read.csv("Prostate_Cancer_KNN.csv",stringsAsFactors = FALSE) #This command imports the required data set and saves it to the prc data frame.
#stringsAsFactors = FALSE   #This command helps to convert every 
#character vector to a factor wherever it makes sense.
str(prc)

#the first variable 'id' is unique in nature and can be removed.

prc <- prc[-1]  #removes the first variable(id) from the data set.


#The data set contains patients who have been diagnosed with either Malignant (M) or Benign (B) cancer

table(prc$diagnosis_result)  # it helps us to get the numbers of patients

#The variable diagnosis_result is our target variable 

#In case we wish to rename B as"Benign" and M as "Malignant" and 
#see the results in the percentage form

prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))


round(prop.table(table(prc$diagnosis)) * 100, digits = 1) 
# it gives the result in the percentage form rounded of to 1 decimal place( and so it's digits = 1)

#normalize the data and transform all the values to a common scale.

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

prc_n <- as.data.frame(lapply(prc[2:9], normalize))
#function lapply() applies normalize() to each feature in the data frame
#The first variable in our data set (after removal of id) is 'diagnosis_result' which is not numeric in nature. So, we start from 2nd variable. 

#Creating training and test data set

prc_train <- prc_n[1:65,]
prc_test  <- prc_n[66:100,]

#Our target variable is 'diagnosis_result' which we have not included in our training and test data sets.

prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]   
#This code takes the diagnosis factor in column 1 of the prc 
#data frame and on turn creates prc_train_labels and prc_test_labels 
#

#Training a model on data
#The knn () function needs to be used to train a model for which we 
#need to install a package 'class'.

#install.packages("class")

library(class)

#now use the knn() function to classify test data

prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)
?knn

#Evaluate the model performance

#use the CrossTable() function available in the package 'gmodels'
#install.packages("gmodels")
library(gmodels)
CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = F)
?CrossTable
#refer the ppt slide to interpret the results
