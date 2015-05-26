# Decision Tree Classifier
from sklearn import datasets
from sklearn import metrics
#from sklearn.base import 
from sklearn.tree import DecisionTreeClassifier

#when data is not initially shaped in (n_samples, n_features), it needs to be preprocessed before being handled by scikit-learn
#the digits dataset is made of 1797 8x8 images of hand-written digits
digits = datasets.load_digits()
print digits.data.shape
#print digits.data[0]
#print digits.DESCR
import pylab as pl
pl.imshow(digits.images[-1], cmap=pl.cm.gray_r)
digitsdata = digits.images.reshape((digits.images.shape[0], -1))

# load the iris datasets
#datasets are represented as 2D arrays: the 1st axis is the samples axis, the 2nd is the features axis
iris = datasets.load_iris()
#the data is made of 150 samples with 4 features each
print iris.data.shape
#print iris.DESCR

import numpy as np
iris_X = iris.data
iris_y = iris.target
print np.unique(iris_y)

#estimator = Estimator(param1=1)
#print estimator.param1
#estimator.fit(iris.data)
#print estimator.estimated_param_

#Classification
#split data into training and test data
np.random.seed(0)
indices = np.random.permutation(len(iris_X))
iris_X_train = iris_X[indices[:-20]]
iris_y_train = iris_y[indices[:-20]]
iris_X_test = iris_X[indices[-20:]]
iris_y_test = iris_y[indices[-20:]]
#Create and fit a nearest-neighbour classifier
from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier()
print knn.fit(iris_X_train, iris_y_train)
print knn.predict(iris_X_test)
print iris_y_test

# fit a CART model to the data
model = DecisionTreeClassifier()
model.fit(iris.data, iris.target)
print(model)
# make predictions
expected = iris.target
predicted = model.predict(iris.data)
# summarize the fit of the model
print(metrics.classification_report(expected, predicted))
print(metrics.confusion_matrix(expected, predicted))
