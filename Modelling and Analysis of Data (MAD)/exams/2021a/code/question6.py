import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import random
import matplotlib.cm as cm
import numpy.matlib
from sklearn.metrics import average_precision_score
from matplotlib.colors import ListedColormap
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier

### QUESTION A

training_data = pd.read_csv("../data/accent-mfcc-data_shuffled_train.txt").values.astype(np.float64)
validation_data = pd.read_csv("../data/accent-mfcc-data_shuffled_validation.txt").values.astype(np.float64)

training_features = training_data[:,1:].astype(np.float64)
training_labels = training_data[:,0:1].astype(np.int64)
validation_features = validation_data[:,1:].astype(np.float64)
validation_labels = validation_data[:,0:1].astype(np.int64)
def __randomForests(trainingFeatures, trainingLabels, criterion = "gini", max_features = "auto", max_depth = None):
    predictor = RandomForestClassifier(criterion=criterion, max_features=max_features, max_depth=max_depth)
    predictor.fit(trainingFeatures, trainingLabels.ravel())
    return predictor

def accuracy(predictor, validation_features, validation_labels):
    correct = 0
    count = 0
    probabilities = predictor.predict_proba(validation_features)
    chance_of_correct = []
    correct_map = []
    for idx, valid_label in enumerate(predictor.predict(validation_features)):
        count = count + 1
        chance_of_correct.append(probabilities[idx,valid_label])
        if(valid_label == validation_labels[idx]):
            correct = correct +1
            correct_map.append(1)
            continue
        correct_map.append(0)
    return correct / count, correct, np.mean(chance_of_correct)


predictor = __randomForests(training_features, training_labels)
precision, correct, probability_mean = accuracy(predictor, training_features, training_labels)
print("Test Accuracy: ", precision)


### QUESTION B and C
criterions = ["gini", "entropy"]
max_features = ["sqrt", "log2"]
max_depths = [2, 5, 7, 10, 15]
best_metric = ("", "", 0, 0, 0)
for criterion in criterions:
    for max_feature in max_features:
        for max_depth in max_depths:
            predictor = __randomForests(training_features, training_labels, criterion, max_feature, max_depth)
            precision, correct, probability_mean = accuracy(predictor, validation_features, validation_labels)

            best_criterion, best_max_feature, best_max_depth, best_correct, best_mean = best_metric
            if (correct < best_correct):
                continue
            if (correct == best_correct and probability_mean < best_mean):
                continue
            best_metric = (criterion, max_feature, max_depth, correct, probability_mean)
            # C part:
            print(f"criterion = {criterion}; max_depth = {max_depth}); max_features = {max_feature}; accuracy on validation data = {precision}; number of correctly classified validation samples = {correct};" )








