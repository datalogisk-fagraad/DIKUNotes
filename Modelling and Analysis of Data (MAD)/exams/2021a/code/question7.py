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
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA


### QUESTION A

seeds_data = pd.read_csv("../data/seedsDataset.txt").values.astype(np.float64)
mean_data = seeds_data.mean(0)
std_data = seeds_data.std(0)
norm_data = (seeds_data - mean_data)/std_data


### QUESTION B

def n_euclidean_distance(point_a, point_b):
    return np.linalg.norm(point_a-point_b)

def pick_random_entry(data, amount=1):
    return data[np.random.choice(data.shape[0], amount, replace=False), :]


def kMean(points, centroids):
    groups = [[] for x in range(centroids.shape[0])]
    intra_distance = 0

    for idx1, point in enumerate(points):
        closest = (-1, 0)
        for idx2, centroid in enumerate(centroids):
            distance = n_euclidean_distance(point, centroid)
            closest_distance, closest_index = closest
            if (closest_distance == -1 or distance < closest_distance):
                closest = (distance, idx2)
        groups[closest[1]].append(point)
        intra_distance = intra_distance + closest[0]**2
    new_centroids = []
    for idx, group in enumerate(groups):
        new_centroids.append(np.mean(group, 0))
    new_centroids = np.array(new_centroids)
    if (np.array_equal(centroids, new_centroids)):
        return groups, new_centroids, intra_distance
    return kMean(points, new_centroids)

def kMean_optimized(points, clusters = 3, iterations = 5):
    best = (np.zeros(0), np.zeros(0), -1)
    for i in range(iterations):
        points, centroids, intra_distance = kMean(norm_data, pick_random_entry(norm_data, clusters))
        if (best[2] == -1 or intra_distance < best[2]):
            best = (points, centroids, intra_distance)
    return best

groups, centroids, intra_distance = kMean_optimized(norm_data)
print("Smallest intra-cluster distance: ", intra_distance)


### QUESTION C

def kMean_cluster_counts(groups):
    return [np.array(x).shape[0] for x in groups]

print("Number of samples in each group: ", kMean_cluster_counts(groups))


### QUESTION D AND E
def __PCA(data):
    pca = PCA(n_components=2)
    return pca.fit(data)


## The order is the same, so the entries in our PCA data should map directly onto our groups :)
pca_flattened = []
group_map = []
for idx, group in enumerate(groups):
    for point in group:
        group_map.append(idx)
        point = np.array(point)
        pca_flattened.append(point)
pca_flattened = np.array(pca_flattened)
pca_data = __PCA(pca_flattened)
transformed_data = pca_data.transform(pca_flattened)
transformed_centroids = pca_data.transform(centroids)

def visualize(points, centroids, group_map):
    plt.figure()
    cmap_light = ListedColormap(['#FFAAAA', '#AAFFAA', '#AAAAFF'])
    cmap_bold  = ListedColormap(['#FF0000', '#00FF00', '#0000FF'])
    plt.title("Clusters")
    plt.scatter(points[:, 0], points[:, 1], c = group_map, cmap = cmap_bold)
    plt.scatter(centroids[:, 0], centroids[:, 1], c = "#000000", s=200)
    plt.xlim(points[:, 0].min() - 0.1, points[:, 0].max() + 0.1)
    plt.ylim(points[:, 1].min() - 0.1, points[:, 1].max() + 0.1)
    plt.show()
    t = 0

visualize(transformed_data, transformed_centroids, group_map)
    