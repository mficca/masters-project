#!/usr/bin/env python3
"""
UMAP and KMeans clustering analysis for CONVAE patient representations.

This script reads the patient representations CSV, runs UMAP dimensionality
reduction, performs an elbow analysis, applies KMeans clustering, and saves
results and plots in the UMAP/output/ directory.
"""
from pathlib import Path
import csv
import numpy as np
import pandas as pd
import umap
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans

def main():
    # ─ Determine repo root (two levels up from UMAP folder)
    base_dir = Path(__file__).resolve().parent.parent

    # ─ Input and output paths
    start_path = base_dir / "data_example" / "encodings"
    input_csv  = start_path / "convae-avg_vect.csv"

    out_dir = base_dir / "UMAP" / "output"
    out_dir.mkdir(parents=True, exist_ok=True)

    cleaned_csv = out_dir / "convae-avg_vect_cleaned.csv"
    cluster_csv = out_dir / "patient_representations_clustered.csv"

    # ─ Copy CSV WITHOUT the header row
    with open(input_csv, 'r', newline='') as inf, \
         open(cleaned_csv, 'w', newline='') as outf:
        reader = csv.reader(inf)
        next(reader, None)           # skip the original header
        writer = csv.writer(outf)
        for row in reader:
            writer.writerow(row)

    # ─ Load into DataFrame without assuming a header
    df = pd.read_csv(cleaned_csv, header=None)

    # ─ Rename columns: first is MRN, the rest are dimensions
    n_cols = df.shape[1]
    col_names = ["mrn"] + [f"dim_{i}" for i in range(1, n_cols)]
    df.columns = col_names

    # ─ Make MRN the index
    df = df.set_index("mrn", drop=True)

    # ─ Prepare data matrix (all numeric dims)
    data = df.values

    #UMAP reduction (2D)
    reducer = umap.UMAP(n_jobs=-1)
    embedding = reducer.fit_transform(data)

    # ─ Save a 2D scatter
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.scatter(embedding[:, 0], embedding[:, 1], c=np.arange(len(embedding)), s=10)
    ax.set_title('UMAP projection (2D) of CONVAE representations')
    fig.savefig(out_dir / 'umap_2d.png', dpi=300)

    #Elbow method for KMeans
    inertias = []
    ks = range(1, 15)
    for k in ks:
        km = KMeans(n_clusters=k, max_iter=1500, random_state=42)
        km.fit(embedding)
        inertias.append(km.inertia_)

    #Plot elbow
    plt.figure(figsize=(8, 6))
    plt.plot(ks, inertias, marker='o')
    plt.xlabel('Number of clusters k')
    plt.ylabel('Inertia')
    plt.title('Elbow Method for optimal k')
    plt.xticks(ks)
    plt.grid(True)
    plt.savefig(out_dir / 'elbow_kmeans.png', dpi=300)

    #Apply KMeans with chosen k
    chosen_k = 3
    kmeans = KMeans(n_clusters=chosen_k, random_state=42)
    labels = kmeans.fit_predict(embedding)

    #Append cluster labels and save
    df['cluster'] = labels
    df.to_csv(cluster_csv)

    #Plot clustered UMAP
    fig, ax = plt.subplots(figsize=(8, 6))
    scatter = ax.scatter(
        embedding[:, 0], embedding[:, 1],
        c=labels, cmap='tab10', s=10
    )
    ax.set_title(f'UMAP + KMeans (k={chosen_k})')
    legend1 = ax.legend(*scatter.legend_elements(), title='Cluster')
    ax.add_artist(legend1)
    fig.savefig(out_dir / f'umap_kmeans_k{chosen_k}.png', dpi=300)

    print(f"All outputs saved in {out_dir}")

if __name__ == "__main__":
    main()
