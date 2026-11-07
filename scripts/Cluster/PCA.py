import pandas as pd
from sklearn.datasets import load_iris 
from sklearn.decomposition import PCA
from sklearn import preprocessing
import matplotlib.pyplot as plt
import seaborn as sns 


def pca(df):
    """
    Expects a dataframe that has been transposed such that
        - Columns = Features
        - Rows = Observations
    
    """

    scaled = preprocessing.scale(df) # scale the data 
    pca = PCA()                      # create PCA model
    pca.fit(scaled)                  # "fit" the pca model to our data
    pca_data = pca.transform(scaled) # get our principal components
    pca_data = pd.DataFrame(pca_data)
    pca_data.columns = ["PC"+str(i) for i in list(range(1,len(pca_data.columns)+1))]

    return pca_data



def main():
    plt.show()

if __name__ =="__main__":
    data = load_iris()
    df = pd.DataFrame(data.data, columns=data.feature_names)

    pca_data = pca(df)



    sns.set_style("darkgrid")
    plt.figure(figsize=(6,4))

    # Plotting syntax in python
    sns.scatterplot(
    data=pca_data, 
    x="PC1",
    y="PC2",
    color="cornflowerblue",
    alpha=0.8,
    edgecolor="black"
    )

    plt.title("PCA plot of Iris dataset Python", fontweight="bold")

    


    main()