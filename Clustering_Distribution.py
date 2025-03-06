import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the dataset
file_path = "Smartwatch_Data_Clustered.xlsx"  # Update this with the correct file path
df = pd.read_excel(file_path, sheet_name='Sheet 1')

# Define a color palette for better visualization
cluster_colors = ['red', 'blue', 'green', 'yellow']

# Create the scatter plot for clustering distribution
plt.figure(figsize=(8, 5))
sns.scatterplot(
    x=df['Wellness'], 
    y=df['TaskMgm'], 
    hue=df['Cluster'], 
    palette=cluster_colors, 
    alpha=0.7
)

# Labels and title
plt.xlabel('Wellness Features Importance')
plt.ylabel('Task Management Importance')
plt.title('Clustering Distribution of Consumer Segments')

# Custom legend labels
plt.legend(title='Cluster', labels=['Tech Enthusiasts', 'Health & Fitness Conscious', 'Traditionalists', 'Style & Utility Seekers'])

# Grid for better readability
plt.grid(True)

# Show the plot
plt.show()
