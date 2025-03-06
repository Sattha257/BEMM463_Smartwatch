import matplotlib.pyplot as plt

# Define segment positions based on Market Attractiveness & Competitive Strength
segments = ["Tech Enthusiasts", "Health & Fitness Conscious", "Style & Utility Seekers", "Traditionalists"]
market_attractiveness = [9, 9, 6, 4]  # High, High, Medium, Low
competitive_strength = [6, 9, 6, 5]  # Medium, High, Medium, Medium

# Define colors for clarity
segment_colors = ['blue', 'green', 'orange', 'red']

# Create GE Matrix
fig, ax = plt.subplots(figsize=(8, 6))

# Define a structured 3x3 grid
ax.set_xticks([3, 6, 9])
ax.set_yticks([3, 6, 9])
ax.set_xticklabels(["Low", "Medium", "High"])
ax.set_yticklabels(["Low", "Medium", "High"])
ax.set_xlim(1, 10)
ax.set_ylim(1, 10)
ax.grid(True, linestyle="--", alpha=0.5)

# Plot segments with distinct colors
for i in range(len(segments)):
    ax.scatter(market_attractiveness[i], competitive_strength[i], 
               color=segment_colors[i], s=200, label=segments[i])

# Annotate each segment on the plot
for i, segment in enumerate(segments):
    ax.text(market_attractiveness[i] + 0.2, competitive_strength[i], segment, fontsize=10, verticalalignment='center')

# Labels and title
ax.set_xlabel("Market Attractiveness")
ax.set_ylabel("Competitive Strength")
ax.set_title("GE Matrix Analysis of Smartwatch Market Segments")

# Add legend
plt.legend()

# Show the plot
plt.show()
