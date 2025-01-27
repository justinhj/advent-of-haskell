import matplotlib.pyplot as plt

# Data
cpu_counts = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
elapsed_times = [1482350, 893269, 712288, 637595, 657250, 710301, 790488, 880149, 955170, 1108325, 1197059, 1344551, 1318880, 1435471, 1568019, 1571290]

# Create bar chart
plt.figure(figsize=(10, 6))
plt.bar(cpu_counts, elapsed_times, color='skyblue')
plt.xlabel('CPU Count')
plt.ylabel('Elapsed Time (µs)')
plt.title('Elapsed Time vs CPU Count')
plt.xticks(cpu_counts)
plt.grid(axis='y', linestyle='--', alpha=0.7)

# Show the chart
plt.show()
