line_plot_EV
line_plot_Diesel
line_plot_Hybrid
line_plot_Benzin



# Assuming you have already created the individual plots: line_plot_EV, line_plot_Diesel, line_plot_Hybrid, line_plot_Benzin, line_plot_EV

# Load the required package
library(gridExtra)

# Combine the plots into one using grid.arrange
combined_plots <- grid.arrange(line_plot_EV, line_plot_Diesel, line_plot_Hybrid, line_plot_Benzin,
                               ncol = 2)  # You can adjust the number of columns as per your preference

# Display the combined plot
print(combined_plots)
