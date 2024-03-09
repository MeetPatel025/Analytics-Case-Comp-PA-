library(readxl)
library(ggplot2)
library(scales)

# NATIONAL SPORTING GOODS MARKET
df <- read_excel("/Users/meetpatel/Desktop/Pitt Business Analytics Case Competition/Visualizations with R/sporting_goods.xlsx")
df

# Reshape the data for plotting
data_long <- tidyr::pivot_longer(df, -Total, names_to = "Category", values_to = "Value")


# Create the line chart with y-axis in scientific notation format
ggplot(data_long, aes(x = Total, y = Value, color = Category)) +
  geom_line() +
  labs(title = " National Sporting Goods Sales Over Years",
       x = "Year",
       y = "Sales",
       color = "Category") +
  scale_y_continuous(labels = label_number_si(scale = 1e-9, suffix = " Bil")) +  # Format y-axis labels in scientific notation with 'Bil' suffix
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# bar chart- total sales: 2018 to 2023
total_sales <- aggregate(Value ~ Category, data = data_long, sum)

ggplot(data_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.3) +  # Adjust thickness of bars
  geom_text(data = total_sales, aes(label = sprintf("%.2f Bil", Value/1e9)), vjust = -0.5, size = 4) +  # Display total values at the top of bars with two decimal places
  labs(title = "Total National Sporting Goods Sales by Category",
       x = "Category",
       y = "Total Sales") +
  scale_y_continuous(labels = label_number_si(scale = 1e-9, suffix = " Bil")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))  # Rotate x-axis labels


# Create the pie chart
# ggplot(data_long, aes(x = "", y = Value, fill = Category)) +
#   geom_bar(stat = "identity") +
#   coord_polar("y") +
#   theme_minimal()


# NATIONAL ATHLETIC FOOTWEAR MARKET

athletic <- read_excel("/Users/meetpatel/Desktop/Pitt Business Analytics Case Competition/Visualizations with R/athletic_footwear.xlsx")
athletic

athletic_long <- tidyr::pivot_longer(athletic, -Total, names_to = "Category", values_to = "Value")

# Create the line chart with y-axis in scientific notation format
ggplot(athletic_long, aes(x = Total, y = Value, color = Category)) +
  geom_line() +
  labs(title = " National Athletic Footwear Sales Over Years",
       x = "Year",
       y = "Sales",
       color = "Category") +
  scale_y_continuous(labels = label_number_si(scale = 1e-9, suffix = " Bil")) +  # Format y-axis labels in scientific notation with 'Bil' suffix
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# bar chart- total sales: 2018 to 2023
total_sales <- aggregate(Value ~ Category, data = athletic_long, sum)

ggplot(athletic_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.2) +  # Adjust thickness of bars
  geom_text(data = total_sales, aes(label = sprintf("%.2f Bil", Value/1e9)), vjust = -0.5, size = 4) +  # Display total values at the top of bars with two decimal places
  labs(title = "Total National Athletic Footwear Sales by Category",
       x = "Category",
       y = "Total Sales") +
  scale_y_continuous(labels = label_number_si(scale = 1e-9, suffix = " Bil")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))  # Rotate x-axis labels
