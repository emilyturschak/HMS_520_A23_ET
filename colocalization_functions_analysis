library(jpeg)
library(imager)

#function for displaying/visualizing the images
display <- function(img, title) {
  par(mfrow=c(1, 2))
  plot(img, main=paste("Original", title, "Image"))
}

#function to convert an image to an array
convert_to_array <- function(img) {
  array_data <- as.array(img)
  return(array_data)
}

#function that calculates the percentage of True values in a binary array
print_percentage_true_values <- function(binary_array, label) {
  percentage_true_pixels <- mean(binary_array) * 100
  cat(sprintf("Percentage of True values for %s: %.2f%%\n", label, percentage_true_pixels))
}

#function that will print the dimensions of a given array 
print_array_dimensions <- function(binary_array, label) {
  dimensions <- dim(binary_array)
  cat(sprintf("Dimensions of %s array: %s\n", label, paste(dimensions, collapse = ", ")))
}

#function for calculating the percentage of colocalized pixels 
co_localization_percentage <- function(binary_array1, binary_array2) {
  # Compare the arrays element-wise
  co_localized_pixels <- (binary_array1 & binary_array2)
  
  # Calculate the percentage of co-localized pixels
  # sum(co_localized_pixels) counts the TRUE values in the co-localized array.
  # sum(binary_array1) counts the total TRUE values in binary_array1.
  # The division calculates the percentage of co-localized pixels relative to the total pixels in binary_array1.
  percentage_co_localized <- (sum(co_localized_pixels) / sum(binary_array1)) * 100
  
  return(percentage_co_localized)
}

# Define file paths
c1_red_path <- "/Users/emilyturschak/Desktop/HMS520_final_project/condition1/s_max_a6_sorla_eea1_8.jpg"
c1_green_path <- "/Users/emilyturschak/Desktop/HMS520_final_project/condition1/e_max_a6_sorla_eea1_8.jpg"
c2_red_path <- "/Users/emilyturschak/Desktop/HMS520_final_project/condition2/s_max_7068_sorla_eea1_8.jpg"
c2_green_path <- "/Users/emilyturschak/Desktop/HMS520_final_project/condition2/e_max_7068_sorla_eea1_8.jpg"

##I want to load and display the images I'm working with so I can see them 
# Load and display color images
c1_img_red <- load.image(c1_red_path)
c1_img_green <- load.image(c1_green_path)
c2_img_red <- load.image(c2_red_path)
c2_img_green <- load.image(c2_green_path)

# Display color images
display(c1_img_red, "C1 Red")
display(c1_img_green, "C1 Green")
display(c2_img_red, "C2 Red")
display(c2_img_green, "C2 Green")

###now that we've seen what they look like, let's convert the images to arrays so we can work with them
c1_red_array <- convert_to_array(c1_img_red)
c1_green_array <- convert_to_array(c1_img_green)
c2_red_array <- convert_to_array(c2_img_red)
c2_green_array <- convert_to_array(c2_img_green)

##now let's make them into binary arrays, this will be helpful for the colocalizing questions later
# Create binary arrays, values greater than zero = True, values that are zero = False
c1_binary_array_green <- c1_green_array > 0
c2_binary_array_green <- c2_green_array > 0
c1_binary_array_red <- c1_red_array > 0
c2_binary_array_red <- c2_red_array > 0

# Calculate and print the percentage of True values
#this is just a sanity check. we want to make sure that the percentage of the image with a color is reasonable
#for example, if any of them are 0% or 100%, something probably went wrong 
print_percentage_true_values(c1_binary_array_red, "C1 Red")
print_percentage_true_values(c1_binary_array_green, "C1 Green")
print_percentage_true_values(c2_binary_array_red, "C2 Red")
print_percentage_true_values(c2_binary_array_green, "C2 Green")

#another sanity check. we want to make sure all the dimensions are still the same (in case something earlier got messed up)
print_array_dimensions(c1_binary_array_red, "C1 Red")
print_array_dimensions(c2_binary_array_red, "C2 Red")
print_array_dimensions(c1_binary_array_green, "C1 Green")
print_array_dimensions(c2_binary_array_green, "C2 Green")

##colocalization questions!
# Co-localization for condition 1
percentage_colocalized_greenisred_c1 = co_localization_percentage(c1_binary_array_green, c1_binary_array_red)
percentage_colocalized_redisgreen_c1 = co_localization_percentage(c1_binary_array_red, c1_binary_array_green)
print(percentage_colocalized_greenisred_c1)
print(percentage_colocalized_redisgreen_c1)

#colocalization for condition 2
percentage_colocalized_greenisred_c2 = co_localization_percentage(c2_binary_array_green, c2_binary_array_red)
percentage_colocalized_redisgreen_c2 = co_localization_percentage(c2_binary_array_red, c2_binary_array_green)
print(percentage_colocalized_greenisred_c2)
print(percentage_colocalized_redisgreen_c2)

# Data for the bar graph
categories <- c('C1 co-localized Green is Red', 'C1 co-localized Red is Green', 'C2 co-localized Green is Red', 'C2 co-localized Red is Green')
percentages <- c(percentage_colocalized_greenisred_c1, percentage_colocalized_redisgreen_c1, percentage_colocalized_greenisred_c2, percentage_colocalized_redisgreen_c2)
conditions <- rep(c('Condition 1', 'Condition 1', 'Condition 2', 'Condition 2'), each = 1)

# Create a bar graph
barplot(percentages, names.arg = categories, col = c('blue', 'blue', 'orange', 'orange'), 
        main = 'Co-localization', xlab = 'Pixel Categories', ylab = 'Percentage', 
        las = 2, ylim = c(0, max(percentages) + 10), border = 'white')

# Rotate x-axis labels
axis(1, at = 1:length(categories), labels = categories, las = 2)

# Add legend
legend('topright', legend = unique(conditions), fill = c('blue', 'orange'))

# Show the plot
dev.off()
