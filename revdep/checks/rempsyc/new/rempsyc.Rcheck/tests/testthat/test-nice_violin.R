# Make the basic plot
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len"
)

# Save a high-resolution image file to specified directory
ggplot2::ggsave("niceviolinplothere.tiff",
  width = 7, height = 7, unit = "in",
  dpi = 300, path = NULL
) # change for your own desired path
# Change x- and y- axes labels
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  ytitle = "Length of Tooth",
  xtitle = "Vitamin C Dosage"
)

# See difference between two groups
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  comp1 = "0.5",
  comp2 = "2"
)

nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  comp1 = 2,
  comp2 = 3
)

# Compare all three groups
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  signif_annotation = c("*", "**", "***"),
  # manually enter the number of stars
  signif_yposition = c(30, 35, 40),
  # What height (y) should the stars appear?
  signif_xmin = c(1, 2, 1),
  # Where should the left-sided brackets start (x)?
  signif_xmax = c(2, 3, 3)
)
# Where should the right-sided brackets end (x)?

# Set the colours manually
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  colours = c("darkseagreen", "cadetblue", "darkslateblue")
)

# Changing the names of the x-axis labels
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  xlabels = c("Low", "Medium", "High")
)

# Removing the x-axis or y-axis titles
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  ytitle = NULL,
  xtitle = NULL
)

# Removing the x-axis or y-axis labels (for whatever purpose)
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  has.ylabels = FALSE,
  has.xlabels = FALSE
)

# Set y-scale manually
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  ymin = 5,
  ymax = 35,
  yby = 5
)

# Plotting individual observations
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  obs = TRUE
)

# Micro-customizations
nice_violin(
  data = ToothGrowth,
  group = "dose",
  response = "len",
  CIcap.width = 0,
  alpha = 1,
  border.colour = "black"
)
