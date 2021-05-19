library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)

shooting_data <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)


# SUMMARY INFORMATION
# To start your report, you should summarize relevant features of your dataset.
# Write a paragraph providing a high-level overview of shootings in the US,
# based on the dataset. This should provide your reader with a sense of scale
# of the issue, including answers to these questions:
# - How many shootings occurred?
# - How many lives were lost?
# - Which city was most impacted by shootings
# (make sure to clarify how you are measuring "impact")?
# - Two other insights of your choice
# Data in this paragraph should reference values that you calculate in R, and
# should not simply be typed as text into the paragraph.

# This data set is about relevant information on shootings across the US. There
# have been
num_shootings <- nrow(shooting_data) #(340) shootings that occurred within this
# data set. The amount of lives lost were
total_lives_lost <- shooting_data %>%
  summarize(lives_lost = sum(num_killed)) %>%
  pull() # (373). The city that was most
# impacted (had the most deaths) was
impacted_city <- shooting_data %>%
  group_by(city) %>%
  summarize(most_impacted = sum(num_killed, na.rm = TRUE)) %>%
  arrange(desc(most_impacted)) %>%
  slice(1) %>%
  pull(city) # (Pompano Beach Parkland with 17 deaths). Below we can see the
# highest amount of deaths in one day nationwide,
highest_deaths_in_day <- shooting_data %>%
  group_by(date) %>%
  summarize(total_deaths = sum(num_killed)) %>%
  filter(total_deaths == max(total_deaths)) %>%
  pull()
# which is 17. And below we can see the highest amount of injuries in one day
# nation wide is,
highest_injuries_in_day <- shooting_data %>%
  group_by(date) %>%
  summarize(total_injuries = sum(num_injured)) %>%
  filter(total_injuries == max(total_injuries)) %>%
  pull()
# 30.


# SUMMARY TABLE
# To show a set of quantitative values to your user, you should include a well
# formatted summary table of your interest. The table should be sorted in a
# meaningful way. This should not just be the raw data, but instead should an
# aggregate table of information. How you would like to aggregate the
# information (by city, state, month, day of the week, etc.) is up to you. Make
# sure to include accompanying text that describes the important insights from
# the table.

state_killed_data <- shooting_data %>%
  group_by(state) %>%
  summarize(total_killed = sum(num_killed, na.rm = TRUE),
            total_injured = sum(num_injured, na.rm = TRUE)) %>%
  arrange(desc(total_killed)) %>%
  slice(1:10)

# My table includes the total amount killed and injured in the top 10 states.



# DESCRIPTION OF A PARTICULAR INCIDENT
# Your report will include a paragraph (4+ sentences) of in-depth information
# about a particular (single) incident. You should provide your reader with
# relevant information from the dataset, such as the date and location of the
# incident, as well as the number of people impacted (injured, killed). You
# should include a link to at least one outside resource
# (not found in the data). Data in this paragraph should reference values that
# you calculate in R, and should not simply be typed as text into the paragraph

date_of_incident <- shooting_data %>%
  group_by(date) %>%
  summarize(most_impacted = sum(num_killed, na.rm = TRUE)) %>%
  arrange(desc(most_impacted)) %>%
  slice(1) %>%
  pull(date)

location_of_incident <- shooting_data %>%
  group_by(city) %>%
  summarize(most_impacted = sum(num_killed, na.rm = TRUE)) %>%
  arrange(desc(most_impacted)) %>%
  slice(1) %>%
  pull(city)

number_killed <- shooting_data %>%
  group_by(city) %>%
  summarize(most_impacted = sum(num_killed, na.rm = TRUE)) %>%
  arrange(desc(most_impacted)) %>%
  slice(1) %>%
  pull()

number_injured <- shooting_data %>%
  group_by(city) %>%
  summarize(most_impacted = sum(num_killed, na.rm = TRUE),
            total_injured = sum(num_injured, na.rm = TRUE)) %>%
  arrange(desc(most_impacted)) %>%
  slice(1) %>%
  pull(total_injured)

number_impacted <- number_killed + number_injured

# AN INTERACTIVE MAP
# While maps are not always the most appropriate visual representation of
# geographic data, they are extraordinarily popular and attract broad audiences.
# Before rendering your make, make sure to introduce the purpose of displaying
# the map in the report (e.g., what types of comparisons it affords). You'll
# build an interactive map that shows a marker at the location of each shooting.
# On your map, manipulate the size of the markers based on the underlying
# data set (# injured, # killed, etc.). When hovered or clicked on, each point
# should provide at least 3 pieces of information about the incident
# (with a line break -- <br> -- between each piece of information) and no
# irrelevant information.

# Below your map, you must note at least 2 insights revealed by the map.

# Choice of plotting library is up to you, though I suggest you consult the
# interactive visualization chapter of the book -- remember, the map must be
# interactive.


# marker for location of each shooting
# size of markers based on # injured and # killed
# when hover / clicked, provide 3 pieces of info with <br> between each


map_df <- shooting_data %>%
  mutate(Casualties = num_killed + num_injured) %>%
  select(lat, long, Casualties, state, city, date)

interactive_map <- leaflet(data = map_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lat = ~lat, lng = ~long, stroke = FALSE,
                   radius = ~ (Casualties * 2),
    popup = paste("Date =", map_df$date, "<br>", "City =", map_df$city, "<br>",
                  "People Impacted =", map_df$Casualties))

# A plot of your choice
# In addition to the interactive map, you will build an additional plot of your
# choice to answer a specific question about your data. You can do this using
# the package of your choice, such as ggplot2, plotly, bokeh, or others.
# The choices you make should be tied directly to the question you have about
# your data.

# Similarly to your map, you should integrate your plot seamlessly with the
# rest of your report, and reference/describe it in your text. Regardless of
# library, the chart should have meaningful and clear title, axis labels, and
# legend (if appropriate).

# You should provide a defense of why you chose the visual encodings of the
# chart (i.e., you chose a layout to answer a specific question), and list at
# least 2 insights gained from the chart.

plot_df <- shooting_data %>%
  mutate(month = months(as.Date(date, "%B %d, %Y"))) %>%
  group_by(month) %>%
  summarise(occured_shootings = n())


plot_of_choice <- ggplot(data = plot_df) +
  geom_col(aes(x = month, y = occured_shootings, fill = month)) +
  xlab("Months") +
  ylab("Number of shooting") +
  ggtitle("Number of occurred shootings in each month of 2018")