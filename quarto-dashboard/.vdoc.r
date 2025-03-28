#
#
#
#
#
#
#
#
#
#| label: load-packages
#| message: false
library(dplyr)
library(ggplot2)
library(gt)
#
#
#
#| label: load-data
#| message: false

cake_data_raw <- read.csv("../2024_final_scores.tsv", sep = "\t", header = T)
#
#
#
#| label: prep-data
#| message: false

cake_data <- 
    cake_data_raw %>%
    mutate(month = format(as.Date(Date, format = "%Y-%m-%d"), "%m"))

n_cake <- nrow(cake_data)
#
#
#
#| label: cake origin country
#| message: false
swedish_yes <- 
    cake_data %>%
    filter(Nationality_of_the_cake == "Swedish") %>%
    count(Cake)

n_swedish <- nrow(swedish_yes)

swedish_no <- 
    cake_data %>%
    filter(Nationality_of_the_cake != "Swedish") %>%
    count(Cake)

n_world <- nrow(swedish_no)
#
#
#
#
#| content: valuebox
#| title: "Cakes"

list(
  icon = "cake",
  color = "#DDC3E3",
  value = n_cake
)
#
#
#
#| content: valuebox
#| title: "Swedish cakes"

list(
  icon = "cake-fill",
  color = "#F7E5B7",
  value = n_swedish
)
#
#
#
#| content: valuebox
#| title: "Global cakes"

list(
  icon = "cake-fill",
  color = "#CBD3AD",
  value = n_world
)
#
#
#
#
#
#| title: How much does your job title matter in baking?

cake_data %>%
    ggplot(aes(x = Job_group, y = Score)) +
    geom_boxplot(fill = "#DBD9DA", alpha = 0.7) +
    theme_minimal() +
    theme_bw() + 
    labs(x = "Job Title", y = "Score") + 
    theme(axis.text = element_text(size = 20),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1.0), 
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20), 
    legend.title = element_text(size = 20))
#
#
#
#
#| title: Who bakes better?

cake_data %>%
    ggplot(aes(x = Gender, y = Score, fill = Gender)) +
    geom_boxplot() +
    theme_minimal() +
    theme_bw() + 
    scale_fill_manual(values = c("#F5E8E1", "#D0BBA8")) + 
    labs(x = "Gender", y = "Score") + 
    theme(axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20), 
    legend.title = element_text(size = 20))
#
#
#
#
#| title: How does outdoor temperature influence final score?
cake_data %>%
    ggplot(aes(x = Temperature, y = Score)) +
    geom_line( color="darkgrey") +
    geom_point(shape=21, color="black", fill="#DBD9DA", size=6) + 
    theme_bw() + 
    theme(axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20), 
    legend.title = element_text(size = 20))
#
#
#
#
#
#
#
#| label: prepare cheesecakes

cheesecake_yes <- 
    cake_data %>%
    filter(Cheesecake == "yes")

cheesecake_yes %>%
  select(Cake, Score) %>%
  arrange(desc(Score)) %>%
  gt() %>%
  cols_align(align = "left", columns = Score)
```
#
#
#
#
#
#
#| label: Berry cakes

berries_yes <- 
    cake_data %>%
    filter(Berries == "yes")

berries_yes %>%
  select(Cake, Score) %>%
  arrange(Score) %>%
  gt() %>%
  cols_align(align = "left", columns = Score)
```
#
#
#
#
#
#
#| label: Chocolate cakes

choco_yes <- 
    cake_data %>%
    filter(Chocolate == "yes")

choco_yes %>%
    select(Cake, Score) %>%
    arrange(desc(Score))
```
#
#
#
#
