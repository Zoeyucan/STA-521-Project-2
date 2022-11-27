library(tidyverse)
library(kableExtra)
library(GGally)
library(ggcorrplot)
#read table
image1 = read.table("data/imagem1.txt",sep = "", header = FALSE)
image2 = read.table("data/imagem2.txt",sep = "", header = FALSE)
image3 = read.table("data/imagem3.txt",sep = "", header = FALSE)
image1$class = "image1"
image2$class = "image2"
image3$class = "image3"


#combind image1-3
image = rbind(image1,image2,image3)
image$class = factor(image$class)
colnames(image) = c("y_coordinate",
        "x_coordinate",
        "expert_label",
        "NDAI",
        "SD",
        "CORR",
        "Rad_DF",
        "Rad_CF",
        "Rad_BF",
        "Rad_AF",
        "Rad_AN",
        "class"
        )

#part b------------------------------------------------------------------------
##percentages
image %>%
  group_by(class) %>%
  mutate(num = n()) %>%
  group_by(class, expert_label) %>%
  summarize(percentage = paste0(round(n() / num*100,2), "%")) %>%
  distinct(percentage)%>%kbl(format = "markdown")

image %>%
  mutate(num = n()) %>%
  group_by( expert_label) %>%
  summarize(percentage = paste0(round(n() / num*100,2), "%")) %>%
  distinct(percentage)%>%kbl(format = "markdown")

## well-labeled beautiful map
#data
pixel_img1 = image %>%
  mutate(
    across(
      expert_label,
      ~ case_when(
        .x == 1 ~ "Cloud",
        .x == -1 ~ "No cloud",
        .x == 0 ~ "Unlabelled"
      )
    )
  )%>%ggplot(aes(x = x_coordinate, y = y_coordinate, color = factor(expert_label))) +
  geom_point() +
  scale_color_manual(values = c( "Cloud" = "#F4EDCA","Unlabelled" = "#C4961A","No cloud" = "#FFDB6D"),
                     name = "expert_label") +
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_bw()  + facet_grid(~class)

#save image
ggsave(
  "graphs/part1_graph1.png",
  pixel_img1,
  width = 18,
  height = 7,
  units = "cm"
)


#part c------------------------------------------------------------------------
## correlation
corr = cor(image[3:11])
c1 = ggcorrplot(corr, method = 'square',type = "upper",legend.title = "Correlation",
                lab_size  =  3,lab=TRUE,tl.cex=10,colors = c("#6D9EC1", "white", "#E46726"))
ggsave(
  "graphs/corrplot.png",
  c1,
  width = 12,
  height = 10,
  units = "cm"
)


#density plot
density_image = image[3:11] %>%
  mutate("log(SD)" = log(SD))%>%
  filter(expert_label == 1 | expert_label == -1)%>%
  mutate(
    across(
      expert_label,
      ~ case_when(
        .x == 1 ~ "Cloud",
        .x == -1 ~ "No cloud",
      )
    )
  )%>%
  pivot_longer(cols = -expert_label)%>%
  ggplot(aes(x = value, fill = factor(expert_label))) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c( "Cloud" = "#6D9EC1","No cloud" = "#E46726"),
                                           name = "expert_label")+
  facet_wrap(~name, scales = "free") +
  theme_bw()

density_image

ggsave(
  "graphs/density_image.png",
  density_image,
  width = 18,
  height = 15,
  units = "cm"
)
