# Library
library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p


#### Top 6 research output - Faculty ####

ResearchOutput_Faculty<- ResearchOutputs_checkbox_long %>%
  drop_na(faculty) %>% 
  group_by(`Research Output`, faculty) %>%
  count() %>%
  drop_na(`Research Output`) %>%  
  arrange(desc(`Research Output`)) %>% 
  mutate(faculty = factor(faculty, levels=faculty)) %>%
  filter(faculty!="Other/Not Applicable") %>% 
  mutate(`Research Output` = factor(`Research Output`, levels=`Research Output`),
         Faculty_Short = case_when(
           faculty == "Faculty of Medicine and Health" ~ "Medicine and Health",
           faculty == "Faculty of Architecture, Design and Planning" ~ "Architecture, Design & Planning",
           faculty == "Faculty of Engineering" ~ "Engineering",
           faculty == "Faculty of Arts and Social Sciences" ~ "Arts & Social Sciences",
           faculty == "Faculty of Law" ~ "Law",
           faculty == "Faculty of Science" ~ "Science",
           faculty == "Business School" ~ "Business")) %>%
  filter(`Research Output` %in% c("Thesis/Dissertation","Report","Presentation",
                                  "Poster","Journal Article","Conference","Book or Book Chapter"))

nodes_test <- data.frame(
  name=c(as.character(ResearchOutput_Faculty$`Research Output`), 
         as.character(ResearchOutput_Faculty$Faculty_Short)) %>% unique())

ResearchOutput_Faculty$IDsource <- match(ResearchOutput_Faculty$`Research Output`, nodes_test$name)-1 
ResearchOutput_Faculty$IDtarget <- match(ResearchOutput_Faculty$Faculty_Short, nodes_test$name)-1

plot <- sankeyNetwork(Links = ResearchOutput_Faculty, Nodes = nodes_test,
                      Source = "IDsource", Target = "IDtarget",
                      Value = "n", NodeID = "name", 
                      sinksRight=FALSE)
plot  



#### Top 6 research output - Faculty - Tool ####
#### Using ggsankey package
#### Ref: https://rpubs.com/techanswers88/sankey-with-own-data-in-ggplot

# install.packages("remotes")
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

ResearchOutput_Faculty_Tool <- 
  ResearchOutputs_checkbox_long %>%
  drop_na(faculty) %>% 
  drop_na(`Research Output`) %>%  
  # arrange(desc(`Research Output`)) %>%
  filter(faculty!="Other/Not Applicable") %>%
  select(`Research Output`,faculty,Technology) %>% 
  mutate(Faculty = case_when(
           faculty == "Faculty of Medicine and Health" ~ "Medicine and Health",
           faculty == "Faculty of Architecture, Design and Planning" ~ "Architecture, Design & Planning",
           faculty == "Faculty of Engineering" ~ "Engineering",
           faculty == "Faculty of Arts and Social Sciences" ~ "Arts & Social Sciences",
           faculty == "Faculty of Law" ~ "Law",
           faculty == "Faculty of Science" ~ "Science",
           faculty == "Business School" ~ "Business")) %>%
  filter(`Research Output` %in% c("Thesis/Dissertation","Report","Presentation",
                                  "Poster","Journal Article","Conference","Book or Book Chapter"))

# Transform data
ResearchOutput_Faculty_Tool_Transformed <- ResearchOutput_Faculty_Tool %>% 
  make_long(`Research Output`,Faculty,Technology)

TotalCount = nrow(ResearchOutput_Faculty_Tool)

ResearchOutput_Faculty_Tool_Percent<- ResearchOutput_Faculty_Tool %>% 
  make_long(`Research Output`,Faculty,Technology) %>%
  group_by(node) %>% 
  summarise(Count = n()) %>% 
  mutate(Frequency = formattable::percent(Count/ TotalCount))

ResearchOutput_Faculty_Tool_Transformed_Percent <-
  merge(
    ResearchOutput_Faculty_Tool_Transformed,
    ResearchOutput_Faculty_Tool_Percent,
    by.x = 'node',
    by.y = 'node',
    all.x = TRUE
  )

library(randomcoloR)
n <- 30
palette <- distinctColorPalette(n)

# Create sankey plot: Output - Faculty - Technology
ResearchOutput_Faculty_Tool_Transformed_Percent %>% 
  ggplot(aes(x = x, 
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = factor(node),
             # label = node
             label = paste0(node," n=", Count, ' (', Frequency, ')')
             ))+
  geom_sankey(flow.alpha = 0.5,
              node.color = "gray40",
              show.legend = FALSE)+
  geom_sankey_label(size = 2,
                    color = "black",
                    fill= "white",
                    hjust = 0,
                    # width = 0.2
                    )+
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  scale_fill_manual(values = palette)+
  labs(title = "Sankey diagram of top 7 research outputs")+
  labs(subtitle = "leveled by Faculty and Technology")+
  # labs(caption = "Intersect Australia")+
  labs(fill = 'Nodes')
