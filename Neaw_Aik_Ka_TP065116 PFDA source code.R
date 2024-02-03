#Neaw Aik Ka
#TP065116


#====Package Installed====
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor") #Data Cleaning
install.packages("RColorBrewer") #Data Visualization (Colour Platte)
install.packages("hrbrthemes")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("reshape2")
install.packages("ggExtra")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("fmsb")
install.packages("GGally")
install.packages("remotes")
remotes::install_github("davidsjoberg/ggsankey")
install.packages("circlize")

#====Load Library====
library(dplyr)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(hrbrthemes)
#Extra feature 1: using the color palette to make the report more visualizable
library(tidyverse)
#Extra features 2: this library helps in dealing with massive data or drawing more complicated graphs, so that the element and information included in the graphs will be more and helps researchers to understand the dataset well
library(ggpubr)
library(reshape2)#Extra features 3: this library is using in reshape the data table that is extracted from the dataset to the data table that is needed in drawing different kinds of graphs
library(ggExtra)#extra features 4: this library helps us to add the histogram, density graphs and etc graphs into scatter plot, adding more information to one graph
library(viridis)
library(fmsb)#Extra features 5: this library is used to draw the multi-polygon chart, especially the radar chart
library(GGally)#Extra features 6: this library is widely used in determining the correlation index between index, it will be used in drawing heatmap
library(ggsankey)#Extra features 7: this library is used to draw the Sankey diagram that visualize the flow of the people that have different choice
library(circlize)#Extra features 8: this library is used to draw the chord diagram which shows the people that has different situation might end with the result that the dataset points to

#====import data======
df <- read.csv("~/Placement_Data_Full_Class.csv", header=T)

#====Exploratory data analysis EDA====
dim(df)
View(df)
arrange(df, sl_no)
str(df)
summary(df)
dfnumeric <- select_if(df, is.numeric)
dfnumeric
dfchar <- df %>% select_if(is.character)
dfchar
summary(dfnumeric)

factordf <- lapply(dfchar, factor)
lapply(factordf, levels)

colorP <- c("#2c699a", "#edb21d", "#b5f8df", "#0db39e", "#048ba8", "#2c699a", "#493e7a", "#e64c61")

ggplot(df, aes(y=salary)) + geom_boxplot(colour = colorP[1]) + 
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Salary box plot")
ggsave("~/refer/1.png")
ggplot(df, aes(y=sl_no)) + geom_boxplot(colour=colorP[2])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Sl_no box plot")
ggsave("~/refer/2.png")
ggplot(df, aes(y=mba_p)) + geom_boxplot(colour=colorP[4])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "MBA_p box plot")
ggsave("~/refer/3.png")
ggplot(df, aes(y=age)) + geom_boxplot(colour=colorP[5])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Age box plot")
ggsave("~/refer/4.png")
df_box <- select(df, degree_p, etest_p, hsc_p, mba_p,ssc_p) %>% 
  pivot_longer(everything(), names_to = "column")
ggplot(df_box, aes(x=column, y=value, color=column)) + geom_boxplot()+xlab("")+ylab("Value")+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Test result box plot")
ggsave("~/refer/5.png")
df_box <- select(df,Medu,Fedu) %>% pivot_longer(everything(), names_to = "column")
ggplot(df_box, aes(x=column,y=value, fill=column)) + geom_boxplot()+xlab("")+ylab("Value")+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Father mother education box plot")
ggsave("~/refer/6.png")
ggplot(df, aes(x=sl_no))+geom_density(color=colorP[8])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "sl_no line chart")
ggsave("~/refer/7.png")
ggplot(df, aes(x=etest_p))+geom_density(color=colorP[1])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "etest_p line chart")
ggsave("~/refer/8.png")
ggplot(df, aes(x=mba_p))+geom_density(color=colorP[2])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "MBA_p line chart")
ggsave("~/refer/9.png")
ggplot(df, aes(x=age))+geom_density(color=colorP[8])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Age line chart")
ggsave("~/refer/10.png")
ggplot(df, aes(x=Medu))+geom_density(color=colorP[1])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Mother education line chart")
ggsave("~/refer/11.png")
ggplot(df, aes(x=Fedu))+geom_density(color=colorP[2])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Father education line chart")
ggsave("~/refer/12.png")
ggplot(df, aes(x=ssc_p))+geom_density(color=colorP[8])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Ssc_p line chart")
ggsave("~/refer/13.png")
ggplot(df, aes(x=hsc_p))+geom_density(color=colorP[1])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Hsc_p line chart")
ggsave("~/refer/14.png")
ggplot(df, aes(x=degree_p))+geom_density(color=colorP[2])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Degree_p line chart")
ggplot(df, aes(x=salary))+geom_density(color=colorP[8])+
  scale_color_ipsum() + scale_fill_ipsum() +theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))+
  labs(title = "Salary line chart")
ggsave("~/refer/15.png")

#===heatmap====
corrMap <- round(cor(dfnumeric), 3)
corrMap[lower.tri(corrMap)] <- NA
meltedMap <- melt(corrMap)
ggplot(meltedMap, aes(x = Var1, y =Var2, fill=value)) + geom_tile() + 
  geom_text(aes(Var1, Var2, label=value), color="white", size=4) + 
  scale_fill_distiller(palette="Reds")
ggsave("~/Graph/heatmap.png")


#transformation====
df <- mutate(df, age_group = ifelse(age <= 20, "18-20", ifelse(age > 20, "20-23", "23-")))
df <- df %>% mutate(Fedu_level = ifelse(Fedu==0, "none", 
                                        ifelse(Fedu==1,"primary",ifelse(Fedu==2, "9th Grade", 
                                                                        ifelse(Fedu==3,"secondary education", "higher education")))) )
df <- df %>% mutate(Medu_level = ifelse(Medu==0, "none", ifelse(Medu==1,"primary",
                                                                ifelse(Medu==2, "9th Grade", ifelse(Medu==3,"secondary education", "higher education")))) )
df <- df %>% mutate(age_group = ifelse(age <= 20, "18-20", "21-23"))
df <- df %>% mutate(ssc_level = ifelse(ssc_p >= 85, "HD", 
                                       ifelse(ssc_p>=75, "DN", 
                                              ifelse(ssc_p >= 65,"CR", ifelse(ssc_p>=50, "PS",ifelse(ssc_p>=47,"MF","FL"))))))
df <- df %>% mutate(hsc_level = ifelse(hsc_p >= 85, "HD", 
                                       ifelse(hsc_p>=75, "DN", ifelse(hsc_p >= 65,"CR", ifelse(hsc_p>=50, "PS",
                                                                                               ifelse(hsc_p>=47,"MF","FL"))))))
df <- df %>% mutate(degree_level = ifelse(degree_p >= 85, "HD", ifelse(degree_p>=75, "DN", 
                                                                       ifelse(degree_p >= 65,"CR", ifelse(degree_p>=50, "PS",ifelse(degree_p>=47,"MF","FL"))))))
df <- df %>% mutate(etest_level = ifelse(etest_p >= 85, "HD", 
                                         ifelse(etest_p>=75, "DN", ifelse(etest_p >= 65,"CR", 
                                                                          ifelse(etest_p>=50, "PS",ifelse(etest_p>=47,"MF","FL"))))))
df <- df %>% mutate(mba_level = ifelse(mba_p >= 85, "HD", ifelse(mba_p>=75, "DN", 
                                                                 ifelse(mba_p >= 65,"CR", ifelse(mba_p>=50, "PS",ifelse(mba_p>=47,"MF","FL"))))))
quantile(df$salary, probs = c(0.4,0.8,1), na.rm = T)
df <- df %>% mutate(salary_class = ifelse(is.na(salary), NA, ifelse(salary>=4e+05, "T20", 
                                                                    ifelse(salary>=3e+05,"M40", "B40"))))
df$address <- ifelse(df$address == "U", "Urban", "Rural")
dfrural <- df[df$address=="Rural",]
dfurban <- df[df$address=="Urban",]



#*****++++++=====

#====Question 1:Are educational resources different in urban and rural areas?====

#====Analysis 1.1:	Are Father’s education in different areas between individuals diverse?====
dfedu <- df %>% select(address, Fedu_level) %>% 
  group_by(address) %>% count(Fedu_level) %>%
  mutate(percent=round(n/sum(n), 2)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
dfedu
ggplot(dfedu, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=Fedu_level)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(dfedu$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ facet_grid(~address) +#Extra feature 9 : facet_grid() is the function helps programmers to divide the graphs in to multiple column and row according to the categories of specific attributes
  theme_ipsum_ps(grid="XY", axis="xy") +scale_color_ipsum() + scale_fill_ipsum() +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white"))+
  labs(fill = "Education Level", title = "Father Education in different areas") + xlim(2,4)
ggsave("~/Graph/1p1.png", width=8)

#====Analysis 1.2:	Are Mother’s education diverse between different areas?====
dfedu <- df %>% select(address, Medu_level) %>% 
  group_by(address) %>% count(Medu_level) %>%
  mutate(percent=round(n/sum(n), 2)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
dfedu
ggplot(dfedu, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=Medu_level)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(dfedu$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ facet_grid(~address)+
  labs(fill = "Education Level", title = "Mother Education in different areas") + xlim(2,4) +
  theme_ipsum_ps(grid="XY", axis="xy") +scale_color_ipsum() + scale_fill_ipsum() +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p2.png", width=8)

#====Analysis 1.3:	Do people in different areas prefer secondary school boards?====
dfboard <- df %>% select(address, ssc_b) %>% count(address, ssc_b) %>% group_by(address)
dfboard
ggplot(dfboard, aes(x = address, y = n, fill = ssc_b)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Secondary school board chosen in different areas") +scale_color_ipsum() + 
  scale_fill_ipsum() + theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p3p1.png")

ggplot(dfboard, aes(x = ssc_b, y = n, group = address, color = address, fill = address)) +
  geom_polygon(alpha = 0.2) + geom_point(shape = 24, size = 4) +
  coord_polar(start = -pi/6) + ylab("Population count") + xlab("Secondary school board") +
  ggtitle("Secondary school board chosen in different areas") + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p3p2.png")

#====Analysis 1.4:	Do people in different areas have different choices in choosing their high school board?====
dfboard <- df %>% select(address, hsc_b) %>% count(address, hsc_b) %>% group_by(address)
dfboard
ggplot(dfboard, aes(x = address, y = n, fill = hsc_b)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_color_ipsum() + scale_fill_ipsum() +  theme_minimal()+
  ggtitle("High School Board chosen in different areas") + ylab("count") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p4p1.png")


ggplot(dfboard, aes(x = hsc_b, y = n, group = address, color = address, fill = address)) +
  geom_polygon(alpha = 0.2) + geom_point(shape = 24, size = 4) +#Extra feature 10 : geom_polygon is in ggplot2 that is a function help us in drawing the multi-polygon chart
  
  coord_polar(start = -pi/6) + ylab("Population count") + xlab("Secondary school board") +
  ggtitle("High School Board chosen in different areas") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p4p2.png")


#====Analysis 1.5:	Will the school boards affect the number of students joining in the extra-curricular activities?====
dfboard <- df %>% select(ssc_b, hsc_b, activities) %>% 
  mutate(ssc_b = ifelse(ssc_b == "Central", "Secondary Central", 
                        ifelse(ssc_b == "Private", "Secondary Private", "Secondary State"))) %>% 
  mutate(hsc_b = ifelse(hsc_b == "Central", "High Central", 
                        ifelse(hsc_b == "Private", "High Private", "High State"))) %>% 
  pivot_longer(cols = c(ssc_b, hsc_b), values_to = "board") %>% count(board,activities)
dfboard
ggplot(dfboard, aes(board, n, fill=activities)) + 
  geom_bar(stat = "identity", position = "stack")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra-curricular activities in different school board")+ ylab("count") +xlab("board of school") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p5.png")

#====Analysis 1.6:	Will the school boards affect the number of students joining extra-paid classes?====
dfboard <- df %>% select(ssc_b, hsc_b, paid) %>% 
  mutate(ssc_b = ifelse(ssc_b == "Central", "Secondary Central", 
                        ifelse(ssc_b == "Private", "Secondary Private", "Secondary State"))) %>% 
  mutate(hsc_b = ifelse(hsc_b == "Central", "High Central", 
                        ifelse(hsc_b == "Private", "High Private", "High State"))) %>% 
  pivot_longer(cols = c(ssc_b, hsc_b), values_to = "board") %>% count(board,paid)
dfboard
ggplot(dfboard, aes(board, n, fill=paid)) + geom_bar(stat = "identity", position = "stack")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra paid classes in different school board")+ ylab("count") +xlab("board of school") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p6.png")

#====Analysis 1.7:	Is there any difference for students from different areas in terms of willingness in joining extra-paid classes?====
dfpaid <- df %>% select(address, paid) %>% count(address, paid)
dfpaid
ggplot(dfpaid, aes(x=paid, y=n, fill=address)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra paid classes in different areas")+ ylab("count") +xlab("board of school") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p7.png")

#====Analysis 1.8:	Is there any difference for students from different areas in joining extra-curricular activities?====
dfactivity <- df %>% select(address, activities) %>% count(address, activities)
dfactivity
ggplot(dfactivity, aes(x=activities, y=n, fill=address)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra-curricular Activities in different areas")+ ylab("count") +xlab("board of school") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p8.png")

#====Analysis 1.9:	Does the internet access for students different in urban and rural areas?=====
dfinternet <- df %>% select(address, internet) %>% count(address, internet)
dfinternet
ggplot(dfinternet, aes(x=internet, y=n, fill=address)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Internet access in different areas")+ ylab("count") +xlab("board of school") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p9p1.png")

#Summary 1.9====
dfsummary <- df %>% select(address, paid, activities, internet) %>% 
  count(address, paid, activities, internet)
dfsummary
ggplot(dfsummary, aes(x = activities, y = n, fill = internet)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(paid~address) + scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra-curricular Activities, internet access and 
Extra-paid classes in different areas")+
  ylab("count") +xlab("Extra-curricular activities") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p9Sum1.png")

ggplot(dfsummary, aes(x = interaction(address, paid, internet), y = n, fill = activities)) +
  geom_col() +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B"), labels = c("no", "yes")) +
  labs(x = "Address, Paid, and Internet", y = "Count") +
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Extra-curricular Activities, internet access and 
Extra-paid classes in different areas")+ ylab("count")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p9Sum2.png")

#====Analysis 1.10:	Is there any tendency of students from different areas and school boards in choosing specialization?====
dfspec <- df %>% select(address, hsc_b, hsc_s) %>% count(address, hsc_b, hsc_s) %>% 
  make_long(address, hsc_b, hsc_s)
dfspec
ggplot(dfspec, aes(x = x, next_x = next_x, node = node,next_node = next_node, 
                   fill = factor(node))) + geom_sankey() + theme_sankey(base_size = 16) +
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Specialisation chosen in different areas and school board")+ xlab("Choice")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p10.png")

#====Analysis 1.11:	Do the students from different areas choose different degree types?====
dfdeg <- df %>% select(address, degree_t) %>% count(address, degree_t) %>% arrange(address, n)
dfdeg
ggplot(dfdeg, aes(address, n, fill = degree_t)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Degree types chosen in different areas")+ ylab("count")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/1p11.png")

#====Analysis 1.12:	Do the students from different areas choose different specializations?====
dfspec <- df %>% select(address, specialisation) %>% 
  group_by(address, specialisation) %>% summarize(count = n())
dfspec %>% count(address, specialisation)
chordDiagram(dfspec)+
  title("Specialisation chosen in different areas", cex = 0.8, font = 2, padj = T, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/1p12.png")
dev.off()


#====Questions 2:family support and education background and resources====
#====Analysis 2.1:	Does family support directly affect the rural area student’s secondary school results?====
dfsup <- dfrural %>% select(famsup, ssc_p) %>%
  pivot_longer(cols = c(ssc_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
dfsup
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to secondary school result in rural area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p1p1.png")

#====Analysis 2.2:	Does family support directly affect the urban area student’s secondary school results?====
dfsup <- dfurban %>% select(famsup, ssc_p) %>%
  pivot_longer(cols = c(ssc_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
dfsup
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to secondary school result in urban area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p1p2.png")

#====Analysis 2.3:	Does family support directly affect the rural area student’s high school results?====
dfsup <- dfrural %>% select(famsup, hsc_p) %>%
  pivot_longer(cols = c(hsc_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
dfsup
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to high school result in rural area") +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p2p1.png")

#====Analysis 2.4:	Does family support directly affect the urban area student’s high school results?====
dfsup <- dfurban %>% select(famsup, hsc_p) %>%
  pivot_longer(cols = c(hsc_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to high school result in urban area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p2p2.png")

#====Analysis 2.5:	Does family support directly affect the rural area student’s degree results?====
dfsup <- dfrural %>% select(famsup, degree_p) %>%
  pivot_longer(cols = c(degree_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to degree result in rural area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p3p1.png")

#====Analysis 2.6:	Does family support directly affect the urban area student’s degree school results?====
dfsup <- dfurban %>% select(famsup, degree_p) %>%
  pivot_longer(cols = c(degree_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to degree result in urban area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p3p2.png")

#====Analysis 2.7:	Does family support directly affect the rural area student’s MBA results?====
dfsup <- dfrural %>% select(famsup, mba_p) %>%
  pivot_longer(cols = c(mba_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + 
  geom_point() + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Effect of family support to MBA result in rural areas")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p4p1.png")

#====Analysis 2.8:	Does family support directly affect the urban area student’s MBA results?====
dfsup <- dfurban %>% select(famsup, mba_p) %>%
  pivot_longer(cols = c(mba_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to MBA result in urban area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p4p2.png")

#====Analysis 2.9:	Does family support have gained effect on employability test results for students in rural areas?====
dfsup <- dfrural %>% select(famsup, etest_p) %>%
  pivot_longer(cols = c(etest_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to employability test result in rural area")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p5p1.png")

#====Analysis 2.10:	Does family support have gained effect on employability test results for students in urban areas?====
dfsup <- dfurban %>% select(famsup, etest_p) %>%
  pivot_longer(cols = c(etest_p), names_to = "level", values_to = "percentage") %>% 
  group_by(famsup)
ggplot(dfsup, aes(x = famsup, y=percentage, colour=famsup)) + geom_point() + 
  ggtitle("Effect of family support to employability test result in urban area")+
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p5p2.png")

#====Summary 2.10 The effect of family support to academic and employability test result in different areas====
dfsup <- df %>% select(address, famsup, ssc_p, hsc_p, degree_p, mba_p, etest_p) %>%
  pivot_longer(cols = c(ssc_p, hsc_p, degree_p, mba_p), names_to = "level", 
               values_to = "percentage") %>% group_by(address, famsup)
dfsup
ggplot(dfsup, aes(x = etest_p, y=percentage, colour=level)) + geom_point() + 
  facet_grid(address~famsup) +
  ggtitle("Effect of family support to academic and 
 employability test result in different areas")+
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p5Sum1.png")

#====Analysis 2.11:	Do all the results affect by family support when only top students are observed?====
dfsummary <- df %>% filter(ssc_level == "HD" & hsc_level == "HD" & 
                             degree_level == "HD" & mba_level == "HD" & etest_level == "HD") %>%
  select(famsup, ssc_p, hsc_p, degree_p, mba_p, etest_p) %>% arrange(.)
ggparcoord(dfsummary, columns = c(2,3,4,5,6), groupColumn = 1, showPoints = T)+
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of family support to academic and 
 employability test result in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p5Sum2.png")

#====Analysis 2.12:	Do family support determines the difference for number of students in accessing to internet, 
#attending extra-paid classes and extra-curricular activities?====
dfsup <- df %>% select(famsup, internet, activities, paid) %>% 
  pivot_longer(cols = c(internet, activities, paid), names_to = "condition", 
               values_to = "binary") %>% count(famsup, condition, binary)
dfsup
ggplot(dfsup, aes(binary, n,group=condition, color=condition)) + geom_point() + 
  geom_line()+ facet_wrap(~famsup)+
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of family support to internet access
 extra-curricular activities and extra-paid classes")+ylab("count")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/2p6p1.png")

#====Analysis 2.13:	Does the father’s occupation play a critical role in the children’s secondary school results in rural areas?====
dfjob <- dfrural %>% select(address, Fjob, ssc_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=ssc_p, color=Fjob)) + geom_point() + 
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to secondary school result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p7.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.14:	Does the father’s occupation play a critical role in the children’s secondary school results in urban areas?====
dfjob <- dfurban %>% select(address, Fjob, ssc_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=ssc_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to secondary school result
in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p8.png", plot=g, width=8,height=8,dpi=300)

#====Analysis 2.15:	Does the father’s occupation play a critical role in the children’s high school results in rural areas?====
dfjob <- dfrural %>% select(address, Fjob, hsc_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=hsc_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to high school result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p9.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.16:	Does the father’s occupation play a critical role in the children’s high school results in urban areas?====
dfjob <- dfurban %>% select(address, Fjob, hsc_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=hsc_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to high school result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p10.png", plot=g,width = 8,height = 8,dpi = 300)

#====Analysis 2.17:	Does the father’s occupation play a critical role in the children’s degree results in rural areas?====
dfjob <- dfrural %>% select(address, Fjob, degree_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=degree_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to degree result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))

g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p11.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.18:	Does the father’s occupation play a critical role in the children’s degree results in urban areas?====
dfjob <- dfurban %>% select(address, Fjob, degree_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=degree_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to degree result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))

g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p12.png", plot=g, width=8,height = 8,dpi=300)

#====Analysis 2.19:	Does the father’s occupation play a critical role in the children’s employability test results in rural areas?====
dfjob <- dfrural %>% select(address, Fjob, etest_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=etest_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to employability test result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p13.png", plot=g)

#====Analysis 2.20:	Does the father’s occupation play a critical role in the children’s employability test results in urban areas?====
dfjob <- dfurban %>% select(address, Fjob, etest_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=etest_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to employability test result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <-ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p14.png", plot = g)

#====Analysis 2.21:	Does the father’s occupation play a critical role in the children’s MBA results in rural areas?====
dfjob <- dfrural %>% select(address, Fjob, mba_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=mba_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to MBA result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p15.png", plot = g)

#====Analysis 2.22:	Does the father’s occupation play a critical role in the children’s MBA results in urban areas?====
dfjob <- dfurban %>% select(address, Fjob, mba_p) %>% group_by(address, Fjob)
dfjob
g <- ggplot(dfjob, aes(x=Fjob, y=mba_p, color=Fjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Father job to MBA result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p16.png", plot=g)

#====Analysis 2.23:	Does the mother’s occupation play a critical role in the children’s secondary school results in rural areas?====
dMjob <- dfrural %>% select(address, Mjob, ssc_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=ssc_p, color=Mjob)) + geom_point() + 
  theme_minimal() +
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to secondary school result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p17.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.24:	Does the mother’s occupation play a critical role in the children’s secondary school results in urban areas?====
dMjob <- dfurban %>% select(address, Mjob, ssc_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=ssc_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to secondary school result
in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p18.png", plot=g, width=8,height=8,dpi=300)

#====Analysis 2.25:	Does the mother’s occupation play a critical role in the children’s high school results in rural areas?====
dMjob <- dfrural %>% select(address, Mjob, hsc_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=hsc_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to high school result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p19.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.26:	Does the mother’s occupation play a critical role in the children’s high school results in urban areas?====
dMjob <- dfurban %>% select(address, Mjob, hsc_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=hsc_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to high school result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p20.png", plot=g,width = 8,height = 8,dpi = 300)

#====Analysis 2.27:	Does the mother’s occupation play a critical role in the children’s MBA results in rural areas?====
dMjob <- dfrural %>% select(address, Mjob, degree_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=degree_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of Mother job to degree result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))

g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p21.png", plot=g,width=8,height=8,dpi=300)

#====Analysis 2.28:	Does the mother’s occupation play a critical role in the children’s degree results in urban areas?====
dMjob <- dfurban %>% select(address, Mjob, degree_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=degree_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to degree result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))

g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p22.png", plot=g, width=8,height = 8,dpi=300)

#====Analysis 2.29:	Does the mother’s occupation play a critical role in the children’s employability test results in rural areas?====
dMjob <- dfrural %>% select(address, Mjob, etest_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=etest_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to employability test result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p23.png", plot=g)

#====Analysis 2.30:	Does the mother’s occupation play a critical role in the children’s employability test results in urban areas?====
dMjob <- dfurban %>% select(address, Mjob, etest_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=etest_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to employability test result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <-ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p24.png", plot = g)

#====Analysis 2.31:	Does the mother’s occupation play a critical role in the children’s MBA results in rural areas?====
dMjob <- dfrural %>% select(address, Mjob, mba_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=mba_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to MBA result
 in rural area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p25.png", plot = g)

#====Analysis 2.32:	Does the mother’s occupation play a critical role in the children’s MBA results in urban areas?====
dMjob <- dfurban %>% select(address, Mjob, mba_p) %>% group_by(address, Mjob)
dMjob
g <- ggplot(dMjob, aes(x=Mjob, y=mba_p, color=Mjob)) + geom_point() + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Effect of mother job to MBA result
 in urban area")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
g <- ggMarginal(g, type="density", margins = 'y', color="purple", size=4)
g
ggsave("~/Graph/2p26.png", plot=g)




#Question 3:area gender discrimination=====
#====Analysis 3.1:	Do these students’ parents in rural areas get equal chances of having an education during their education?====
dfedu <- dfrural %>% select(Fedu_level, Medu_level) %>% 
  pivot_longer(cols = c(Medu_level, Fedu_level), names_to = "education", values_to = "education_level") %>% 
  group_by(education) %>% count(education_level) %>%
  mutate(percent=round(n/sum(n), 2)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
dfedu
ggplot(dfedu, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=education_level)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(dfedu$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ 
  facet_grid(~education) +
  labs(fill = "Education Level", title = "Education of Parents in rural areas") + xlim(2,4) +
  scale_color_ipsum() + scale_fill_ipsum()+ theme_void()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p1.png")


#====Analysis 3.2:	Do these students’ parents in urban areas get equal opportunities in getting an education during their studying?====
dfedu <- dfurban %>% select(Fedu_level, Medu_level) %>% 
  pivot_longer(cols = c(Medu_level, Fedu_level), names_to = "education", 
               values_to = "education_level") %>% 
  group_by(education) %>% count(education_level) %>%
  mutate(percent=round(n/sum(n), 2)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
dfedu
ggplot(dfedu, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=education_level)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(dfedu$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ 
  facet_grid(~education) + theme_void() + 
  labs(fill = "Education Level", title = "Education of Parents in urban areas") + xlim(2,4) +
  scale_color_ipsum() + scale_fill_ipsum() + theme_void()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p2p1.png")

#====Summary 3.1 and 3.2: Education equality for students’ parents in the past time====
dfedu <- df %>% select(address, Medu, Fedu) %>% pivot_longer(cols = c(Medu, Fedu)
                                                             , names_to="parents", values_to = "education")  %>% 
  group_by(address, parents, education) %>% summarize(count = n())
ggplot(dfedu, aes(x=education, y=count, fill=parents)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis(discrete = T, name="") + facet_grid(~address) + 
  coord_flip() + ggtitle("Parents education in different areas")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p2p2.png")

dfedu <- df %>% select(address, Fedu) %>% pivot_longer(cols = -address, 
                                                       names_to="Father_education", values_to = "value") %>% 
  group_by(address, Father_education, value) %>% count(value)
ggplot(dfedu, aes(x=value, y=n, fill=address)) + geom_bar(stat = "identity", position = "dodge") + 
  xlab("Father education") + ylab("count") + 
  ggtitle("Father education in different area") + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p2p3.png")

dfedu <- df %>% select(address, Medu) %>% 
  pivot_longer(cols = -address, names_to="Mother_education", values_to = "value") %>% 
  group_by(address, Mother_education, value) %>% count(value)
ggplot(dfedu, aes(x=value, y=n, fill=address)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Mother education in different area") + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p2p4.png")

#====Analysis 3.3:	Is there any stereotype for parents in choosing occupations in rural areas?====
df_gender <- dfrural %>% select(Fjob, Mjob) %>% 
  pivot_longer(cols = c(Fjob,Mjob), names_to = "parents", values_to = "job")  %>% group_by(parents) %>%
  count(job) %>% mutate(percent=round(n/sum(n), 5)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
df_gender
ggplot(df_gender, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=job)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(df_gender$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ 
  facet_grid(~parents) + theme_void() + 
  labs(fill = "Job", title = "Job of Parents in rural area") + xlim(2,4) +
  scale_color_ipsum() + scale_fill_ipsum() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p3.png")

#====Analysis 3.4:	Is there any stereotype for parents in choosing occupations in urban areas?====
df_gender <- dfurban %>% select(Fjob, Mjob) %>% 
  pivot_longer(cols = c(Fjob,Mjob), names_to = "parents", values_to = "job")  %>% group_by(parents) %>%
  count(job) %>% mutate(percent=round(n/sum(n), 5)) %>% mutate(cumulative=cumsum(percent)) %>%
  mutate(ymin = cumulative - percent)
df_gender
ggplot(df_gender, aes(ymax = cumulative, ymin = ymin, xmax = 4, xmin=3, fill=job)) + 
  geom_rect(color = "white") + 
  geom_label(x = 3.5, aes(y = (cumulative + ymin)/2), label=paste0(df_gender$percent*100, "%"), 
             size = 3) +
  coord_polar(theta = "y")+ 
  facet_grid(~parents) + theme_void() + 
  labs(fill = "Job", title = "Job of Parents in urban areas") + xlim(2,4) +
  scale_color_ipsum() + scale_fill_ipsum() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p4.png")

#====Analysis 3.5:	Is the student’s family have a different attitude in supporting different gender children?=====
df_gender <- df %>% select(gender, address, famsup) %>% group_by(gender, address, famsup) %>% count(famsup)
df_gender
ggplot(df_gender, aes(gender, n, fill=gender)) + 
  geom_segment( aes(x=gender, xend=gender, y=0, yend=n), color="grey") +
  geom_point( color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(famsup~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() +
  ggtitle("Family support for different gender in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p5.png")

#====Analysis 3.6:	Are different gender students in different areas having a difference in joining the extra-paid classes?====
df_gender <- df %>% select(gender, address, paid) %>% group_by(gender, address, paid) %>% count(paid)
df_gender
ggplot(df_gender, aes(gender, n, fill=gender)) + 
  geom_segment( aes(x=gender, xend=gender, y=0, yend=n), color="grey") +
  geom_point(color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(paid~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() +
  ggtitle("Extra-paid classes for different gender in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p6.png")

#====Analysis 3.7:	Are different gender students in different areas having a difference in accessing the Internet?====
df_gender <- df %>% select(gender, address, internet) %>% group_by(gender, address, internet) %>% count(internet)
df_gender
ggplot(df_gender, aes(gender, n, fill=gender)) + 
  geom_segment( aes(x=gender, xend=gender, y=0, yend=n), color="grey") +
  geom_point(color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(internet~address) + ylab("count") +
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Internet access for different gender in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p7.png")

#====Analysis 3.8:	Are different gender students in different areas having a difference in joining the extra-curricular activities?====
df_gender <- df %>% select(gender, address, activities) %>% group_by(gender, address, activities) %>% count(activities)
df_gender
ggplot(df_gender, aes(activities, n, fill=gender)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + ylab("count") + facet_grid(~address)+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Extra-curricular Activities for different gender 
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p8.png")

#====Analysis 3.9:	Are different gender students in different areas having discrimination or gap in secondary school tests?====
df_gender <- df %>% select(address, gender, ssc_level) %>% count(address, gender, ssc_level)
df_gender
ggplot(df_gender, aes(ssc_level, y=n)) +
  geom_segment( aes(x=ssc_level, xend=ssc_level, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address)+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Secondary school result for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p9.png")

#====Analysis 3.10:	Are different gender students in different areas having a stereotype in joining the secondary school board?====
df_gender <- df %>% select(address, gender, ssc_b) %>% count(address, gender, ssc_b)
df_gender
ggplot(df_gender, aes(ssc_b, y=n)) +
  geom_segment( aes(x=ssc_b, xend=ssc_b, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Secondary school board for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p10.png")

#====Analysis 3.11:	Are different gender students in different areas having discrimination or gap in high school tests?====
df_gender <- df %>% select(address, gender, hsc_level) %>% count(address, gender, hsc_level)
df_gender
ggplot(df_gender, aes(hsc_level, y=n)) +
  geom_segment( aes(x=hsc_level, xend=hsc_level, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("High school result for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p11.png")

#====Analysis 3.12:	Are different gender students in different areas having a stereotype in joining the high school board?====
df_gender <- df %>% select(address, gender, hsc_b) %>% count(address, gender, hsc_b)
df_gender
ggplot(df_gender, aes(hsc_b, y=n)) +
  geom_segment( aes(x=hsc_b, xend=hsc_b, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("High school board for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p12.png")

#====Analysis 3.13:	Are different gender students in different areas having a stereotype in choosing high school specialization?====
df_gender <- df %>% select(address, gender, hsc_s) %>% count(address, gender, hsc_s)
df_gender
ggplot(df_gender, aes(hsc_s, y=n)) +
  geom_segment( aes(x=hsc_s, xend=hsc_s, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("High school specialization for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p13.png")

#====Analysis 3.14:	Are different gender students in different areas having discrimination or gap in high school tests?====
df_gender <- df %>% select(address, gender, degree_level) %>% count(address, gender, degree_level)
df_gender
ggplot(df_gender, aes(degree_level, y=n)) +
  geom_segment( aes(x=degree_level, xend=degree_level, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Degree result for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p14.png")

#====Analysis 3.15:	Are different gender students in different areas having a stereotype in choosing degree types?====
df_gender <- df %>% select(address, gender, degree_t) %>% count(address, gender, degree_t)
df_gender
ggplot(df_gender, aes(degree_t, y=n)) +
  geom_segment( aes(x=degree_t, xend=degree_t, y=0, yend=n), color="skyblue") +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Degree types for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p15.png")

#====Analysis 3.16:	Are different gender students in different areas having discrimination or gap in employability tests?====
df_gender <- df %>% select(address, gender, etest_level) %>% count(address, gender, etest_level)
df_gender
ggplot(df_gender, aes(etest_level, y=n)) +
  geom_segment( aes(x=etest_level, xend=etest_level, y=0, yend=n), color="skyblue") +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Employability test result for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p16.png")

#====Analysis 3.17:	Are different gender students in different areas having discrimination or gap in MBA tests?====
df_gender <- df %>% select(address, gender, mba_level) %>% count(address, gender, mba_level)
df_gender
ggplot(df_gender, aes(mba_level, y=n)) +
  geom_segment( aes(x=mba_level, xend=mba_level, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("MBA result for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p17.png")

#====Analysis 3.18:	Is there any discrimination that will affect the status of the student's placement in the dataset?====
df_gender <- df %>% select(address, gender, status) %>% count(address, gender, status)
df_gender
ggplot(df_gender, aes(status, y=n)) +
  geom_segment( aes(x=status, xend=status, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(gender~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Status for different gender
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p18.png")

#====Analysis 3.19:	Is there any difference between males and females from different areas in choosing their specialization?====
df_gender <- df %>% select(address, gender, specialisation) %>% 
  count(address, gender, specialisation)
df_gender
to_add <- matrix(NA, empty_bar, ncol(df_gender))
colnames(to_add) <- colnames(df_gender)
df_gender <- rbind(df_gender, to_add)
df_gender$id <- seq(1, nrow(df_gender))
df_gender
df_gender <- df_gender %>% mutate(angle = 90 - 360 * (id-0.5) /nrow(df_gender)) %>% 
  mutate(hjust = ifelse(angle < -90, 1, 0)) %>% mutate(angle = ifelse(angle == 90, angle+180, angle))
df_gender

ggplot(df_gender, aes(x=as.factor(id), y=n, fill=address, colour=gender)) +
  geom_bar(stat="identity") +
  ylim(-1900,2500) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) +  theme_void()+
  geom_text(data=df_gender, aes(x=id, y=n+10, label=paste(address, gender, specialisation, sep = " "), 
  hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= df_gender$angle, inherit.aes = FALSE )+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"),
  plot.background = element_rect(fill = "white", color = "white")) + ggtitle("Specialisation of different gender and areas")
ggsave("~/Graph/3p19.png")

#====Analysis 3.20:	Do the males and females in different areas have equal chances to gain their work experience? ====
df_exp <- df %>% select(workex, gender, address) %>% 
  pivot_longer(cols = workex, names_to = "workex", values_to = "value") %>% 
  group_by(gender, address) %>% count(value)
df_exp
ggplot(df_exp, aes(x = gender, y = n, fill = address)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~value, ncol = 1) + ylab("Count") + 
  ggtitle("Gender work experience in different areas") + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p20.png")

#====Analysis 3.21:	Is the job market providing a different salary for gender?====
df_sal <- df %>% 
  select(address, gender, salary) %>% 
  filter(!is.na(salary)) %>% 
  group_by(address, gender, salary) %>% 
  count(salary)

ggplot(df_sal, aes(x=address, y = n, fill=address)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(salary ~ gender) + 
  coord_flip() + ylim(0,500) + xlab("Area") + 
  ylab("count") + ggtitle("Salary of gender in different areas")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/3p21.png")


#Question 4: area age affect current circumstances====
#====Analysis 4.1:	Does the presence of family support differ according to individuals’ age?=====
df_age_group <- df %>% select(age_group, address, famsup) %>% 
  group_by(age_group, address, famsup) %>% count(famsup)
df_age_group
ggplot(df_age_group, aes(age_group, n, fill=age_group)) + 
  geom_segment( aes(x=age_group, xend=age_group, y=0, yend=n), color="grey") +
  geom_point( color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(famsup~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Family support provided for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p1.png")

#====Analysis 4.2:	Does age affect students' whether to involve themselves in extra-paid classes?====
df_age_group <- df %>% select(age_group, address, paid) %>% 
  group_by(age_group, address, paid) %>% count(paid)
df_age_group
ggplot(df_age_group, aes(age_group, n, fill=age_group)) + 
  geom_segment( aes(x=age_group, xend=age_group, y=0, yend=n), color="grey") +
  geom_point(color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(paid~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Extra-paid classes for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p2.png")

#====Analysis 4.3:	Is there any difference for students in different age groups in having internet access?====
df_age_group <- df %>% select(age_group, address, internet) %>% 
  group_by(age_group, address, internet) %>% count(internet)
df_age_group
ggplot(df_age_group, aes(age_group, n, fill=age_group)) + 
  geom_segment( aes(x=age_group, xend=age_group, y=0, yend=n), color="grey") +
  geom_point(color="orange", size=4, position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + facet_grid(internet~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Internet access for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p3.png")

#====Analysis 4.4:	Do students of different age groups from different areas have different opinions on joining extra-curricular activities?====
df_age_group <- df %>% select(age_group, address, activities) %>% 
  group_by(age_group, address, activities) %>% count(activities)
df_age_group
ggplot(df_age_group, aes(activities, n, fill=age_group)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) + ylab("count") + facet_grid(~address)+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Extra-curricular activities for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p4.png")


#====Analysis 4.5:	Do different age groups of students choose different secondary school boards in the past?====
df_age_group <- df %>% select(address, age_group, ssc_b) %>% count(address, age_group, ssc_b)
df_age_group
ggplot(df_age_group, aes(ssc_b, y=n)) +
  geom_segment( aes(x=ssc_b, xend=ssc_b, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(age_group~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + 
  ggtitle("Secondary school board for different age group
 in different areas")+theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p5.png")

#====Analysis 4.6:	Do different age groups of students choose different high school boards in the past?====
df_age_group <- df %>% select(address, age_group, hsc_b) %>% count(address, age_group, hsc_b)
df_age_group
ggplot(df_age_group, aes(hsc_b, y=n)) +
  geom_segment( aes(x=hsc_b, xend=hsc_b, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(age_group~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("High school board for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p6.png")

#====Analysis 4.7:	Do different age groups of students choose different high school specializations in the past?====
df_age_group <- df %>% select(address, age_group, hsc_s) %>% count(address, age_group, hsc_s)
df_age_group
ggplot(df_age_group, aes(hsc_s, y=n)) +
  geom_segment( aes(x=hsc_s, xend=hsc_s, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(age_group~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("High school specialisation for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p7.png")

#====Analysis 4.8:	Do different age groups of students choose different degree types in the past?====
df_age_group <- df %>% select(address, age_group, degree_t) %>% count(address, age_group, degree_t)
df_age_group
ggplot(df_age_group, aes(degree_t, y=n)) +
  geom_segment( aes(x=degree_t, xend=degree_t, y=0, yend=n), color="skyblue") +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(age_group~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  ggtitle("Degree types for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p8.png")

#====Analysis 4.9:	Do different age groups of students have their own preferences in choosing different specializations?====
df_age_group <- df %>% select(address, age_group, status) %>% count(address, age_group, status)
df_age_group
ggplot(df_age_group, aes(status, y=n)) +
  geom_segment( aes(x=status, xend=status, y=0, yend=n), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + facet_grid(age_group~address) + ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum()+
  ggtitle("Status for different age group
 in different areas")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p9.png")

#====Analysis 4.10:	Do different age groups of students from different areas has their own preferences in choosing the specialization?====
df_age_group <- df %>% select(address, age_group, specialisation) %>% 
  count(address, age_group, specialisation)
df_age_group
to_add <- matrix(NA, empty_bar, ncol(df_age_group))
colnames(to_add) <- colnames(df_age_group)
df_age_group <- rbind(df_age_group, to_add)
df_age_group$id <- seq(1, nrow(df_age_group))
df_age_group
df_age_group <- df_age_group %>% mutate(angle = 90 - 360 * (id-0.5) /nrow(df_age_group)) %>% 
  mutate(hjust = ifelse(angle < -90, 1, 0)) %>% 
  mutate(angle = ifelse(angle == 90, angle+180, angle))
df_age_group

ggplot(df_age_group, aes(x=as.factor(id), y=n, fill=address, colour=age_group)) +
  geom_bar(stat="identity") +
  ylim(-1900,2500) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) +  theme_void()+
  geom_text(data=df_age_group, aes(x=id, y=n+10, 
                                   label=paste(address, age_group, specialisation, sep = " "), hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= df_age_group$angle, 
            inherit.aes = FALSE )+
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white")) + 
  ggtitle("Specialisation of different age group in different areas")
ggsave("~/Graph/4p10.png")

#====Analysis 4.11:	Do different age groups of students have equality in gaining work experience?====
df_exp <- df %>% select(workex, age_group, address) %>% 
  pivot_longer(cols = workex, names_to = "workex", values_to = "value") %>% 
  group_by(age_group, address) %>% count(value)
df_exp
ggplot(df_exp, aes(x = age_group, y = n, fill = address)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~value, ncol = 1) + ylab("count") + 
  ggtitle("Work experience for different age group in different areas") + 
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p11.png")

#====Analysis 4.12:	Does age affect the salary of individuals in different areas?====
df_sal <- df %>% 
  select(address, age_group, salary) %>% 
  filter(!is.na(salary)) %>% 
  group_by(address, age_group, salary) %>% 
  count(salary)
df_sal
ggplot(df_sal, aes(x=address, y = n, fill=address)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(salary ~ age_group) + 
  coord_flip() + ylim(0,500) + xlab("Area") + #Extra feature 11: coord_filp() is used to flip the analysis result graphs to from horizontal to vertical because in some cases, vertical graphs has higher ability in interpreting graphs
  ylab("count") + ggtitle("Salary of different age group in different areas")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/4p12.png")


#Question 5: salary_class effect====
#====Analysis 5.1:	Does the individuals’ gender affect the salary provided by their employers?====
df_sal <- df %>% filter(salary != 0)%>% select(gender, salary_class) %>% 
  group_by(gender, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of gender to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p1p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=gender)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of gender to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p1p2.png")

#====Analysis 5.2:	Does the individual's age affect the salary provided by their employers?====
df_sal <- df %>% filter(salary != 0)%>% select(age_group, salary_class) %>% 
  group_by(age_group, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of age to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p2p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=age_group)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of age to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p2p2.png")

#====Analysis 5.3:	Does the living address of employees affect the salary they get?====
df_sal <- df %>% filter(salary != 0)%>% select(address, salary_class) %>% 
  group_by(address, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Salary in different areas", cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p3p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=address)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Salary in different areas")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p3p2.png")

#====Analysis 5.4:	Does the parent’s job affect the salary of their children?====
df_sal <- df %>% filter(salary != 0)%>% select(salary_class, Fjob, Mjob) %>% 
  pivot_longer(cols = c(Fjob, Mjob), names_to = "parents", values_to = "parents_job") %>%
  group_by(salary_class, parents, parents_job) %>% count(salary_class)
df_sal
ggplot(df_sal, aes(salary_class, n, fill=parents_job)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~parents) + ylab("count")+
  ggtitle("Effect of parents job to salary")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p4p1.png")

#====Analysis 5.5:	Does family support affect the salary of individuals?====
df_sal <- df %>% filter(salary != 0)%>% select(famsup, salary_class) %>% 
  group_by(famsup, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of family support to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p5p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=famsup)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of family support to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p5p2.png")

#====Analysis 5.6:	Do joining extra-paid classes affect the salary of individuals?====
df_sal <- df %>% filter(salary != 0)%>% select(paid, salary_class) %>% 
  group_by(paid, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of extra-paid classes to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p6p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=paid)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of extra-paid classes to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p6p2.png")

#====Analysis 5.7:	Do joining extra-curricular activities help in increasing the salary of one?====
df_sal <- df %>% filter(salary != 0)%>% select(activities, salary_class) %>% 
  group_by(activities, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of extra-curricular-activities to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p7p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=activities)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of extra-curricular activities to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p7p2.png")

#====Analysis 5.8:	Does internet access have a significant effect on students’ salaries during their jobs?====
df_sal <- df %>% filter(salary != 0)%>% select(internet, salary_class) %>% 
  group_by(internet, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of internet access to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p8p1.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=internet)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of internet access to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p8p2.png")

#====Analysis 5.9:	Does the test result during studying affect the salary paid by those students’ bosses?====
df_sal <- df %>% filter(salary != 0)%>% 
  select(ssc_level, hsc_level, degree_level, etest_level, mba_level, salary_class) %>% 
  pivot_longer(cols = c(ssc_level, hsc_level, degree_level, etest_level, mba_level), 
               names_to = "test", values_to = "level") %>%
  group_by(level, salary_class) %>% count(salary_class)
df_sal
chordDiagram(df_sal)
title("Effect of test result to salary", 
      cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/5p9p1.png")
dev.off()
df_sal <- df %>% filter(salary != 0)%>% 
  select(ssc_level, hsc_level, degree_level, etest_level, mba_level, salary_class) %>% 
  pivot_longer(cols = c(ssc_level, hsc_level, degree_level, etest_level, mba_level), 
               names_to = "test", values_to = "level") %>%
  group_by(test, level, salary_class) %>% count(salary_class)
df_sal
ggplot(df_sal, aes(level, n, fill=level)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~salary_class, ncol=1) + 
  ggtitle("Effect of test result to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + facet_grid(~test)+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p9p2.png")

#====Analysis 5.10:	Does the school board’s choice affect the student’s salary when they step into the workplace?====
dfboard <- df %>% filter(salary != 0) %>% select(ssc_b, hsc_b, salary_class) %>% 
  make_long(ssc_b, hsc_b, salary_class)
dfboard
ggplot(dfboard, aes(x = x, next_x = next_x, node = node,next_node = next_node, 
                    fill = factor(node))) + 
  geom_sankey() + theme_sankey(base_size = 16)+
  ggtitle("Effect of school board to salary")+
  scale_color_ipsum() + scale_fill_ipsum() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p10p1.png")

#====Analysis 5.11:	Does the choice of specialization make the salary get by individuals differ?====
dfboard <- df %>% filter(salary != 0) %>% 
  select(hsc_s, degree_t, specialisation, salary_class) %>% 
  make_long(hsc_s, degree_t, specialisation, salary_class)
dfboard
g <- ggplot(dfboard, aes(x = x, next_x = next_x, node = node,next_node = next_node, fill = factor(node))) + 
  geom_sankey() + theme_sankey(base_size = 16)+
  ggtitle("Effect of specialization to salary")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
g
ggsave("~/Graph/5p11p1.png", plot = g, width = 8, height=8, dpi=300)

#====Analysis 5.12:	Does one’s work experience affect his salary?====
df_sal <- df %>% filter(salary != 0)%>% select(workex, salary_class) %>% 
  group_by(workex, salary_class) %>% count(salary_class)
df_sal %>% count(workex, salary_class)
chordDiagram(df_sal)
title("Effect of work experience to salary", cex = 0.8, font = 2, padj = 0, x = -2, y = 0)
#resolution low
dev.copy(png, "~/Graph/1p12.png")
dev.off()

ggplot(df_sal, aes(salary_class, n, fill=workex)) + 
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Effect of work experience to salary")+ylab("count")+
  scale_color_ipsum() + scale_fill_ipsum() + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"),
        plot.background = element_rect(fill = "white", color="white"))
ggsave("~/Graph/5p12p2.png")

