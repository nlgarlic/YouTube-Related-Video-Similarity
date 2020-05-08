#installations
install.packages("tuber")
install.packages("tidyverse")
install.packages("stringr")
install.packages("tidytext")
install.packages("textstem")
install.packages("gtools")
install.packages("ggrepel")

#call libraries
library(tuber)
library(tidyverse)
library(plyr)
library(readr)
library(tidytext)
library(textstem)
library(gtools)
library(ggrepel)
sessionInfo()

#if need to clear out workspace
remove(list = ls())

#for troubleshooting
help(package = tuber)
help(package = tidyverse)

#saving the client ID and client secret for subsequent use by the tuber package
#at prompt choose "1: Yes"
#get stats on YouTube video with VideoID
app_id <- "INSERT YOURS HERE"
app_secret <- "INSERT YOURS HERE"
yt_oauth(app_id, app_secret)

### GATHER DATA

## Initial Video
#gather video details, including tags, title, and description (TTD) for chosen video and write to file
#to enter your own initial video, the video ID is the alphanumeric code at the end of the youtube link 
#(e.g., https://www.youtube.com/watch?v=MQcN5DtMT-0)
i_video_details <- get_video_details(video_id = "MQcN5DtMT-0")
i_video_details <- data.frame(i_video_details)
i_video_details <- select_at(i_video_details, vars(starts_with("items.id"), 
                                              starts_with("items.snippet.channelTitle"),
                                              starts_with("items.snippet.categoryId"),
                                              starts_with("items.snippet.title"), 
                                              starts_with("items.snippet.description"), 
                                              starts_with("items.snippet.tags")))
colnames(i_video_details)<-c("Video ID","Channel Title", "Video Category ID", "Video Title", "Video Description", 
                           paste(rep("Video Tag",(ncol(i_video_details)-5)), c(1:(ncol(i_video_details)-5)),sep=""))

write.csv(i_video_details, file = "initial_video_details.csv")

#review title, description, and tags (TDT) in video_details and create custom keyword list (dictionary)
dictionary <- c("unicef", "child", "pover", "foreverychild", "fightunfair")

## Related Videos
#search for UNICEF YouTube video and download related videos
#includes most video metadata but does not include tags
#make sure to update video ID here if using your own initial video
relatedvids <- get_related_videos(video_id = "MQcN5DtMT-0")
write.csv(relatedvids, file = "relatedvids.csv")

#get video stats for related videos
#for loop to iteratively collect video stats for multiple videos
#creates list to hold results (all_related_videos)
#runs get_video_details function for each video and appends results
#prints after each video is run for confirmation purposes
#may get warning message "No statistics available"; this indicates that loop reached end of list of videos
complete_related_videos <- data.frame()
for (i in relatedvids$rel_video_id) {
  morerelatedvids <- get_video_details(video_id = i)
  more_related_videos <- data.frame(morerelatedvids)
  print(i)
  all_related_videos <- select_at(more_related_videos, vars(starts_with("items.id"), 
                                                            starts_with("items.snippet.channelTitle"),
                                                            starts_with("items.snippet.categoryId"),
                                                            starts_with("items.snippet.title"), 
                                                            starts_with("items.snippet.description"), 
                                                            starts_with("items.snippet.tags")))
  final_related_videos <- data.frame(all_related_videos)
  complete_related_videos <- smartbind(complete_related_videos, final_related_videos, fill=NA, sep=':', verbose=FALSE)
}

#remove unneeded variables from environment
rm(all_related_videos, final_related_videos, more_related_videos, morerelatedvids)

#add column of related video ID order number (Rank)
complete_related_videos <- complete_related_videos %>% mutate(Rank = 1:nrow(complete_related_videos)) %>% select(Rank, everything())

#gather video view data for related videos
complete_view_counts <- data.frame()
for (i in relatedvids$rel_video_id) {
  moreviewcounts <- get_stats(video_id = i)
  more_view_counts <- data.frame(moreviewcounts)
  print(i)
  all_view_counts <- select_at(more_view_counts, vars(starts_with("id"), starts_with("viewCount")))
  final_view_counts <- data.frame(all_view_counts)
  complete_view_counts <- smartbind(complete_view_counts, final_view_counts, fill=NA, sep=':', verbose=FALSE)
}

#rename column (variable) names and merge in view counts
colnames(complete_related_videos)[2] <-"id"
complete_related_videos <- merge(complete_view_counts, complete_related_videos, by = "id")
colnames(complete_related_videos)<-c("Video ID", "View Count", "Rank","Channel Title", "Video Category ID", "Video Title", "Video Description",  
                                     paste(rep("Video Tag",(ncol(complete_related_videos)-7)),
                                           c(1:(ncol(complete_related_videos)-7)),sep=""))

### WRANGLE DATA

## Initial Video
#convert category ID column to matching text 
#first import list of categories then merge and ultimately delete category ID number
#then sort by rank and reorder columns
youtube_categories <- read_csv("youtube categories as of 12 11 19.csv")
colnames(youtube_categories) <- c("Video Category ID", "Video Category Text")
i_video_details <- merge(youtube_categories, i_video_details, by = "Video Category ID")
i_video_details <- i_video_details[-1]

#add "0" rank for initial video
i_video_details <- add_column(i_video_details, Rank = 0, .before = 1)

#combine TTD into one column
#first create character vector of all column names
#then exclude columns not being united
#lastly, combine (unite) all but those and reorder columns
i_video_text_cols <- colnames(i_video_details)
i_video_text_cols <- i_video_text_cols[-1:-4]
i_video_details <- i_video_details %>% unite("Video.Title.Description.Tags", all_of(i_video_text_cols), sep = " ")
i_video_details <- select(i_video_details, 1,3,2,4,5)
colnames(i_video_details) <- c("Rank", "Video.ID", "Video.Category.Text", "Channel.Title", "Video.Title.Description.Tags")

## Related Videos
#convert category ID column to matching text and add to related videos data frame
complete_related_videos_all <- merge(youtube_categories, complete_related_videos, by = "Video Category ID")
complete_related_videos_all <- complete_related_videos_all[-1]
complete_related_videos_all <- complete_related_videos_all[order(complete_related_videos_all$Rank),] 
complete_related_videos_all <- select(complete_related_videos_all, 4,1,3,2, everything())
rm(complete_related_videos)

#combine all title, description, and tag columns (variables) into one
column_names <- colnames(complete_related_videos_all)
column_names <- column_names[-1:-5]
all_related_vids_combined <- unite(complete_related_videos_all, "Video.Title.Description.Tags", all_of(column_names), sep = " ")
colnames(all_related_vids_combined) <- c("Rank", "Video.Category.Text", "View.Count", "Video.ID", "Channel.Title", "Video.Title.Description.Tags")

#output to csv - change name to fit data
write.csv(all_related_vids_combined, "all_related_vids.csv")


###CLEAN DATA

#keep separate column of uncleaned data for later use
i_video_details$uncleanTDT <- i_video_details$Video.Title.Description.Tags
all_related_vids_combined$uncleanTDT <- all_related_vids_combined$Video.Title.Description.Tags

## Initial Video
#clean initial video details first, removing digits, extra spaces, punctuation, and making lowercase
i_video_details$Video.Title.Description.Tags <- tolower(i_video_details$Video.Title.Description.Tags)
i_video_details$Video.Title.Description.Tags <- str_remove_all(i_video_details$Video.Title.Description.Tags, "[:digit:]")
i_video_details$Video.Title.Description.Tags <- str_remove_all(i_video_details$Video.Title.Description.Tags, "[:punct:]")
i_video_details$Video.Title.Description.Tags <- str_remove_all(i_video_details$Video.Title.Description.Tags, "\\|")
i_video_details$Video.Title.Description.Tags <- str_squish(i_video_details$Video.Title.Description.Tags)

## Related Videos
#clean related video details, removing digits, extra spaces, punctuation, NA values, and making lowercase
#remove all NA values and extra spaces from Video.Title.Description.Tags and reorder columns
all_related_vids_combined$`Video.Title.Description.Tags` <- str_remove_all(all_related_vids_combined$`Video.Title.Description.Tags`, "NA")
all_related_vids_combined$`Video.Title.Description.Tags` <- str_remove_all(all_related_vids_combined$`Video.Title.Description.Tags`, "[:digit:]")
all_related_vids_combined$`Video.Title.Description.Tags` <- tolower(all_related_vids_combined$`Video.Title.Description.Tags`)
all_related_vids_combined$`Video.Title.Description.Tags` <- str_remove_all(all_related_vids_combined$`Video.Title.Description.Tags`, "[:punct:]")
all_related_vids_combined$`Video.Title.Description.Tags` <- str_remove_all(all_related_vids_combined$`Video.Title.Description.Tags`, "\\|")
all_related_vids_combined$`Video.Title.Description.Tags` <- str_squish(all_related_vids_combined$`Video.Title.Description.Tags`)

#lemmatize
all_related_vids_combined$Video.Title.Description.Tags %>%
  lemmatize_strings() %>%
  head()
  
#export final video data
write.csv(all_related_vids_combined, "all_related_vids_data.csv")

### APPLY DICTIONARY

#recall dictionary terms
dictionary

#search for dictionary terms
#search is done separately because coding scheme may combine two words

#organization matches - change to fit organization
unicef_matches <- str_detect(all_related_vids_combined$`Video.Title.Description.Tags`, "unicef")

#issue matches - change to fit issues
child_matches <- str_detect(all_related_vids_combined$`Video.Title.Description.Tags`, "child|boy|girl|teen|kid|yearold")
poverty_matches <- str_detect(all_related_vids_combined$`Video.Title.Description.Tags`, "pover|homeless|fortunate")

#hashtag matches - change to fit hashtags or delete if none
foreverychild_matches <- str_detect(all_related_vids_combined$`Video.Title.Description.Tags`, "foreverychild")
fightunfair_matches <- str_detect(all_related_vids_combined$`Video.Title.Description.Tags`, "fightunfair")

#convert each to integers from TRUE/FALSE - update to fit your coding scheme
unicef_matches <- as.integer(as.logical(unicef_matches))
child_matches <- as.integer(as.logical(child_matches))
poverty_matches <- as.integer(as.logical(poverty_matches))
foreverychild_matches <- as.integer(as.logical(foreverychild_matches))
fightunfair_matches <- as.integer(as.logical(fightunfair_matches))

#create dataframe of all matches - update to fit your coding scheme
all_matches <- data.frame("Rank" = all_related_vids_combined$Rank,
                          "Channel.Title" = all_related_vids_combined$Channel.Title,
                          "Video.Category.Text" = all_related_vids_combined$Video.Category.Text,
                          "Video.ID" = all_related_vids_combined$Video.ID,
                          "View.Count" = all_related_vids_combined$View.Count,
                          "Org.Matches" = unicef_matches,
                          "Issue.Matches" = child_matches + poverty_matches,
                          "Hashtag.Matches" = foreverychild_matches + fightunfair_matches,
                          "Video.Title.Description.Tags" = all_related_vids_combined$Video.Title.Description.Tags,
                          "Uncleaned.Text" = all_related_vids_combined$uncleanTDT)
View(all_matches)

#the following recoding will need to be customized for your coding scheme

#spot check and then recode for dictionary weighting and add total column
#recode child + poverty as 1 point (0 for only one)
all_matches$Issue.Matches[all_matches$Issue.Matches == 1] <- 0
all_matches$Issue.Matches[all_matches$Issue.Matches == 2] <- 1

#recode hashtags as 1 point
all_matches$Hashtag.Matches[all_matches$Hashtag.Matches == 2] <- 1

#add column for recoding view count into 3 levels - unpopular, popular (1 mil plus), very popular (5 mil plus)
#first convert View Count from factor to numeric
all_matches$View.Count <- as.numeric(levels(all_matches$View.Count))[all_matches$View.Count]
all_matches$Popularity <- all_matches$View.Count
all_matches$Popularity <- cut(all_matches$Popularity, breaks=c(0, 1000000, 5000000, Inf), 
                              labels=c("Unpopular", "Popular", "Very Popular"))

#add columns for recoding rank into 3 levels (Top: 1-5, Middle: 6-15, Bottom: 16+) 
#numerical
all_matches$Rank.Factor <- all_matches$Rank
all_matches$Rank.Factor[all_matches$Rank.Factor <= 5] <- 1
all_matches$Rank.Factor[all_matches$Rank.Factor >= 6 & all_matches$Rank.Factor<= 15] <- 2
all_matches$Rank.Factor[all_matches$Rank.Factor >= 16] <- 3
#categorical
all_matches$Rank.Categorical <- all_matches$Rank
all_matches$Rank.Categorical <- cut(all_matches$Rank.Categorical, breaks=c(0, 5, 15, Inf), 
                                    labels=c("Top", "Middle", "Bottom"))

#add column for recoding channel title into binary categorical variable
all_matches$Channel.Matches <- recode(all_matches$Channel.Title, "UNICEF" = "UNICEF", .default = "Other")

#create total column
all_matches$Total.Matches <- all_matches$Org.Matches + all_matches$Issue.Matches + all_matches$Hashtag.Matches

#create similarity column
all_matches$Similarity <- all_matches$Total.Matches
all_matches$Similarity <- cut(all_matches$Similarity, breaks=c(0, 1, 2, 3), 
                                    labels=c("Somewhat Similar", "Moderately Similar", "Very Similar"))
all_matches$Similarity <- as.character(all_matches$Similarity) %>% replace_na("Not Similar")

#reorder columns
all_matches <- select(all_matches, 1,2,3,4,5,9,10, everything())

#export all matches data frame
write.csv(all_matches, "all matches.csv")

### VISUALIZATION
###https://r4ds.had.co.nz/data-visualisation.html
###https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
###https://ggrepel.slowkow.com/articles/examples.html

#Similarity by Category
all_matches %>%
  mutate(Similarity = fct_reorder(Similarity, Total.Matches)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Similarity, fill = Video.Category.Text), position = "fill") +
  xlab("") +
  ylab("") + 
  labs(fill = "Video Category") +
  theme(axis.text.y = element_blank(),
  axis.ticks = element_blank())

#Similarity by Channel
all_matches %>%
  mutate(Similarity = fct_reorder(Similarity, Total.Matches)) %>%
  ggplot(mapping = aes(x = Channel.Title, y = Similarity, color = Channel.Matches)) + 
  geom_text_repel(aes(label = Channel.Title), size = 4, min.segment.length = Inf,
                  seed = 42, box.padding = 0.5) +
  coord_flip() +
  xlab("") +
  ylab("") +
  guides(fill = "none") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_blank())

#Number of Recommended Videos by Rank and Popularity
all_matches %>%
  mutate(Rank.Categorical = fct_reorder(Rank.Categorical, desc(Rank.Factor))) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Rank.Categorical, fill = Popularity), position = "dodge") +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab("# Recommended Videos") +
  xlab("Recommended Video Rank")




