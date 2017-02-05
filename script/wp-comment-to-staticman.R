
#
# Import Wordpress comments to Staticman yml file
# 

# Step 0:
#   
# Download Wordpress comments from PHP MyAdmin
# Save as JSON file
# 
# SELECT `comment_ID`
#   , `comment_author`
#   , `comment_author_email`
#   , `comment_author_url`
#   , `comment_date`
#   , `comment_content`
#   , `comment_parent`
#   , `post_title`
# FROM `wp_comments` 
# JOIN `wp_posts`
# ON `comment_post_ID` = `ID`
# WHERE `comment_approved` = 1;
# 
# Note: file cleansing might be needed
# e.g. remove comment line, replace quote character
# 

# Step 1:

library(jsonlite)
library(openssl)

comments <- paste(readLines("wp-comments.json"), collapse = "\\r\\n")
comments <- fromJSON(comments)

id <- paste("wordpress", comments$comment_ID, sep = "-")
post_id <-
  paste("\"/", gsub("^-|-$", "", gsub(
    "[^a-z0-9]+", "-", tolower(comments$post_title)
  )), "\"", sep = "")
name <-
  ifelse(comments$comment_author == "nurandi",
         "Nur Andi Setiabudi",
         comments$comment_author)
email <- openssl::md5(as.character(comments$comment_author_email))
url <-
  ifelse(
    comments$comment_author == "nurandi",
    "https://nurandi.net",
    ifelse(
      comments$comment_author_url %in% c("http://-", ""),
      "#",
      comments$comment_author_url
    )
  )
date <-
  strftime(as.character(comments$comment_date), format = "%Y-%m-%dT%H:%M:%SZ")
message <- gsub("\r\n", "\\\\r\\\\n", comments$comment_content)
message <- gsub("<pre><code[^>]*>", "```r\\\\r\\\\n", message)
message <- gsub("</code></pre>", "```", message)
message <- gsub("&lt;", "<", message)
message <- paste("\"", message, "\"", sep = "")


for (i in  1:nrow(comments)) {
  comment <- c(
    id = id[i],
    post_id = post_id[i],
    name = name[i],
    email = email[i],
    url = url[i],
    date = date[i],
    message = message[i]
  )
  
  comment_root_folder <- "commentsXX"
  comment_post_folder <- gsub("^..|.$", "", post_id[i])
  comment_yml_file <-
    paste("comment-", as.numeric(strptime(date[i], 
                          format = "%Y-%m-%dT%H:%M:%SZ")), ".yml", sep = "")
  
  if (!file.exists(comment_root_folder)) {
    dir.create(comment_root_folder)
  }
  
  if (!file.exists(file.path(comment_root_folder, comment_post_folder))) {
    dir.create(file.path(comment_root_folder, comment_post_folder))
  }
  
  sapply(names(comment), function(x) {
    cat(
      paste(x, comment[x], sep = ": "),
      file = file.path(
        comment_root_folder,
        comment_post_folder,
        comment_yml_file
      ),
      sep = "\n",
      append = T
    )
  })
}

