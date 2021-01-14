## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("repello")

## ---- warning=FALSE-----------------------------------------------------------
suppressPackageStartupMessages(require(repello))
#Note: you may want to also install the following packages for well-formatted report tables
suppressPackageStartupMessages(require(knitr))
suppressPackageStartupMessages(require(kableExtra))

## ---- eval=FALSE--------------------------------------------------------------
#  set_key_token(key_file="user_key.txt", token_file="user_token.txt")
#  #Alternatively, you may leave the arguments blank and manually enter the key and token when prompted
#  set_key_token()

## ---- eval=FALSE--------------------------------------------------------------
#  activity <- cards_info(get_board_id("Repello - R Reports from Trello"))
#  head(activity) %>% kable(escape=F, align="cl") %>% trimws %>% kable_styling(c("striped","bordered"))

## ---- echo=FALSE--------------------------------------------------------------
activity <- readRDS("activity.rds")
head(activity) %>% kable(escape=F, align="cl") %>% trimws %>% kable_styling(c("striped","bordered"))

## ---- eval=FALSE--------------------------------------------------------------
#  trello_object <- all_checklists("Repello - R Reports from Trello", save=FALSE)
#  #save can be set to 'TRUE' if you want to save the current Trello object for
#  #later comparison
#  trello_object[[4]]

## ---- echo=FALSE--------------------------------------------------------------
trello_object <- readRDS("trello_object.rds")
trello_object[[4]]

## -----------------------------------------------------------------------------
trello_object[[2]]

## ---- eval=FALSE--------------------------------------------------------------
#  report <- trello_updates("Repello - R Reports from Trello", prior="old_object.rds", save=FALSE)
#  #Note: If 'prior' is not specified, the function will automatically find the most recent saved
#  #Trello object to compare to the current rendition.  The function can also accept a 'recent'
#  #argument if you want to compare two lists from different time points.
#  
#  report[[4]]

## ---- echo=FALSE--------------------------------------------------------------
report <- readRDS("report.rds")
report[[4]]

## -----------------------------------------------------------------------------
report[[5]]

## -----------------------------------------------------------------------------
report[[2]]

## ---- eval=FALSE--------------------------------------------------------------
#  set_token("user_token.txt")
#  report <- trello_updates("Repello - R Reports from Trello", prior="old_object.rds", save=FALSE)
#  

## ---- results='asis'----------------------------------------------------------
new_sticker <- "<div class='sticker'>NEW</div>"
new_headersticker <- "<div class='header-sticker'>NEW</div>"
checkmark <- "<span class='checkmark'><div class='checkmark_stem'></div><div class='checkmark_kick'></div></span>"

for (i in 1:length(report)){
  if (report[[i]]$contains_list=="List Available" & ("New Item" %in% (report[[i]]$list_diff)$history | "Newly Completed" %in% (report[[i]]$list_diff)$status)){
    if (report[[i]]$card_status=="New Card"){
      cat("<h2>", new_headersticker, report[[i]]$name, "</h2>", sep = " ")
    } else {
      cat("## ", report[[i]]$name, "\n\n")
    }
    temp <- report[[i]]$list_diff %>% filter(history=="New Item" | status=="Newly Completed")
    for (j in 1:nrow(temp)){
      if (temp$history[j]=="New Item"){
        temp$item[j] <- paste0(new_sticker, temp$item[j])
      }
    }
    for (k in 1:nrow(temp)){
      if (temp$status[k]=="Newly Completed"){
        temp$complete[k] <- checkmark
      } else {
        temp$complete[k] <- " "
      }
    }
    temp <- temp %>% select(complete, item)
    colnames(temp) <- c("", "Item")
    temp %>% kable(escape=F, align="cl") %>% trimws %>% kable_styling(c("striped","bordered")) %>% column_spec(1, "30px") %>% cat
    cat("  \n")
  }
}

