#' Compare Checklists
#'
#' This function compares checklists for all items in a list of lists.
#' @param old_list A  prior list of Trello activities.
#' @param new_list A more recent list of Trello activities.
#' @keywords repello
#' @export

compare_checklists <- function(old_list, new_list){
  old_cards <- c()
  new_cards <- c()
  for (i in 1:length(old_list)){
    old_cards[i] <- old_list[[i]]$ID
  }

  for (j in 1:length(new_list)){
    new_cards[j] <- new_list[[j]]$ID
  }

  new_tasks <- list()
  for (k in 1:length(old_cards)){
    temp <- old_cards[k]
    Match <- match(temp, new_cards)
    if (!is.na(Match)){
      list1 <- data.frame(old_list[[k]][["item"]], old_list[[k]][["status"]])
      colnames(list1) <- c("item", "status")
      list2 <- data.frame(new_list[[Match]][["item"]], new_list[[Match]][["status"]])
      colnames(list2) <- c("item", "status")
      contents <- check_new_tasks(list1, list2)
      if (dim(contents)[1]!=0){
        new_tasks[[old_cards[k]]] <- list(name=old_list[[k]]$name, ID=old_list[[k]]$ID, card_status="Existing Card", contains_list="List Available", list_diff=contents)
      } else {
        new_tasks[[old_cards[k]]] <- list(name=old_list[[k]]$name, ID=old_list[[k]]$ID, card_status="Existing Card", contains_list="No List Available", list_diff=contents)
      }
    } else {
      new_tasks[[old_cards[k]]] <- list(name=old_list[[k]]$name, ID=old_list[[k]]$ID, card_status="Card deleted")
    }
  }
  for (m in 1:length(new_cards)){
    temp <- new_cards[m]
    Match <- match(temp, old_cards)
    if (is.na(Match)){
      if (new_list[[m]]$item[1] !="No checklist for this project"){
        new_tasks[[new_cards[m]]] <- list(name=new_list[[m]]$name, ID=new_list[[m]]$ID, card_status="New Card", contains_list="List Available", list_diff=data.frame(item=new_list[[m]]$item, history=rep("New Item", length(new_list[[m]]$item)), status=ifelse(new_list[[m]]$status=="complete", "Newly Completed", "Incomplete")))
      } else {
        new_tasks[[new_cards[m]]] <- list(name=new_list[[m]]$name, ID=new_list[[m]]$ID, card_status="New Card", contains_list="No List Available", list_diff=data.frame(item=character(), history=character(), status=character()))
      }
    }
  }
  new_tasks
}
