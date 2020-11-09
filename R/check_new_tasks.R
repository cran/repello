#' Check New Tasks
#'
#' This function allows you to compare checklists for new items and completions.
#' @param list1 A checklist for a Trello card.
#' @param list2 A more recent checklist for the same card.
#' @keywords repello
#' @export

check_new_tasks <- function(list1, list2){
  changes <- dplyr::setdiff(list2, list1)
  item_history <- c()
  task_completeness <- c()
  if (list2$item[1] == "No checklist for this project"){
    changes <- data.frame(item=character(), history=character(), status=character())
    changes
  } else {
    for (i in 1:nrow(list2)){
      if (list2$item[i] %in% list1$item & list2$item[i] %in% changes$item){
        item_history[i] <- "Existing Item"
        task_completeness[i] <- "Newly Completed"
      } else if (list2$item[i] %in% list1$item & !(list2$item[i] %in% changes$item)){
        item_history[i] <- "Existing Item"
        if (list2$status[i]=="complete"){
          task_completeness[i]="Previously Completed"
        } else {
          task_completeness[i]="Incomplete"
        }
      } else {
        item_history[i] <- "New Item"
        if (list2$status[i]=="complete"){
          task_completeness[i] <- "Newly Completed"
        } else {
          task_completeness[i] <- "Incomplete"
        }
      }
    }
    changes <- data.frame(item=list2$item, history=item_history, status=task_completeness)
    changes
  }
}
