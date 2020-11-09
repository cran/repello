#' Trello Updates
#'
#' This function allows you to check for updates to all cards on a Trello board, comparing to a prior board setting.
#' @param board_name The name of the board you want to browse.
#' @param prior The name of the prior Trello list you want to compare to.  Defaults to most recently saved Trello object prior to current save.
#' @param recent The name of the Trello board to compare to a prior Trello object.  By default will use current object grab as most recent object.
#' @param save Set to TRUE to save a copy of the current Trello object.
#' @param user_token The user token for an individual's Trello account.
#' @importFrom dplyr %>%
#' @keywords repello
#' @export

trello_updates <- function(board_name, prior=FALSE, recent=FALSE, save=FALSE, user_token=NULL){
  fvn.tgsify <- function(data, pattern){
    grep(pattern, data, ignore.case = TRUE) %>% 
      (function(x){data[x]})
  }
  if (prior==FALSE){
    recent_date <- gsub("checklist_([0-9_\\-]+)\\.csv","\\1",list.files() %>% fvn.tgsify("checklist_")) %>%
      sort(decreasing = TRUE) %>%
      '['(1)
    prior <- recent_date
  }
  if (recent!=FALSE & save==FALSE){
    new <- readRDS(paste(recent))

  } else if (recent!=FALSE & save==TRUE) {
    if (is.null(user_token) & !exists("trello_api_token_08192020", envir = globals)){
      return(warning("Need to input a user token or set the token using 'set_token()'"))
    }
    if (is.null(user_token) & exists("trello_api_token_08192020", envir = globals)){
      user_token <- globals$trello_api_token_08192020
    }
    new <- all_checklists(board_name, save=save)
    new <- readRDS(paste(recent))
  } else {
    if (is.null(user_token) & !exists("trello_api_token_08192020", envir = globals)){
      return(warning("Need to input a user token or set the token using 'set_token()'"))
    }
    if (is.null(user_token) & exists("trello_api_token_08192020", envir = globals)){
      user_token <- globals$trello_api_token_08192020
    }
    new <- all_checklists(board_name, save=save)
  }
  old <- readRDS(paste(prior))
  comp <- compare_checklists(old, new)
  comp
}
