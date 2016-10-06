

survey_output <- function(format, qid, qtext, answer_choices, location){
  
  if(!file.exists(location)){
    file.create(location)
  }
  
  advanced_format <- "[[AdvancedFormat]]"
  # qid <- c("q1")
  # qtext <- c("This is a multiple choice question")
  # answer_choices <- c("test1", "test2", "test3")
  
  if(format == 'MC'){
  q_format <- c("[[Question:MC]]")
    
  } else if(format == "MA"){
    q_format <- c("[[Question:MultipleAnswer]]")  
    
  } else if(format == "MT"){
    q_format <- "[[Question:Matrix]]"
  } else{
    break("Format not supported")
  }
  
  my_survey_list <- list(advanced_format, q_format, qid, qtext, answer_choices)
  
  text <- c(" ", unlist(my_survey_list[[1]]), 
            unlist(my_survey_list[[2]]), 
            paste0("[[ID:", my_survey_list[[3]], "]]"), 
            unlist(my_survey_list[[4]]), 
            " ", 
            "[[Choices]]", 
            cbind(my_survey_list[[5]]))
  
  cat(text, sep = '\n', file = "~/Desktop/output.txt", append = TRUE)
  
}
  
survey_output(
  format         = "MA", 
  qid            = "q1",
  qtext          = c("This is a multiple choice question"), 
  answer_choices = c("test1", "test2", "test3"), 
  location       = file.path("~/Desktop/output.text")
)
  
  







fileConn <- file("~/Desktop/output.txt")
write("[This is advanced]", append=TRUE, fileConn)
close(fileConn)

fileConn <- file("~/Desktop/output.txt")
write(c(paste0(num[1], ".", " ", "Q-text..."),
             " ",
             "a",
             "b", 
             "c"), append=TRUE, fileConn)
close(fileConn)

close(fileConn)
