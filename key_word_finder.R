# the input abstract is the text file downloaded on pubmed that contain all the abstracts (just put the name, not the .txt), 
# the input output_directory is the path of the output

key_word_finder <- function (abstract, summary,scholar_ID){
    library(pubmed.mineR)
    library(tools)
    library(scholar)
    library(stringr)
    abstractR <- readabs(abstract)
    atomized_text <- word_atomizations(abstractR)
    file_without_ext <- file_path_sans_ext(abstract)
    atomized_file_name <- paste0(file_without_ext, '_atomized.csv')
    write.csv(atomized_text, atomized_file_name)
    output_text <- paste0('Most used words described in ',atomized_file_name,' take the time to read them and to select the relevent key words')
    print(output_text)

    current_word <- ''
    key_words <- c()

    while (current_word != 'stop'){
        current_word <- readline('what is the next key word (print stop if you have enough key words)')

        if ((current_word != '' )& (current_word != 'stop')){
            key_words <- c(key_words, current_word)
            print('your keyword list is composed of : ')
            print(key_words)
        }
    }
    print('your final key words list is : ')
    print(key_words)     
    key_words_recurrence <- tdm_for_lsa(abstractR, key_words)

    csv_summary <- read.csv(summary)
    matrix_summary <- as.matrix(csv_summary)

   

    list_scholar<- get_publications(scholar_ID)
    write.csv(list_scholar,'list_scholar.csv')
    csv_scholar <- read.csv('list_scholar.csv')
    matrix_scholar <- as.matrix(csv_scholar)

    n_row <- dim(matrix_summary)[1]
    n_col <- 7 + length(key_words)
    output <- matrix(nrow = n_row, ncol =n_col)

    columns = c('title','Date','Citation','url','author','journal','number',key_words)
    colnames(output) <- c('title','Date','Citation','url','author','journal','number',key_words)




    s_n_row <- dim(matrix_scholar)[1]


    
    number <- 0

    for (row in (1:n_row)){
        output[row,1] <- matrix_summary[row,1]
        output[row,5] <- matrix_summary[row,3]
        output[row,4] <- paste0('https://www.ncbi.nlm.nih.gov/',matrix_summary[row,2])
        for (s_row in  (1:s_n_row)){
            a = str_to_lower(paste0(matrix_scholar[s_row,2],'.'))
            b = str_to_lower(output[row,1])
            if (a == b){
                output[row,2] <- matrix_scholar[s_row,7]
                output[row,3] <- matrix_scholar[s_row,6]
                output[row,6] <- matrix_scholar[s_row,4]
                output[row,7] <- matrix_scholar[s_row,5]              
            }
        }
    }

    for(j in (1:length(key_words))){
        for (i in (1:n_row)){
            output[i,(j+7)] <- key_words_recurrence[j,i]

        }
    }

    write.csv(output,'output_analysis.csv')
   

    
    
    return()

 }
