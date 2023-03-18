library(dplyr)
library(quanteda)
library(wordcloud)
library(RColorBrewer)

# Transfer to quanteda corpus format and segment into sentences
fun.corpus = function(x) {
  corpus(unlist(segment(x, 'sentences')))
}

# Tokenize
fun.tokenize = function(x, ngramSize = 1, simplify = TRUE) {
  tolower(
    quanteda::tokens(x,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeSeparators = TRUE,
                       removeTwitter = TRUE,
                       ngrams = ngramSize,
                       concatenator = " ",
                       simplify = simplify
    ) 
  )
}



# Prediction cumulative algorithm 

fun.predict = function(x,y, n = 3) {
  
  t_uni <- min(dfTrain1 %>%    
    arrange(desc(Share)) %>% select(Share)%>%
    slice(1:n))
  
  if(isTruthy(x) & isTruthy(y)) 
    {if (paste(y,x,sep="") %in% paste(dfTrain3$word1,dfTrain3$word2, sep=""))
       {
        prediction = dfTrain3 %>%
        filter(word2 %in% x & word1 %in% y) %>%
        mutate(Nextword=word3) %>%
        select(Nextword, Share, freq)
       }
    else if (x %in% dfTrain2$word1)
      {
        prediction = dfTrain2 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
      }
    else if (x %in% dfTrain3$word1)
      {
      prediction = dfTrain3 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
      }
    else if (x %in% dfTrain3$word2)
      {
      prediction = dfTrain3 %>%
        filter(word2 %in% x) %>%
        mutate(Nextword=word3) %>%
        select(Nextword, Share, freq)
      }
    else 
     {
      prediction = dfTrain1 %>%
      filter(Share>= t_uni) %>%
      mutate(Nextword=word1) %>%
      select(Nextword, Share, freq)
     }
    }   
  else if (isTruthy(x) & !isTruthy(y))
  { 
    if (x %in% dfTrain2$word1)
    {
    prediction = dfTrain2 %>%
      filter(word1 %in% x) %>%
      mutate(Nextword=word2) %>%
      select(Nextword, Share, freq)
    }
    else if (x %in% dfTrain3$word1)
    {
      prediction = dfTrain3 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
    }
    else if (x %in% dfTrain3$word2)
    {
      prediction = dfTrain3 %>%
        filter(word2 %in% x) %>%
        mutate(Nextword=word3) %>%
        select(Nextword, Share, freq)
    }
    else 
    {  
       prediction = dfTrain1 %>%
       filter(Share>= t_uni) %>%
       mutate(Nextword=word1) %>%
       select(Nextword, Share, freq)
    } 
  }
    else 
    {   
        
        prediction = dfTrain1 %>%
        filter(Share>= t_uni) %>%
        mutate(Nextword=word1) %>%
        select(Nextword, Share, freq)
    }
  
  return(prediction[1:n, ])
}

# Prediction Tri-gram algorithm 
fun.predict3 = function(x,y, n = 3) {
  
  t_uni <- min(dfTrain1 %>%    
                 arrange(desc(Share)) %>% select(Share)%>%
                 slice(1:n))
  
  if(isTruthy(x) & isTruthy(y)) 
  {if (paste(y,x,sep="") %in% paste(dfTrain3$word1,dfTrain3$word2, sep=""))
  {
    prediction = dfTrain3 %>%
      filter(word2 %in% x & word1 %in% y) %>%
      mutate(Nextword=word3) %>%
      select(Nextword, Share, freq)
  }
    else if (x %in% dfTrain3$word1)
    {
      prediction = dfTrain3 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
    }
    else if (x %in% dfTrain3$word2)
    {
      prediction = dfTrain3 %>%
        filter(word2 %in% x) %>%
        mutate(Nextword=word3) %>%
        select(Nextword, Share, freq)
    }
    else 
    {
      prediction = dfTrain3 %>%
        filter(Share>= 0) %>%
        mutate(Nextword=word1) %>%
        select(Nextword, Share, freq)
    }
  }   
  else if (isTruthy(x) & !isTruthy(y))
  { 
    if (x %in% dfTrain3$word1)
    {
      prediction = dfTrain3 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
    }
    else if (x %in% dfTrain3$word2)
    {
      prediction = dfTrain3 %>%
        filter(word2 %in% x) %>%
        mutate(Nextword=word3) %>%
        select(Nextword, Share, freq)
    }
    else 
    {  
      prediction = dfTrain3 %>%
        filter(Share>= 0) %>%
        mutate(Nextword=word1) %>%
        select(Nextword, Share, freq)
    } 
  }
  else 
  {   
    
    prediction = dfTrain3 %>%
      filter(Share>= 0) %>%
      mutate(Nextword=word1) %>%
      select(Nextword, Share, freq)
  }
  
  return(prediction[1:n, ])
}
# Prediction Bi-gram algorithm
fun.predict2 = function(x,y, n = 3) {
  
  t_uni <- min(dfTrain1 %>%    
                 arrange(desc(Share)) %>% select(Share)%>%
                 slice(1:n))
  
  if(isTruthy(x) & isTruthy(y)) 
  {
    if (x %in% dfTrain2$word1)
    {
      prediction = dfTrain2 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
    }
    else 
    {
      prediction = dfTrain2 %>%
        filter(Share>= 0) %>%
        mutate(Nextword=word1) %>%
        select(Nextword, Share, freq)
    }
  }   
  else if (isTruthy(x) & !isTruthy(y))
  { 
    if (x %in% dfTrain2$word1)
    {
      prediction = dfTrain2 %>%
        filter(word1 %in% x) %>%
        mutate(Nextword=word2) %>%
        select(Nextword, Share, freq)
    }
    else 
    {  
      prediction = dfTrain2 %>%
        filter(Share>= 0) %>%
        mutate(Nextword=word1) %>%
        select(Nextword, Share, freq)
    } 
  }
  else 
  {   
    
    prediction = dfTrain2 %>%
      filter(Share>= 0) %>%
      mutate(Nextword=word1) %>%
      select(Nextword, Share, freq)
  }
  
  return(prediction[1:n, ])
}
# Prediction Uni-gram algorithm
fun.predict1 = function(x,y, n = 3)
  {
  
  t_uni <- min(dfTrain1 %>%    
                 arrange(desc(Share)) %>% select(Share)%>%
                 slice(1:n))
 
    prediction = dfTrain1 %>%
      filter(Share>= t_uni) %>%
      mutate(Nextword=word1) %>%
      select(Nextword, Share, freq)
  
  return(prediction[1:n, ])
}
