#This is a function to compare the first n rows of lemposes 

compare_ranks <- function(corpus1, corpus2, row_count) {
  rank_intersects = merge(corpus1, corpus2, by = "X.U.FEFF.lempos", all = FALSE)
  total = nrow(rank_intersects)
  pct_total = total/row_count
  #model <- lm(rank_intersects$rank.x ~ rank_intersects$rank.y)
  #print(summary(model))
  cat("total overlapping lemposes:", total, "\n")
  cat("percent total overlapping lemposes:", pct_total, "\n")
  cat("spearman correlation:")
  print(cor.test(rank_intersects$rank.x,rank_intersects$rank.y, method="spearman"))
  #View(rank_intersects)
}

summarize_pos <- function(corpus, row_count) {
  count_pos <- function(corpus, tag){
    return(sum(corpus$pos == tag))
  }
  
  pos_data <- data.frame(
    pos = c("nouns", "verbs", "adjectives", "adverbs", "conjunctions", 
            "prepositions", "pronouns", "interjections", "numbers", "other"),
    num_pos = c(count_pos(corpus, "N"), count_pos(corpus, "V"),count_pos(corpus, "A"),
                  count_pos(corpus, "D"), count_pos(corpus, "J"),count_pos(corpus, "R"),
                  count_pos(corpus, "P"), count_pos(corpus, "I"),count_pos(corpus, "C"),
                  count_pos(corpus, "X")), 
    percent_pos = c(count_pos(corpus, "N")/row_count, count_pos(corpus, "V")/row_count,count_pos(corpus, "A")/row_count,
                count_pos(corpus, "D")/row_count, count_pos(corpus, "J")/row_count,count_pos(corpus, "R")/row_count,
                count_pos(corpus, "P")/row_count, count_pos(corpus, "I")/row_count,count_pos(corpus, "C")/row_count,
                count_pos(corpus, "X")/row_count), 
    stringsAsFactors = FALSE
   )
  print(pos_data)
}


intersect_all_lempos <- function(){
  #browser()
  merge1 = merge(koditex, ORALv1, by="X.U.FEFF.lempos", suffixes=c(".koditex", ".ORALv1"))
  merge1[,c("lemma.koditex", "pos.koditex", "X", "lemma.ORALv1", "pos.ORALv1")]=NULL
  colnames(ORTOFONv2)=c("X.U.FEFF.lempos", 'lemma', 'rank.ORTOFONv2', 'pos')
  merge2 = merge(merge1, ORTOFONv2, by="X.U.FEFF.lempos")
  merge2[,c('lemma', 'pos')]=NULL
  colnames(syn2020)=c("X.U.FEFF.lempos", 'lemma', 'rank.syn2020', 'pos')
  merge3 = merge(merge2, syn2020, by="X.U.FEFF.lempos")
  merge3[,c('lemma', 'pos')]=NULL
  colnames(cstenten17)=c("X.U.FEFF.lempos", 'lemma', 'rank.syntenten17', 'pos')
  merge4 = merge(merge3, cstenten17, by="X.U.FEFF.lempos")
  merge4[,c('lemma', 'pos')]=NULL
  lempos_intersection = merge4
  return(lempos_intersection)
}

union_all_lempos <- function(){
  merge1 = merge(koditex, ORALv1, by="X.U.FEFF.lempos", suffixes=c(".koditex", ".ORALv1"), all=TRUE)
  merge1[,c("lemma.koditex", "pos.koditex", "X", "lemma.ORALv1", "pos.ORALv1")]=NULL
  colnames(ORTOFONv2)=c("X.U.FEFF.lempos", 'lemma', 'rank.ORTOFONv2', 'pos')
  merge2 = merge(merge1, ORTOFONv2, by="X.U.FEFF.lempos", all=TRUE)
  merge2[,c('lemma', 'pos')]=NULL
  colnames(syn2020)=c("X.U.FEFF.lempos", 'lemma', 'rank.syn2020', 'pos')
  merge3 = merge(merge2, syn2020, by="X.U.FEFF.lempos", all=TRUE)
  merge3[,c('lemma', 'pos')]=NULL
  colnames(cstenten17)=c("X.U.FEFF.lempos", 'lemma', 'rank.syntenten17', 'pos')
  merge4 = merge(merge3, cstenten17, by="X.U.FEFF.lempos", all=TRUE)
  merge4[,c('lemma', 'pos')]=NULL
  lempos_union = merge4
  return(lempos_union)
}

# get the union of the CGSL, the spoken CGSL, and the written CGSL
union_CGSL_spok_writ <- function(CGSL, spoken_CGSL, written_CGSL){
  new_CGSL <- CGSL[, c('X.U.FEFF.lempos', 'final_rank')]
  new_spoken_CGSL <- spoken_CGSL[, c('X.U.FEFF.lempos', 'final_rank')]
  new_written_CGSL <- written_CGSL[, c('X.U.FEFF.lempos', 'final_rank')]
  
  merge1 = merge(new_CGSL, new_spoken_CGSL, by="X.U.FEFF.lempos", 
                 suffixes=c(".CGSL", ".spoken_CGSL"), all=TRUE)
  merge2 = merge(merge1, new_written_CGSL, by="X.U.FEFF.lempos", all=TRUE)
  colnames(merge2)= c("X.U.FEFF.lempos", 'final_rank.CGSL', 'final_rank.spoken_CGSL', 'final_rank.written_CGSL')
  return(merge2)
}

#newDf = oldDf[, c('col1','col4','col6')]


#re-rank a list by mean + min
rerank <- function(my_list){
  #my_list$score = my_list$median + .1 * my_list$min + my_list$product * 1e-21
  my_list <- my_list[with(my_list, order(median, min, product)),]
  my_list$final_rank <- 1:nrow(my_list)
  return(my_list)
}

#median + (.1 * min) + (.001 * product)
#final tie breaker
#max(CGSL$product) * 1e-20
#2,500,625,000,000,000,000

#add summary stats columns
add_summary_cols <- function(df, num_cols){
  df$mean = apply(df[, 2:num_cols], 1, mean)
  df$max = apply(df[, 2:num_cols], 1, max)
  df$min = apply(df[, 2:num_cols], 1, min)
  df$median = apply(df[, 2:num_cols], 1, median)
  df$product = apply(df[, 2:num_cols], 1, prod)
  return(df)
}



#replacing NAs with a value of n
replace_NAs <- function(lempos_union, replacement){
  lempos_union[is.na(lempos_union)] <- replacement
  return(lempos_union)
}

#reordering the list
#lemposes_intersection_RF = lemposes_intersection_RF[order(lemposes_intersection_RF$mean),]
#this does not need a function because it's really short, but I will forget the syntax so here it is!

#intersection of spoken
intersect_spoken_lempos <- function(){
  merge1 = merge(ORALv1, ORTOFONv2, by="X.U.FEFF.lempos", suffixes=c(".ORALv1", ".ORTOFONv2"))
  merge1[,c("lemma.ORALv1", "pos.ORALv1", "X", "lemma.ORTOFONv2", "pos.ORTOFONv2")]=NULL
  return(merge1)
}

#intersection of written
intersect_written_lempos <- function(){
  merge1 = merge(koditex, syn2020, by="X.U.FEFF.lempos", suffixes=c(".koditex", ".syn2020"))
  merge1[,c("lemma.koditex", "pos.koditex", "X", "lemma.syn2020", "pos.syn2020")]=NULL
  colnames(cstenten17)=c("X.U.FEFF.lempos", 'lemma', 'rank.cstenten17', 'pos')
  merge2 = merge(merge1, cstenten17, by="X.U.FEFF.lempos")
  merge2[,c('lemma', 'pos')]=NULL
  return(merge2)
}

#find the strings that are different
find_diff <- function(mylist){
  mypairs <- NULL
    for (i in 1:length(mylist)){
  #for (i in 1:length(mylist[[1]])){
    mystr1 = mylist[[i]][1]
    mystr2 = mylist[[i]][2]
    if(nchar(mystr1)==nchar(mystr2)){
      if(is.null(mypairs) || 
         (nrow(subset(mypairs, str1==mystr2 & str2==mystr1))==0 && 
         nrow(subset(mypairs, str1==mystr1 & str2==mystr2))==0)){
        s1=unlist(strsplit(mystr1,""))
        s2=unlist(strsplit(mystr2,""))
        if(length(s1) != length(s2)){
          browser()
        }
        myphone <- which(s1!=s2)
        mypairs <- rbind(mypairs, data.frame(str1=mystr1, str2=mystr2, phone1=s1[myphone], phone2=s2[myphone]))
      } 
    }
  }
  return(mypairs)
}


#phonetic difference
get_phon_table <- function (in_diff, numcols){
  phon_table <- NULL
  for(i in 1:nrow(in_diff)){
    for(j in 1:nrow(in_diff)){
      if(i==j)
        next
      row1 = in_diff[i,]
      row2 = in_diff[j,]
      if(!is.null(phon_table) && nrow(subset(phon_table, char1==row2[1,1] & char2==row1[1,1])))
        next
      diff_count <- sum(row1[1,2:numcols]!=row2[1,2:numcols])
      phon_table <- rbind(phon_table, data.frame(char1=in_diff[i,1], char2=in_diff[j,1], diff_count=diff_count))
      
    }
  }
  return(phon_table)
} 
