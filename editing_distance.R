#install.packages('stringdist')

words <- toString(CGSL_RF$X.U.FEFF.lempos)
# this is not an elegant solution, but if you do "head" here, then you can 
# copy and paste this as a string, save it as a .txt file (remove the " "'s
# around the whole thing), and then import the data into excel, transposing the
# data and making sure to split on the ,'s 
# This is a little bit silly.
head(words)



#read in my file
CGSL_525 = read.csv('C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/CGSL_RF_IPA.csv', encoding="UTF-8", stringsAsFactors = FALSE)
View(CGSL_525)
CGSL_525_length <- nrow(CGSL_525)

#collapse into a single string
big_string <- paste(CGSL_525$modified.lemma.IPA, collapse="", sep="")
char_list <- unlist(strsplit(big_string, ""))
char_table <- table(char_list)
View(char_table)
write.csv(char_table, "C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/char_table.csv", fileEncoding='UTF-8', row.names=FALSE)


#load stringdist package
library(stringdist)

dist_matrix <- matrix(nrow=CGSL_525_length, ncol=CGSL_525_length, dimnames=list(CGSL_525$modified.lemma.IPA, CGSL_525$modified.lemma.IPA))

lev_1 = list()
lev_2 = list()
lev_3 = list()
lev_4 = list()

for(i in 1:CGSL_525_length)
{
  for(j in 1:CGSL_525_length)
  {
    lev_dist <- stringdist(CGSL_525$modified.lemma.IPA[i], CGSL_525$modified.lemma.IPA[j], method='lv')
    dist_matrix[i,j] <- lev_dist
    if(lev_dist==1){
      lev_1[[length(lev_1)+1]] <- c(CGSL_525$modified.lemma.IPA[i], CGSL_525$modified.lemma.IPA[j])
    }
    else if(lev_dist==2){
      lev_2[[length(lev_2)+1]] <- c(CGSL_525$modified.lemma.IPA[i], CGSL_525$modified.lemma.IPA[j])
    }
    else if(lev_dist==3){
      lev_3[[length(lev_3)+1]] <- c(CGSL_525$modified.lemma.IPA[i], CGSL_525$modified.lemma.IPA[j])
    }
    else if(lev_dist==4){
      lev_4[[length(lev_4)+1]] <- c(CGSL_525$modified.lemma.IPA[i], CGSL_525$modified.lemma.IPA[j])
    }
  }
  #this is a counter so we can monitor the progress while it is running
  cat(paste(i, "\r      "))
}


mypairs <- find_diff(lev_1)
View(mypairs)
iconv(mypairs, from = "", to = "UTF-8", sub=NA, mark= TRUE, toRaw=TRUE)
View(mypairs)
head(mypairs)

try(utils::head(iconvlist(), n=50))


write.csv(mypairs, "C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/mypairs.csv", fileEncoding='UTF-8', row.names=FALSE)


#Comparing the phonetic differences
cons_diff <- read.csv('C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/phonetic_diff.csv', encoding="UTF-8", stringsAsFactors = FALSE)
vowels_diff <- read.csv('C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/vowel_diff.csv', encoding="UTF-8", stringsAsFactors = FALSE)

View(vowels_diff)

cons <- get_phon_table(cons_diff, 9)
cons_min <- subset(cons, diff_count<3)
View(cons_min)

vowels <- get_phon_table(vowels_diff, 5)
View(vowels)
vowels_min <- subset(vowels, diff_count<3)
View(vowels_min)
vowels_2 <- subset(vowels, diff_count<3 & diff_count>1)
View(vowels_2)

vowels_1 <- subset(vowels, diff_count<2)
View(vowels_1)
cons_2 <- subset(cons, diff_count<3 & diff_count>1)
View(cons_2)
cons_1 <- subset(cons, diff_count<2)
View(cons_1)

nrow(cons_1)
nrow(cons_2)

View(lev_1)
View(lev_2)
View(lev_3)
length(lev_1)
View(dist_matrix[1:100, 1:100])
dist_matrix <- as.data.frame(dist_matrix)
rownames(dist_matrix) <- CGSL_525$X.U.FEFF.lempos
colnames(dist_matrix) <- CGSL_525$X.U.FEFF.lempos
View(dist_matrix[1:100, 1:100])

head(lev_1)

#
lev_1 <- which(dist_matrix == 1)

print(lev_1)
write.csv(lev_1, "C:/Users/kate/OneDrive/Desktop/Czech general service list/v3/lev_1.csv", fileEncoding='UTF-8', row.names=FALSE)


#calculate Levenshtein distance
lev_dist <- stringdist(CGSL_525$lemma, CGSL_525$compare, method='lv')

CGSL_525$lev_dist <- lev_dist

View(CGSL_525)

View(lev_1)
