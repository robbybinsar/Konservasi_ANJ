library(stringr)
#str view
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

#str detect
x <- c("apple siaam", "banana siaam", "pear")
str_detect(x, coll("[appsiam]"))

#str word
words[str_detect(words, "x$")]
str_subset(words, "galss$")

#str count
x <- c("apple siam", "banana siam", "pear")
c <- str_count(x, "[apple siam]")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
#> [1] 1.99

str_count("abababa", "aba")
#> [1] 2
str_view_all("abababa", "aba")

#Replacing matches
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
#> [1] "-pple"  "p-ar"   "b-nana"
str_replace_all(x, "[aeiou]", "-")
#> [1] "-ppl-"  "p--r"   "b-n-n-"




strings <- c("macaca fascicularis", "macaca cegewg", "macaca ", "c abd")

grep("ab", strings)
grep("[macaca fasiclaris]", strings, value = FALSE)
grep("ab", strings, value = TRUE)

