#question 1
#Design a function that reverses the digits of an integer. For example, 50
#should become 5 and -12 should become -21.

reverse_digits <- function(x) {
  # Check if the number is negative
  is_negative <- x < 0
  
  # Convert the number to a string and remove the negative sign if necessary
  x_str <- as.character(abs(x))
  
  # Reverse the string
  reversed_str <- paste(rev(strsplit(x_str, NULL)[[1]]), collapse = "")
  
  # Convert the reversed string back to an integer
  reversed_int <- as.integer(reversed_str)
  
  # Restore the negative sign if the original number was negative
  if (is_negative) {
    reversed_int <- -reversed_int
  }
  
  return(reversed_int)
}

#Question 2
#Write a recursive function to calculate the factorial of a number

# Test the function
print(reverse_digits(50))  # Should print 5
print(reverse_digits(-12)) # Should print -21

factorial_recursive <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial_recursive(n - 1))
  }
}

# Test the function
print(factorial_recursive(5))  # Should print 120
print(factorial_recursive(0))  # Should print 1

#question 3
#Design a function that takes a string or sequence of characters as input and
#returns the character that appears most frequently.
#//Eg 11189 => '1'
#//hello => l

most_frequent_char <- function(input_str) {
  # Split the input string into individual characters
  chars <- unlist(strsplit(input_str, NULL))
  
  # Create a table of character frequencies
  freq_table <- table(chars)
  
  # Identify the character with the highest frequency
  most_frequent <- names(freq_table)[which.max(freq_table)]
  
  return(most_frequent)
}

# Test the function
print(most_frequent_char("11189")) # Should print '1'
print(most_frequent_char("hello")) # Should print 'l'

#question 4
#Design a function that determines whether a given string is a pangram. A
#pangram is a sentence or phrase containing every letter of the alphabet at
#least once. Punctuation and case are typically ignored. For example, the
#string "The quick brown fox jumps over the lazy dog" is a pangram, while
#"Hello, world!" is not.

is_pangram <- function(input_str) {
  # Convert the input string to lowercase
  input_str <- tolower(input_str)
  
  # Remove non-alphabetic characters
  input_str <- gsub("[^a-z]", "", input_str)
  
  # Get the unique characters in the string
  unique_chars <- unique(strsplit(input_str, NULL)[[1]])
  
  # Check if the length of unique characters is 26 (number of letters in the alphabet)
  return(length(unique_chars) == 26)
}

# Test the function
print(is_pangram("The quick brown fox jumps over the lazy dog")) # Should print TRUE
print(is_pangram("Hello, world!"))                              # Should print FALSE

#question 5
#Design a function that takes a list of integers as input. The function should
#return True if the list contains two consecutive threes (3 next to a 3) anywhere
#within the list. Otherwise, it should return False. For example, the function
#should return True for [1, 3, 3] and False for [1, 3, 1, 3].

has_consecutive_threes <- function(lst) {
  for (i in 1:(length(lst) - 1)) {
    if (lst[i] == 3 && lst[i + 1] == 3) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#question 6
#Master Yoda, a renowned Jedi Master from the Star Wars universe, is known
#for his unique way of speaking. He often reverses the order of words in his
#sentences. For example, instead of saying "I am home" he might say "Home
#am I" Design a function that takes a sentence as input and returns a new
#sentence with the words reversed in the same order that Master Yoda would
#use.

yoda_speak <- function(sentence) {
  words <- strsplit(sentence, " ")[[1]]  # Split the sentence into words
  reversed_words <- rev(words)  # Reverse the order of the words
  yoda_sentence <- paste(reversed_words, collapse = " ")  # Join the words back into a sentence
  return(yoda_sentence)
}
