#Case Study: Calculating NPS
ltr <- c(9,8,3,9,7,8,9,6,7,8,9)
range(ltr)
promoters <- ltr[ltr>8]
numPromoters <- length(promoters)
detractorsTrueFalse <- ltr < 7
numDetractors <- sum(detractorsTrueFalse)
total <- length (ltr)
#Does it follow mathematical logic?
nps <- (numPromoters/total - numDetractors/total)*100
nps
range(ltr)

#Chapter End Questions
#1. Use the c() function to add another family member’s age onto the end of the myFamilyAges vector.
myFamilyAges <- c(43, 43, 12, 8, 5)
myFamilyAges <- append(myFamilyAges, 25)
myFamilyAges

#2. Use square brackets subsetting to show just the first element of the myFamilyAges vector.
myFamilyAges[c(1)]

#3. Use square brackets subsetting together with the c() command to show just the odd numbered items from the myFamilyAges vector (i.e., just the first, third, and fifth items from this vector).
myFamilyAges[c(1,3,5)]
# Found another way
myFamilyAges[seq(1, length(myFamilyAges), 2)]

#4. Create a conditional expression that outputs a set of TRUEs and FALSEs. The expression should show TRUE when an element of the myFamilyAges vector is equal to 12.
myFamilyAges == 12

#5. Using the code from the previous item, put an exclamation point in front of the conditional expression.
myFamilyAges != 12


#6. Use the conditional expression from the previous item within the square brackets to select those elements of the myFamilyAges vector that are not equal to 12.
myFamilyAges[myFamilyAges!=12]

#7. Power User: Using the built-in Nile dataset, create a conditional expression that shows TRUE for every observation where the level of the Nile was over 900. Then use the sum() command to count up how many times the Nile dataset had observations higher than 900.
sum(Nile > 900)
