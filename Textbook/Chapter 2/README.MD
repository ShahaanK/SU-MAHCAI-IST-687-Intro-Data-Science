# Chapter 2 — Data Frames and NPS (IST 687)

This folder contains my completed work for Chapter 2 of "Data Science for Business with R" by Jeffrey S. Saltz and Jeffrey M. Stanton. All work is part of my progress in IST 687: Introduction to Data Science, taught by Christopher Dunham, during my Master's of Science in Applied Human-Centered AI at Syracuse University.

---

## Contents
- Case Study: Calculating Net Promoter Score (NPS) using a data frame
- Chapter Challenges (data frame manipulation and logical indexing)
- How to Run
- Files

---

## Case Study: Calculating NPS Using a Data Frame
Prompt:
- LTR values: 9, 9, 7, 6, 8, 7
- Type of Travel: "Business travel", "Business travel", "Business travel", "Mileage", "Personal Travel", "Personal Travel"

Tasks (questions only):
1) Create a data frame with columns `ltr` and `TypeOfTravel` using the values above.
2) What is the overall Net Promoter Score (NPS)? Assume promoters are LTR > 8 and detractors are LTR < 7.
3) What is the NPS when considering only the Business travel tickets?
4) Compare the two NPS values. What do you conclude?

---

## Chapter Challenges
Questions (no answers provided):

1) Use `c()` to create a new variable containing the favorite food of each family member (exactly five values). Name it `myFoods`. Use `str()` on `myFoods` to show its type.

2) Add `myFoods` to the `myFamily` data frame so that it appears as a new column.

3) Rerun `summary(myFamily)` to get descriptive information on all variables, including `myFoods`. Note the data type for `myFoods` in a comment.

4) Create an expression that produces TRUE/FALSE values based on `myFamily$myFamilyAges < 40`. Assign it to `myIndex` (TRUE for kids, FALSE for adults).

5) Use `myIndex` to show the favorite foods for each kid in the family (e.g., `myFamily$myFoods[myIndex]`).

6) Use `!` to invert the Boolean values and show the favorite foods for the adults (i.e., the complement of the previous selection).

---

## How to Run
You can use either the script or the R Markdown file.

- Option A — Run the R script:
  - Open `Chapter-2.R` in R or RStudio
  - Source the file to execute all steps

- Option B — Knit the R Markdown:
  - Open `Chapter-2.Rmd` in RStudio
  - Knit to PDF to produce a report (you may need a LaTeX engine such as TinyTeX)

---

## Files
- `Chapter-2.R` — Script version of the chapter’s code (NPS case study and challenges)
- `Chapter-2.Rmd` — R Markdown report with narrative and code chunks


