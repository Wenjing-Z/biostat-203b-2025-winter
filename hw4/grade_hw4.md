*Wenjing Zhou*

### Overall Grade: 174/250

### Late penalty

- Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  
	- No, 1 day late (-10)
### Quality of report: 10/10

-   Is the final report in a human readable format html? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

### Completeness, correctness and efficiency of solution: 143/200

- Q1 (98/100)
	- Q1.8 - Show number of unknown for vitals (-2)

If `collect` before end of Q1.7, take 20 points off.

If ever put the BigQuery token in Git, take 50 points off.

Cohort in Q1.7 should match that in HW3.

Q1.8 summaries should roughly match those given.
  
- Q2 (45/100)
	- Names of variables should be adjusted for a more professional-looking app (e.g. "marital_status_" --> "Marital Status") (-5)
	- Many of the chosen variables produce errors for the Cohort Summaries (-10)
	- Patient lookup is set up correctly but does not produce any graphs upon input (-40)

### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put a lot auxiliary files into version control. 

-   If those gz data files are in Git, take 5 points off.

### Reproducibility: 5/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 
	- Does not connect to database in app.R using bq_auth() and dbConnect() (-5)_
-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 16/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  
	Many violations in app.R and .qmd (-4)
-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
