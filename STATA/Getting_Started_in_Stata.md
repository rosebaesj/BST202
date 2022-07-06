# What's it like–Getting started in Stata
from [youtube](https://www.youtube.com/watch?v=YAVq99iUTTI)

## Open VDI to use STATA 17
STATA 17 is available in VDI, so you should get logged in VDI first. 
(Or you can just buy the student version, but it costs money) 
But be careful since you can loose your work if you save it directly. 
You should save them in "Personal :P" directory for permanent save.

1. Open VDI
2. Login with NetID + type "push" in password section.
3. Login again with HarvardID+PW
4. Wait for it to set up

Then you're good to go

## Getting started
### Import data
#### 1. From excel 
1. Simply copy them from excel
2. Open data editor from upper tool bar
3. Paste

#### 2. Directly import
1. Upper File - Import - Excel (or any other)
2. browse file and open
3. First row as variable names, etc.
4. Open data editor to take a look

### Decimal places
Don't want to round but just to show them 
1. select the variable
2. format
3. fixed numeric 
4. digits 

### Value lable
1. select the variable
2. create value lable 
3. value - lable 

### Create new variable
Convert column to column
1. Data-Create or change data- Create new variable.
2. Variable name
3. Create
4. select a variable and make an equation
5. click OK
6. At view, add Label

### Discribe data
1. Data - Describe data - Discribe data in memory or in a file
2. Don't select to describe all
Summarize
1. Statistics - Summaries, tables, and tests, - Summary and descriptive statistics - Summary statistics
2. Don't select to summarize all
Graphics
1. Graphics - Histogram
2. Select variable 
3. Density plots - normal distribution etc
4. Submit (leave the dialog open), OK (close the dialog)

### Save commands
1. Review window - select commands
2. Right click - Send selected to Do-file editor
3. Save. 

### Help, manuals
~~~~STATA~~~~
search regress, manual
search regress, faq
search regress, sj
