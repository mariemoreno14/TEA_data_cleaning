Data Reading and Processing: TEA 8th Grade Math Assessments

This repository contains scripts for reading and processing TEA (Texas Education Agency) 8th-grade math assessment data from multiple Excel files. The goal of this project is to clean and reshape the data, making it suitable for further analysis and visualization.


Introduction

In educational research, it's common to work with large amounts of data distributed across multiple files. This project focuses on streamlining the process of reading and transforming such data, making it more efficient and manageable.


Prerequisites

Before you start, ensure you have the following installed:
R (version x.y.z)
RStudio (optional but recommended)


Usage

Clone this repository to your local machine.
Open the R script files using RStudio.
Modify the file paths in the script to point to your data files.


Data Reading

The data is stored in Excel files for different years. The read_excel function from the readxl package is used to read the data into R data frames. To improve efficiency, consider storing all your data files in a consistent directory structure.


Data Processing

The script then performs various data processing steps to clean and reshape the data. Key steps include:

Decoding column names using the match() function and codebook data.
Selecting and renaming relevant columns using regular expressions.
Pivoting the data from wide to long format using the pivot_longer() function.
Aggregating data based on certain criteria, such as year and group.
Handling missing values and converting data types.


Contributing

Contributions are welcome! Feel free to open issues or pull requests if you find improvements or have suggestions.
