### Problem Two ###----


# You have a class with several students -- They just took an exam!
# Here are the results (run the code):

students <- c("Johnny", "Frank", "Timmy", "Alice", "Susan")
num_grades <- c(88, 90, 76, 98, 85)
names(num_grades) <- students 
class(num_grades)

# Problem: Convert the numeric grades (e.g., 85) to letter grades (e.g., "B")----
# Assume standard conversions... 90+ is "A", 80-89 is "B", etc
# Loop through the vector of numeric grades. Convert to letter grades.
# Save the results in a new vector, "letter_grades"
# Finally, make a dataframe, "student_grades", with columns made of "letter_grades", "num_grades", and "students"
# Save "student_grades" as a csv file.

letter_grades <- c()


for (i in 1:length(num_grades)){
  
  if (num_grades[i] >= 90){
    letter_grades[i] <- "A"
  }
  
}

letter_grades <- c()
for (i in 1:length(num_grades)){
  if (num_grades[i] >= 90) {
    letter_grades[i] = "A"
  }
  else if (num_grades[i] > 80 & num_grades[i] <= 89){
    letter_grades[i] = "B"
  } else { letter_grades[i] <- "F"
    }
}

letter_grades

student_grades <- data.frame(students = students,
                             num_grades = num_grades,
                             letter_grades = letter_grades)

write.csv(student_grades, "student_grades.csv")





### Challenge Problem ###----

# You just discovered some genes which are associated with cancer progression. Cool!
# However... you only know the Ensembl gene IDs (e.g., ENSG00000141510) of these genes...
# These IDs are hard to read, so you need to convert them to gene symbols (e.g., TP53)

cancer_genes <- c("ENSG00000139618", "ENSG00000106462", "ENSG00000116288")
cancer_genes


# Problem: Make a function to convert Ensembl gene IDs to gene symbols. ----    
# You have also been provided with a data.frame has the mapping of ids to symbols

id2symbol <- data.frame(
  "Ensembl" = c("ENSG00000141510", "ENSG00000139618", "ENSG00000106462", "ENSG00000116288"),
  "gene_symbol" = c("TP53", "BRCA2", "EZH2", "PARK7")
)

gene_converter <- function(gene_Id){
  # Check if gene_Id exists in the Ensembl column
  if (gene_Id %in% id2symbol$Ensembl){
    # Return the corresponding gene_symbol
    return(id2symbol$gene_symbol[id2symbol$Ensembl == gene_Id])
  } else {
    # Return NA or a message if gene_Id is not found
    return(NA)
  }
}


gene_converter(gene_Id = "ENSG00000141510" )


### SUPER Challenge Problem ###----

# Your gene_id_converter is working splendidly! But now you've got a new problem:
# Your colleague just sent you a ton of new genes to convert! None of these new
# genes are in your id2symbol data frame... 

# Problem: Extend you gene_id_converter function to convert ANY ensembl gene id to a gene symbol
# To assist you, your advisor sent you a csv file that contains the mapping of all ids to symbols. 

# This the mapping file and it's in the resources folder.

# Perfecto! Only one problem: now we need to convert symbols to ids!----

# Problem: extend the gene_id_converter function so it can convert symbols to gene IDs!
# Requirement: instead of "gene_id", it should now accept the argument "gene" as this 
# argument can be either a gene id or a symbol. 
# The "type" argument should now accept the option "gene_symbol"

"gene_id_to_symbol.csv" 



#Import mapping file

library(readr)

gene_id_to_symbol <- read_csv("week_two/gene_id_to_symbol.csv")

class(gene_id_to_symbol)

gene_converter_all <- function(gene, type) {
  if (type == "Ensembl"){
    if (gene %in% gene_id_to_symbol$Ensembl){
      return(gene_id_to_symbol$gene_symbol[gene_id_to_symbol$Ensembl==gene])
      
    }
    
  }
  else if (type == "Entrez"){
    if (gene %in% gene_id_to_symbol$Entrez){
      return(gene_id_to_symbol$gene_symbol[gene_id_to_symbol$Entrez==gene])
    }
    
  }
  #major code solution for the third problem
  else if ( type == "gene_symbol"){
    if (gene %in% gene_id_to_symbol$gene_symbol){
    
      return (list(
        Entrez = gene_id_to_symbol$Entrez[gene_id_to_symbol$gene_symbol==gene],
        Ensembl = gene_id_to_symbol$Ensembl[gene_id_to_symbol$gene_symbol==gene]
      ))
    }
    
  }
  return(NA)
}
 

# Test 1 -- this should output "BRCA1"
gene_converter_all(gene="ENSG00000012048", type = "Ensembl")
gene_converter_all(gene= 472, type = "Entrez")
gene_converter_all(gene = "BRCA1", type = "gene_symbol")


  
  
  
  