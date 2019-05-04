
fileName = "pharmacy.csv"

#Open file if exist and read its content, else create it.
if (file.exists(fileName)){
  data <- read.csv(fileName, stringsAsFactors = F) 
}else{
  file.create(fileName)
  data <- data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors = F)
  names <- c("id","name", "price", "amount")
  colnames(data) <- names
}

#Search By name
search_name <- function(n){
  med_name = subset(data, tolower(name) == tolower(n))
  if(nrow(med_name) != 0){
    View(med_name)
    cat("med found")
  }else{
    cat("med not found!")
  }
}

#Search By id
search_id <- function(med_id){
  med = subset(data, med_id == id)
  if(nrow(med) != 0){
    cat("med found!")
    View(med)
  }else{
    cat("med not found!")
  }
}

#create 
create <- function(){
  cat("Enter med details", '\n')
  id = nrow(data) + 1
  name = readline("Name: ")
  price = readline("price: ")
  amount = readline("amount: ")

  
  newEmp = data.frame(
    id = c(id),
    name = c(name),
    price = c(price),
    amount = c(amount),
    stringsAsFactors = F
  )
  
  data <<- rbind(data, newEmp)
  cat("med added successfully")
}

#Update 
update <- function(id){
  
  cat('\n',"1- Name",'\n',"2- price",'\n',"3- amount",'\n')
  
  op = readline("option: ")
  newValue = readline("new value: ")
  switch(op,
    '1' = {data[id,2] <<- newValue},
    '2' = data[id,3] <<- newValue,
    '3' = data[id,4] <<- newValue,
    '4' = data[id,5] <<- newValue, 
    {#default
      cat("Wrong! please try again.")
    }
  )
  cat("med updated successfully")
  
}

#Remove 
delete <- function(med_id){
  data <<- subset(data, id != med_id)
  cat("med deleted successfully")
}

#If file exists truncate it, else create it.
save <- function(){
  write.csv(data, fileName, row.names = F)
  cat("File saved successfully")
}

#Perform choosen file operation for user.
performOpertaion <- function(op){
  switch (op,
    '1' = { #Show All
      print(data)
      View(data)
    },
    '2' = { #Create
      create()
      View(data)
    },
    '3' = { #Search
      cat("1 - Search by name", '\n')
      cat("2 - Search by id", '\n')
      searchOp = readline("Enter option: ")
      switch (searchOp,
        '1' = { #Search by name
          name = readline("name: ")
          search_name(name)
        },
        '2' = { #Search by id 
          id = readline("id: ")
          search_id(id)
        }, #default
        {
          cat("Wrong! please try again.")
        }
      )
      
    
    },
    '4' = { #Update
      id = readline("id: ")
      id = as.integer(id)
      update(id)
      View(data)
    },
    '5' = { #Delete
      id = readline("id: ")
      delete(id)
      View(data)
    },
    '6' = { #Save
      save()
    }, #default
    {
      cat("Wrong! please try again.")
    }
  )
  
  
  readline()
  
}

main <- function(){

  repeat{
    

    cat(
   
    "1 - Display", '\n',
    "2 - insert", '\n',
    "3 - Search", '\n',
    "4 - edit", '\n',
    "5 - Delete", '\n',
    "6 - Save work \n",
    "7 - Exit", '\n',
    '\n\n')
    
    op = readline("Enter option: ")
    op = as.integer(op)
    if(op == 7){
      return()
    }else{
    performOpertaion(op)
  }
  }
}





main()
