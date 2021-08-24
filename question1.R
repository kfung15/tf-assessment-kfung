# Create the employee table

name_list = list(c("John", "Mike", "Sally", "Jane", "Joe", "Dan", "Phil"))
salary_list = list(c(300, 200, 550, 500, 600, 600, 550))
direct_supervisor_list = list(c(3,3,4,7,7,3,NA))

employee_table = data.frame(unlist(name_list),unlist(salary_list),unlist(direct_supervisor_list))
colnames(employee_table) = c("Name", "Salary", "manager_id")


# Question 1a
greater_salary_than_manager = c()
for(x in 1:nrow(employee_table)){
  if(!is.na(employee_table[x,3]) & (employee_table[x,2] > employee_table[employee_table[x,3],2])){
    greater_salary_than_manager = c(greater_salary_than_manager,as.character(employee_table[x,1]))
  }
}

print(greater_salary_than_manager) # Sally, Joe, Dan

# Question 1b

total_non_manager_salary = 0
non_manager_count = 0
for(x in 1:nrow(employee_table)){
  if(!(x %in% direct_supervisor_list[[1]])){
    total_non_manager_salary = total_non_manager_salary + employee_table[x,2]
    non_manager_count = non_manager_count + 1
  }
}

avg_non_manager_salary = total_non_manager_salary / non_manager_count
print(avg_non_manager_salary) # 425