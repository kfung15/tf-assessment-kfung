# Question 3

# Nth layer of a pascal triangle

pascal_layer_total = function(layer_num){
  return(2 ^ layer_num)
}

pascal_layer = function(layer_num){
  if(layer_num == 1){
    return(c(1))
  } else if (layer_num == 2){
    return(c(1,1))
  } else {
    beginning = c(1,1)
    to_return = c(1)
    for(x in 3:layer_num){
      for(y in 1:(length(beginning) - 1)){
        to_add = beginning[y] + beginning[y+1]
        to_return = c(to_return,to_add)
      }
      to_return = c(to_return,1)
      beginning = to_return
      to_return = c(1)
    }
    return(beginning)
  }
}

print(pascal_layer(10)) # 1 9 36 84 126 126 84 36 9 1