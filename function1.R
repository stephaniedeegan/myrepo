convertMiles <- function(x,unit){
  if(unit == "kilometers"|unit =="kilometer"|unit=="km")
  {
    km <- x*1.60934
    return(km)
  }
  
  if(unit == "meters"|unit == "meter"| unit == "m")
  {
    meter <- x*1609.344
    return(meter)
  }
  
  if(unit == "centimeters"|unit == "centimeter"|unit == "cm")
    {
      cm <- x*160934.4
      return(cm)
    }
  
  if(unit == "millimeters"|unit == "millimeter"|unit == "mm")
  {
    mm <- x*1609344
    return(mm)
  }
  
  if(unit == "inches"|unit == "inch"|unit == "in")
  {
    inch <- x*63360
    return(inch)
  }
  
  if(unit == "feet"|unit == "foot"|unit == "ft")
  {
    feet <- x*5280
    return(feet)
  }
  
  if(unit == "yards"|unit == "yard"|unit == "yd")
  {
    yard <- x*1760
    p1 <- paste(x,"miles equals")
    p2 <- paste(yard,"yards")
    p3 <- c(yard,paste(p1,p2))
    return(yard)
  }
  
  warning("Unit is not valid")
}

convertMiles(3,"km")

