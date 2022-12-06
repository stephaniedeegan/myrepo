convertDist <- function(x,startUnit,endUnit){
  # Converting Kilometers
  if(startUnit == "kilometers"|startUnit =="kilometer"|startUnit=="km")
  {
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m" )
    {
      m <- x*1000
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x*100000
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*1000000
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/1.609344
      return(mi)
    }
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x*39370.1
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x*3280.84
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x*10936.13298
      return(yard)
    }
  }
  
  if(startUnit == "meters"|startUnit =="meter"|startUnit=="m")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/1000
      return(km)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x/100000
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x/1000000
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/1609.344
      return(mi)
    }
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x/0.0254
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x*3.280839895
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x/0.9144
      return(yard)
    }
  }
  # Converting centimeters
  if(startUnit == "centimeters"|startUnit =="centimeter"|startUnit=="cm")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/100000
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x/100
      return(m)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*10
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/160934.4
      return(mi)
    }
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x*2.54
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x/30.48
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x/91.44
      return(yard)
    }
  }
  # Converting millimeters
  if(startUnit == "millimeters"|startUnit =="millimeter"|startUnit=="mm")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/1000000
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x/1000
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x/10
      return(cm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/1609344
      return(mi)
    }
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x/25.4
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x/304.8
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x/914.4
      return(yard)
    }
  } 
  # Converting miles
  if(startUnit == "miles"|startUnit =="mile"|startUnit=="mi")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x*1.60934
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x*1609.344
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x*160934.4
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*1609344
      return(mm)
    }
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x*63360
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x*5280
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x*1760
      return(yard)
    }
  }    
  # Converting inches
  if(startUnit == "inches"|startUnit =="inch"|startUnit=="in")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/39370.1
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x*0.0254
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x* 2.54
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*25.4
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/63360
      return(mi)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit == "ft")
    {
      feet <- x/ 12
      return(feet)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x/36
      return(yard)
    }
  }
  # Converting feet
  if(startUnit == "feet"|startUnit =="foot"|startUnit=="ft")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/3280.84
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x*0.3048
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x* 30.48
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*304.8
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/5280
      return(mi)
    }
    
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x*12
      return(inch)
    }
    
    if(endUnit == "yards"|endUnit == "yard"|endUnit== "yd")
    {
      yard <- x/3
      return(yard)
    }
  }
  # Converting yards
  if(startUnit == "yards"|startUnit =="yard"|startUnit=="yd")
  {
    if(endUnit == "kilometers"|endUnit == "kilometer"|endUnit == "km" )
    {
      km <- x/3280.84
      return(km)
    }
    if(endUnit == "meters"|endUnit == "meter"|endUnit == "m")
    {
      m <- x*0.0009144
      return(m)
    }
    if(endUnit == "centimeters"|endUnit == "centimeter"|endUnit == "cm")
    {
      cm <- x*91.44
      return(cm)
    }
    if(endUnit == "millimeters"|endUnit == "millimeter"|endUnit == "mm")
    {
      mm <- x*914.4
      return(mm)
    }
    if(endUnit == "miles"|endUnit == "mile"|endUnit == "mi")
    {
      mi <- x/1760
      return(mi)
    }
    
    if(endUnit == "inches"|endUnit == "inch"|endUnit == "in")
    {
      inch <- x*36
      return(inch)
    }
    
    if(endUnit == "feet"|endUnit == "foot"|endUnit== "ft")
    {
      feet <- x*3
      return(feet)
    }
  }
  warning("Unit is not valid")
}

convertDist(100,"ft","gm")
