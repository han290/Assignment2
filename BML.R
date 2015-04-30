

createBMLGrid = 
  function(r=100,c=99, ncars = c(100,100))
  
  {
    if (r*c < sum(ncars)) stop('number of cars out of bound')
    
    Grids = matrix('',ncol = c, nrow = r)
  
    Grids[sample(1:(r*c),ncars[1])] = 'red'
  
    Grids[sample(1:(r*c),ncars[2])] ='blue'
    
    Grids = structure(Grids,class = 'BMLGrid')
  
    Grids
    }


plot.BMLGrid = 
  function(x) 
    {
       z = matrix(match(x, c('','red', 'blue')), nrow(x), ncol(x))

       image(z, col = c('white','red','blue'))
       
       box()
  }


summary.BML = 
  function(x){
    
    nrows = dim(x)[1]
    
    ncols = dim(x)[2]
    
    nredcars = cumsum( x %in% 'red')[nrows*ncols]
    
    nbluecars = cumsum( x %in% 'blue')[nrows*ncols] 
    
    RedBlocked = NumCarsBlocked(x, colors = 'red')
    
    BlueBlocked = NumCarsBlocked(x, colors = 'blue')
    
    list(dimention = list(nrows=nrows,ncols=ncols), numRedCars = nredcars,
         
         numBlueCars = nbluecars, numBlockedReds = RedBlocked,
         
         numBlockedBlues = BlueBlocked)
    
  }


NumCarsBlocked = function(x, colors = 'red'){
  
  cars = getLocations(x)
  
  if(colors == 'red'){
    
    l = RedCarLocs(p = cars, x) 
    
  } else {
    
    l = BlueCarLocs(p = cars, x)   
  }
  
  nextLocs = cbind(l$nextRow, l$nextCol)
  
  w = x[nextLocs]  != ""
  
  n = cumsum(w)[nrow(nextLocs)]
  
  n
}


TotalCars = function(x, color = 'red'){
  
  nrows = nrow(x)
  
  ncols = ncol(x)
  
  if(color =='red'){
    
    TotalCars = cumsum( x %in% color)[nrows*ncols]
  
  } else {
    
    TotalCars = cumsum( x %in% color)[nrows*ncols]
    
  }
  
  TotalCars
  
}


getLocations =
  function(g)
  {
    i = row(g)[g != ""]
   
    j = col(g)[g != ""]
   
    pos = cbind(i, j)
   
    colors = g[pos]
  
    p = data.frame(i=i,j=j,colors=colors)
    
    p
  }


BlueCarLocs = function(p,g)
  {
    l = p$colors %in% 'blue'
   
    rows = p[l, 1]
   
    cols = p[l, 2]
    
    nextRows = rows - 1L
    
    nextRows[ nextRows == 0 ] = nrow(g)
    
    nextCols = cols
    
    nextLocs_list = list(nextRow = nextRows, nextCol = nextCols, 
                         
                         rows = rows, cols = cols)
    
    nextLocs_list 
}


RedCarLocs =
  function(p,g)
    {
      l = p$colors %in% 'red'
      
      rows = p[l, 1]
      
      cols = p[l, 2]
      
      nextRows = rows
        
      nextCols = cols + 1L
        
      nextCols[ nextCols > ncol(g) ]  = 1L 
      
      nextLocs_list = list(nextRow = nextRows, nextCol = nextCols, 
                           
                           rows = rows, cols = cols) 
      
      nextLocs_list
  }


moveCars = 
  function(l, g, color = 'blue')
  {
    nextLocs = cbind(l$nextRow, l$nextCol)
    
    w = g[nextLocs]  == ""
    
    g[ nextLocs[w, , drop = FALSE] ] = color
    
    g[ cbind(l$rows, l$cols)[w, , drop = FALSE] ] = ""
    
    list(Grids = g, w = w)
  }


NextLocs = 
  function(l, p, s, color = 'blue')
    {
      w = s$w
      
      g = s$Grids
      
      if(color == 'blue')
      {
        l$nextRow[!w] = l$nextRow[!w]+1L
      
        l$nextRow[l$nextRow > nrow(g)] = 1L
      
      } else {
        
        l$nextCol[!w] = l$nextCol[!w] -1L
      
        l$nextCol[l$nextCol == 0] = ncol(g)
      }
        
      p[p$colors == color, 'i'] = l$nextRow  
        
      p[p$colors == color, 'j'] = l$nextCol
      
      p
  }



BML = 
  function(g, numSteps = 3)
  {
    cars = getLocations(g) 
    
      for(i in 1:numSteps)
      {
        if(i%%2 == 0)
        {  
          locs_list =  RedCarLocs(p=cars, g)
           
          g = moveCars(l=locs_list, g, color = 'red')
          
          cars = NextLocs(l=locs_list, p=cars,color = 'red',s = g)     
          
      } else {
        
        locs_list = BlueCarLocs(p = cars, g)
        
        g = moveCars(l = locs_list,g, color = 'blue')
        
        cars = NextLocs(l = locs_list,p=cars, color = 'blue', s = g)
        
      }
      
      g = g$Grids
      
    }
    g
  }
 

BML_Process = function(x,Steps, color = 'red' )
  {
    g = BML(g = x, numSteps = Steps)
  
    nb = NumCarsBlocked(g, color)
  
    tot = TotalCars(x = g, color)
  
    nm = tot - nb
  
    v = nm/tot
    
    list(NumMove = nm, NumBlocked = nb, velocity =  v)
  
}



velocity = 
  function(x, numSteps)
    {
      sapply(1:numSteps,function(x)
        {
          if(x%%2 == 0){
            BML_Process(x = g,Steps = x, color = 'red' )
            
        } else {
            BML_Process(x = g,Steps = x, color = 'blue' )
        }
        })
  }






