

zad1 <-  function(first, second)
{
  if(second != 0){
    if(first%%second==0){
      return ("jest podzielna")
    }
  }
  return ("Nie jest podzielna")
}


zad2 <- function(speedFirst, speedSecond){
  return ((speedFirst+speedSecond)/2)
}



zad3 <- function(file){
  data<-read.csv(file,header = TRUE,sep=";",dec=",")
  result = cor(data[['waga']], data[['wzrost']], method = "pearson")  
}

#Z linku jaki znalazłem w Internecie wynika, że jak wartość korelacji jest większa od 0.5 lub -0.5 to oznacza że jest duża.
#https://statistics.laerd.com/statistical-guides/pearson-correlation-coefficient-statistical-guide.php
#Yes, the following guidelines have been proposed:

#  Coefficient, r
#Strength of Association	Positive	Negative
#Small	  .1  to  .3	  -0.1 to -0.3
#Medium	  .3  to  .5	  -0.3 to -0.5
#Large	  .5  to  1.0	  -0.5 to -1.0

stworzDataFrame <- function(ile=1){
  print("Nazwy wolumn")
  headSplit <- strsplit(readline(), " ")
  header <-headSplit[[1]]
  countColumns<-length(header)
  df <- data.frame(matrix(vector(),nrow = ile,ncol= countColumns ))
  colnames(df) <- header
  for( i in 1 : ile){
    r=0
    while(r==0){
      print("zawartsc wiersza")
      rowSplit <- strsplit(readline(), " ")
      row <-rowSplit[[1]]
      if(length(row)==countColumns){
        df[i,] = row
        r=1
      }else{
        print(sprintf("ilesc elementów w wierszu musi byc równa %s",countColumns))
      }
    }

    }
  print(df)
  View(df)
}




liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=3){ 
  ResultVector <- c()
  for( i in 1 : DlaIluPlikow){
    ResultVector <- c(ResultVector, na.omit(read.csv(file.path(sciezka,list.files(sciezka)[i]))[ ,c(paste0("X",nazwaKolumny))]))
  }
  if(jakaFunkcja=="mean"){
    return(mean(ResultVector))
  }
  if(jakaFunkcja=="median"){
    return(median(ResultVector))
  }
  if(jakaFunkcja=="min"){
    return(min(ResultVector))
  }
  if(jakaFunkcja=="max"){
    return(max(ResultVector))
  }
  
  #print(ResultVector)
  #dfs <- data.frame(ResultVector)
  #View(dfs)
}





print(zad1(1, 3))
print(zad1(10, 5))
print(zad1(11, 3))
print(zad1(1, 3))

print(zad2(120, 90))

print(zad3("dane.csv"))


stworzDataFrame(2)

print(liczZplikow("C:/R1/R1/smogKrakow","169_pm25","mean",2))
print(liczZplikow("C:/R1/R1/smogKrakow","169_pm25","median",2))
print(liczZplikow("C:/R1/R1/smogKrakow","169_pm25","min",2))
print(liczZplikow("C:/R1/R1/smogKrakow","169_pm25","max",2))