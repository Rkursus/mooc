ylesanne = "Ülesanne 2.02.1"
yl = 2

# Try to evaluate submission exercise
read.commands = (eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n'))))

# If error in reading file, then exit function. Else run tests
if(class(read.file)=="try-error") {
  
  message(paste0("VIGA: Ei suutnud käivitada ", ylesanne, " koodi!"))
  return(NULL)
  break
  
} else {
  
  test_that(ylesanne, 
            {
              # Test if exercise code runs without errors
              expect_silent(tryCatch(eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n'))), finally = print("Viga")))
              
              # Evaluate submission exercise
              eval(parse(text = paste(tmp_parts[[yl]], collapse = '\n')))
              
              expect_equal(object = w,
                           expected = 3,
                           info = paste0(ylesanne, ": muutujale 'w' on antud vale väärtus"))
              
              expect_equal(object = z,
                           expected = 8,
                           info = paste0(ylesanne, ": muutuja 'z' on valesti arvutatud"))
              
              expect_true(length(grep("^z$", tmp_parts[[yl]])) > 0, 
                          info = paste0(ylesanne, ": muutujat 'z' pole välja prinditud"),
                          label = paste0(ylesanne, " muutuja 'z' väljatrüki kontroll"))
              
            })
}