Warning message:
In file(): kan ikke åbne fil 'OpenSky_Saves/cirkelfly_fundet.csv': No such file or directory

Warning message:
In file(): kan ikke åbne fil 'OpenSky_Saves/cirkelfly_fundet.csv': No such file or directory

Warning message:
In ifelse(append, "a", "w")(): kan ikke åbne fil 'OpenSky_Saves/cirkelfly_fundet.csv': No such file or directory


Fejl i file(file, ifelse(append, "a", "w")) : kan ikke åbne forbindelsen Kald:
write.csv -> eval.parent -> eval -> eval -> <Anonymous> -> file
Traceback:
6: file(file, ifelse(append, "a", "w"))
5: utils::write.table(alleFund, fundfil, row.names = FALSE, col.names = TRUE, 
       sep = ",", dec = ".", qmethod = "double")
4: eval(expr, p)
3: eval(expr, p)
2: eval.parent(Call)
1: write.csv(alleFund, fundfil, row.names = FALSE)

