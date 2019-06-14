import Parser
import DataStructures
import Data.Either

stepEval :: Expr -> Int -> [Int] -> IO()
stepEval EndCommand _ _= return ()
stepEval (BracketCommand withinbr expr2) ptr  arr  = do 
                                    evalBrackets withinbr withinbr ptr arr
                                    stepEval expr2 ptrinside arrinside
                                        where (ptrinside,arrinside) = evalBracketsptr withinbr withinbr ptr arr     
stepEval (IncrementP expr ) ptr arr  = stepEval expr (ptr+1) arr 
stepEval (IncrementV expr ) ptr  arr  =  stepEval expr ptr   (editArrplus ptr arr )
stepEval (DecrementP expr ) ptr arr  = stepEval expr (ptr-1) arr 
stepEval (DecrementV expr) ptr arr  = stepEval expr ptr   (editArrminus ptr arr )
stepEval (InputV int expr  ) ptr arr = stepEval expr ptr   (editArrInput ptr arr int) 
stepEval (OutputV expr ) ptr arr= do 
                               putStrLn (show( arr!!ptr))
                               stepEval expr ptr arr


editArrplus ptr arr = take ptr arr ++ [(arr!!ptr)+1] ++drop (ptr+1) arr 

editArrInput ptr arr int = take ptr arr ++ [int] ++drop (ptr+1) arr 

editArrminus ptr arr = take ptr arr ++ [(arr!!ptr)-1] ++drop (ptr+1) arr   
  

startEval  expr = stepEval expr 0 (replicate 10000 0) 


evalBrackets ::  Expr->Expr -> Int -> [Int] -> IO()
evalBrackets   exprKeep EndCommand ptr arr = if (arr!!ptr) ==0 then return () else evalBrackets  exprKeep exprKeep ptr arr
evalBrackets   exprKeep (BracketCommand withinbr expr2) ptr  arr  =  do 
                                    evalBrackets withinbr withinbr ptr arr
                                    evalBrackets exprKeep expr2 ptrinside arrinside
                                        where (ptrinside,arrinside) = evalBracketsptr withinbr withinbr ptr arr     
evalBrackets   exprKeep (IncrementP expr ) ptr arr  = evalBrackets  exprKeep expr  (ptr+1) arr 
evalBrackets   exprKeep (IncrementV expr ) ptr  arr  =  evalBrackets  exprKeep expr  ptr   (editArrplus ptr arr )
evalBrackets   exprKeep (DecrementP expr ) ptr arr  = evalBrackets  exprKeep expr (ptr-1) arr 
evalBrackets  exprKeep (DecrementV expr) ptr arr  = evalBrackets  exprKeep expr  ptr   (editArrminus ptr arr )
evalBrackets  exprKeep (InputV int expr  ) ptr arr = evalBrackets  exprKeep expr ptr   (editArrInput ptr arr int) 
evalBrackets  exprKeep (OutputV expr ) ptr arr= do 
                               putStrLn (show( arr!!ptr))
                               evalBrackets  exprKeep expr ptr arr
                               
                               
                               
                               
                               
evalBracketsptr ::  Expr->Expr -> Int -> [Int] -> (Int,[Int])
evalBracketsptr   exprKeep EndCommand ptr arr = if (arr!!ptr) ==0 then (ptr,arr)  else evalBracketsptr  exprKeep exprKeep ptr arr
evalBracketsptr   exprKeep (BracketCommand withinbr expr2) ptr  arr  = evalBracketsptr exprKeep expr2 ptrinside arrinside
                where (ptrinside,arrinside) = evalBracketsptr withinbr withinbr ptr arr                                    
evalBracketsptr   exprKeep (IncrementP expr ) ptr arr  = evalBracketsptr  exprKeep expr  (ptr+1) arr 
evalBracketsptr   exprKeep (IncrementV expr ) ptr  arr  =  evalBracketsptr  exprKeep expr  ptr   (editArrplus ptr arr )
evalBracketsptr   exprKeep (DecrementP expr ) ptr arr  = evalBracketsptr  exprKeep expr (ptr-1) arr 
evalBracketsptr  exprKeep (DecrementV expr) ptr arr  = evalBracketsptr  exprKeep expr  ptr   (editArrminus ptr arr )
evalBracketsptr  exprKeep (InputV int expr  ) ptr arr = evalBracketsptr  exprKeep expr ptr   (editArrInput ptr arr int) 
evalBracketsptr  exprKeep (OutputV expr ) ptr arr= evalBracketsptr  exprKeep expr ptr arr
                               
                               
                               
                               
parseAndEval str =    either (\x -> putStrLn "failed to parse" ) (startEval )  (parseSpecification str)
                                                      
                               
                               