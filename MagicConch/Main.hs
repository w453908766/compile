

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as List


import SExp
import Var
import Sema
import CodeGen




--code = "(declare fact (-> Int Int)) (define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (declare g Int) (define g (fact 5))"
--code = "(define Int num 4)"

--code = "(define (Int Int Int) (add a b) (+ a b))"

--code = "(define (Int Int) (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
 
code = "(define (Int Int) (f a) (+ 1 a))"

main = do
  let varcode = toVar $ parseSExp $ lexSExp code
  let ast = makeModule varcode
  ass0 <- toASS ast
  putStr $ fromBS ass0
