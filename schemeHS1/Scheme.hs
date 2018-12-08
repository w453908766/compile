

import Eval
import Var
import PrimProc
import Control.Monad.State
import qualified Data.Map as Map




code = "(define a 4) (+ a 1)" :: String
varcode = parseScheme code
a = evalState (evalSequence (toList varcode)) [primProcMap]
