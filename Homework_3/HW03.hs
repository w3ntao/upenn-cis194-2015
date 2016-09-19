module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend base key val = \x -> if x == key
                                then val
                                else base x

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _     (Val v) = v
evalE state (Var x) = state x
evalE state (Op lExp binOp rExp) = trans binOp (evalE state lExp) (evalE state rExp)
                                   where trans Plus  = (+)
                                         trans Minus = (-)
                                         trans Times = (*)
                                         trans Divide = div
                                         trans Gt  = (boolToInt .) . (>)
                                         trans Ge  = (boolToInt .) . (>=)
                                         trans Lt  = (boolToInt .) . (<)
                                         trans Le  = (boolToInt .) . (<=)
                                         trans Eql = (boolToInt .) . (==)
                                         boolToInt False = 0
                                         boolToInt _     = 1

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                   deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign x exp) = DAssign x exp
desugar (Incr x)       = DAssign x (Op (Var x) Plus (Val 1))
desugar (If cond thenS elseS) = DIf cond (desugar thenS) (desugar elseS)
desugar (While cond body)            = DWhile cond (desugar body)
desugar (For init cond loopInc body) = DSequence (desugar init) (DWhile cond (DSequence (desugar body) (desugar loopInc)))
desugar (Sequence s0 s1) = DSequence (desugar s0) (desugar s1)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple base (DAssign x exp) = extend base x (evalE base exp)
evalSimple base (DIf cond thenS elseS) = evalSimple base (if not (evalE base cond == 0)
                                                              then thenS
                                                              else elseS)
evalSimple base (DWhile cond body) = if evalE base cond == 0
                                         then base
                                         else evalSimple (evalSimple base body) (DWhile cond body)
evalSimple base (DSequence s0 s1) = evalSimple (evalSimple base s0) s1
evalSimple base DSkip = base

run :: State -> Statement -> State
run base statement = evalSimple base (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
