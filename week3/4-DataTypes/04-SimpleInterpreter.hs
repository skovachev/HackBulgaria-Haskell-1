import qualified Data.Map.Strict as Map

type Variable = String
type Value    = Int

-- statement with return value
data Expr = Var Variable
          | Val Value
          | Op  Expr Oper Expr
  deriving (Show, Eq)

data Oper = Plus | Lt
    deriving (Show, Eq)

-- no return value
data Statement = Assign Variable Expr 
               | For Statement Expr Statement Statement
               | Incr Variable
    deriving (Show, Eq)

type Variables = Map.Map Variable Value

data State = State [Statement] Variables
  deriving (Show, Eq)

-- Evaluate an expression
evaluateExpression :: Expr -> Variables -> Maybe Value
evaluateExpression (Val a) vars = Just a
evaluateExpression (Var a) vars = Map.lookup a vars
evaluateExpression (Op expr1 oper expr2) vars | oper == Plus = evaluateExpression expr1 vars + evaluateExpression expr2 vars
                                              | oper == Lt = if evaluateExpression expr1 vars < evaluateExpression expr2 vars then 1 else 0
                                              | otherwise = error "unsupported operator"

executeStatement :: Statement -> Variables -> Maybe Variables
executeStatement (Assign variable expr) vars = case evaluateExpression expr vars of 
                                               Just v -> Just $ Map.insert variable v vars
                                               _ -> Nothing
executeStatement (Incr variable) vars = case Map.lookup variable vars of 
                                             Just v -> Just $ Map.adjust (+1) variable vars
                                             _ -> Nothing

-- executeStatement (For statement expr statement2 statement3) vars = executeStatement
executeStatement _ _ = error "unsupported statement"


-- Execute the current statement and update the state
executeState :: State -> Maybe State
executeState (State (statement:xs) vars) = case executeStatement statement vars of 
                                           Just v -> executeState (State xs v)
                                           _ -> Nothing

executeState (State (x:_) vars) = case executeStatement x vars of 
                                  Just v -> Just $ State [] v
                                  _ -> Nothing


-- Assign "sum" (Val 0)
-- For (Assign "i" (Val 0))
--     (Op (Var "i") Lt (Val 10))
--     (Incr "i")
--     (Assign "sum" (Op (Var "sum") Plus (Var "i")))

-- state = State [ Assign "sum" (Val 0),
--     For (Assign "i" (Val 0))
--         (Op (Var "i") Lt (Val 10))
--         (Incr "i")
--         (Assign "sum" (Op (Var "sum") Plus (Var "i")))] Empty
    