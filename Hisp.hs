import qualified Data.Map as Map

data Val = Num Int
         | Sym String
         | List [Val]
         | Closure [String] Val Env
         | Prim String  deriving (Eq, Ord, Show)

type Env = [Map.Map String Val]

-- env
emptyEnv = [Map.empty] :: Env
extendEnv es = (Map.empty):es
lookupEnv n [] = Nothing
lookupEnv n (e:es) = if Map.member n e then Map.lookup n e else lookupEnv n es
insertEnv n v (e:es) = (Map.insert n v e):es
updateEnv [] [] env = env
updateEnv (p:ps) (v:vs) env = insertEnv p v (updateEnv ps vs env)
updateEnv _ _ _ = error "not enough params or values in updateEnv"

isNum (Num _) = True
isNum _ = False

isSym (Sym _) = True
isSym _ = False

isList (List (x:_)) = True
isList _ = False

isNil (List []) = True
isNil _ = False

isClosure (Closure _ _ _) = True
isClosure _ = False

evalIf code env = case code of
                    (cond:cons:alt:[]) -> if test (eval cond env) then 
                                             eval cons env 
                                          else 
                                             eval alt env
                    (cond:cons:[]) -> if test (eval cond env) then 
                                         eval cons env 
                                      else 
                                         (List [])
    where test = not . isNil

eval :: Val -> Env -> Val
eval (Sym x) env = case lookupEnv x env of
                      Just x -> x
                      Nothing -> error $ "unbound variable '" ++ x ++ "'"
eval (List ((Sym "lambda"):(List params):(List code):[])) env = Closure (map (\(Sym x) -> x) params) (List code) env
eval (List ((Sym "if"):rest)) env = evalIf rest env
eval (List ((Sym "quote"):x:[])) env = x
eval (List (op:rands)) env = apply (eval op env) $ map (flip eval $ env) rands
eval x env = x

apply :: Val -> [Val] -> Val
apply (Closure params code env) args = eval code (updateEnv params args (extendEnv env))
apply (Prim "cons") (a:(List d):[]) = List (a:d)
apply (Prim "cons") (a:b:[]) = List [a, b]
apply (Prim "cons") _ = error "invalid arguments to cons"
apply (Prim "car") ((List []):[]) = error "can't take the car of an empty list"
apply (Prim "car") ((List (a:_)):[]) = a
apply (Prim "cdr") ((List []):[]) = error "can't take the cdr of an empty list"
apply (Prim "cdr") ((List (_:d)):[]) = List d
apply (Prim "not") ((List []):[]) = Num 1
apply (Prim "not") (_:[]) = List []
apply (Prim "not") _ = error "too many args to not"
apply (Prim "+") (Num x:Num y:[]) = Num (x + y)
apply (Prim "+") _ = error "invalid arguments to +"
apply (Prim "-") (Num x:[]) = Num (- x)
apply (Prim "-") (Num x:Num y:[]) = Num (x - y)
apply (Prim "-") _ = error "invalid arguments to -"
apply (Prim "*") (Num x:Num y:[]) = Num (x * y)
apply (Prim "*") _ = error "invalid arguments to *"
apply (Prim "/") (Num x:Num 0:[]) = error "attempt to divide by 0"
apply (Prim "/") (Num x:Num y:[]) = Num (x `div` y)
apply (Prim "/") _ = error "invalid arguments to /"
apply (Prim "=") (a:b:[]) = if a == b then Num 1 else List []
apply (Prim _) _ = error "unknown primitive"


apply _ _ = error "don't know how to apply that"

-- List [List [Sym "lambda", List [Sym "x"], List [Sym "if", Sym "x", Num 1, Num 3]], Num 3)

initialEnv = [Map.fromList [ ("cons", Prim "cons")
                           , ("car", Prim "car")
                           , ("cdr", Prim "cdr")
                           , ("not", Prim "not")
                           , ("+", Prim "+")
                           , ("-", Prim "-")
                           , ("*", Prim "*")
                           , ("/", Prim "/")
                           , ("=", Prim "=")
                           ]]
