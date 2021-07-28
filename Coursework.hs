

-------------------------
-------- PART A --------- 
-------------------------

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  -- deriving Show

instance Show Term where
 show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1

numeral :: Int -> Term
numeral i = (Lambda "f") (Lambda "x" (who i))
  where
   who i
    | i <= 0 =  Variable "x"
    | otherwise = Apply (Variable "f") (who (i-1))



-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


------------------------- Assignment 2

variables :: [Var]
variables =   map(:[]) ['a'..'z'] ++[j: show i | i <-[1..],j<-['a'..'z']]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs [] = xs
filterVariables xs (y:ys) = filter (/=y) (filterVariables xs ys)

fresh :: [Var] -> Var
fresh xs = head (filterVariables (variables) xs)

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x m) = merge [x] (used m)
used (Apply  n m) = merge (used m) (used n)


------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
 | z == x = Variable y
 | otherwise = Variable z
rename x y (Lambda z n)
 | z == x = Lambda z n
 | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
 | y == x =  n
 |  otherwise = Variable y
substitute x n (Lambda y m)
 | y == x = Lambda y m
 | otherwise = Lambda z (substitute x n (rename y z m))
  where z = fresh (used m `merge` used n `merge` ["x","y"])
substitute x n (Apply m1 m2) = Apply (substitute x n m1) (substitute x n m2)


------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda x m) n) = [substitute x n m] ++ 
 [Apply (Lambda x m) n'| n'<-beta n] ++ 
 [Apply (Lambda x m') n| m'<-beta m]
beta (Variable _) = []
beta (Lambda x m) = [Lambda x m'|m'<-beta m]
beta (Apply m n) = [Apply m' n| m'<-beta m] ++ [Apply m n'| n'<-beta n]

normalize :: Term -> Term
normalize x
 | length (oki x) == 0 = x
 | otherwise = normalize (head (oki x))
 where oki x = beta x


run :: Term -> IO ()
run x = do
 print x
 let xs = beta x 
 if null xs
  then return()
 else do
  run (head xs)


 
-------------------------

suc    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Apply (Variable "n") (Variable "f")) (Variable "x")))))
add    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Variable "f")) (Apply (Apply (Variable "n") (Variable "f")) (Variable "x"))))))
mul    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Apply (Variable "n") (Variable "f"))) (Variable "x")))))
dec    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Apply (Variable "n") (Lambda "g" (Lambda "h" (Apply (Variable "h") (Apply (Variable "g") (Variable "f")))))) (Lambda "u" (Variable "x"))) (Lambda "x" (Variable "x")))))
minus  = Lambda "n" (Lambda "m" (Apply (Apply (Variable "m") dec) (Variable "n")))


-------------------------
-------- PART B --------- 
-------------------------

------------------------- Assignment 5

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = filter (/=x) (free n)
free (Apply  n m) = uniq (merge (free n) (free m))

abstractions :: Term -> [Var] -> Term
abstractions x [] = x
abstractions x (y:ys) = Lambda y (abstractions x ys)

applications :: Term -> [Term] -> Term
applications x [] = x
applications x (y:ys) = applications (someting x y) ys
 where someting x y = Apply x y

lift :: Term -> Term
lift n = applications (abstractions n (free n)) (map (Variable) (free n))

super :: Term -> Term
super (Variable x) = Variable x
super (Apply n m) = Apply (super n) (super m)
super (Lambda x n) = lift (Lambda x (find n))
 where 
  find (Lambda x n') = Lambda x (find n')
  find (Apply n' m') = Apply (super n') (super m')
  find (Variable x) = super (Variable x)


------------------------- Assignment 6

data Expr = V Var | A Expr Expr

toTerm :: Expr -> Term
toTerm (V x) = Variable x
toTerm (A m n) = Apply (toTerm m) (toTerm n)

instance Show Expr where
  show = show . toTerm

type Inst = (Var,[Var],Expr)

data Prog = Prog [Inst]


instance Show Prog where
  show (Prog ls) = unlines (map showInst ks)
    where
      ks = map showParts ls
      n  = maximum (map (length . fst) ks)
      showParts (x,xs,e) = (x ++ " " ++ unwords xs , show e)
      showInst (s,t) = take n (s ++ repeat ' ') ++ " = " ++ t
      
names = ['$':show i | i <- [1..] ]

-------------------------

stripAbs :: Term -> ([Var],Term)
stripAbs m = (notfree m, find m)
 where
  find (Lambda x n') = find n'
  find (Apply n' m') = Apply (n') (m')
  find (Variable x) = Variable x
  
  notfree (Lambda x n') = [x] ++ notfree n'
  notfree (Apply n' m') = []
  notfree (Variable x) = []

takeAbs :: Term -> [Term]
takeAbs (Variable x) = []
takeAbs (Lambda x m) = [Lambda x m]
takeAbs (Apply m n) = (takeAbs m) ++ (takeAbs n)


--error checking is applied to make sure there are enough fresh variables to replace
toExpr :: [Var] -> Term -> Expr
toExpr _ (Variable x) = V x
toExpr [] (Lambda x m) = error "Not enough free variables"
toExpr (y:ys) (Lambda x m) = V y
toExpr ys (Apply m n) = A (toExpr (fst (func ys m)) m) (toExpr (snd (func ys m)) n)
 where func ys x = splitAt (length (takeAbs x)) ys

toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst xs (i,m) = ((step1 xs i m),(step2 xs m),snd (step3 xs m))
 where 
  step1 xs i m = (i,fst (stripAbs m),toExpr xs (snd (stripAbs m)))
   
   --2) literally use toExpr to get the variables and zip them with the list of variables
  step2 xs m = zip xs (takeAbs (snd (stripAbs m)))
  
  --3) now I just need the remainder of the list 
  step3 xs m = splitAt (length (takeAbs (snd (stripAbs m)))) xs

prog :: Term -> Prog
prog m = Prog (aux names ([("$main", super m)]))
  where
    aux :: [Var] -> [(Var,Term)] -> [Inst]
    aux _ [] = []
    aux xs ((x,y):rest) = [get1st (toInst xs (x,y))] ++ aux (snd (getArray xs (x,y))) (rest ++ get2nd (toInst xs (x,y)))
    get1st (a,_,_) = a
    get2nd (_,a,_) = a
    getArray xs (x,y) = splitAt (length (get2nd (toInst xs (x,y)))) xs



example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")

------------------------- Assignment 7

sub :: [(Var,Expr)] -> Expr -> Expr
sub [] (V z) = (V z)
sub ((x,y):rest) (V z)
 | z == x = y
 | otherwise = sub (rest) (V z)
sub [] (A m n) = A m n
sub ((x,y):rest) (A m n) = A (sub ((x,y):rest) m) (sub ((x,y):rest) n) 

--for the error expression here it generates user error.
step :: [Inst] -> [Expr] -> IO [Expr]
step i [] = return ([])
step i ((A m n):xs) = return (m:n:xs)
step i ((V m):xs) = do 
  if  rs /= []
    then
    if length xs >= length ys
     then return (applySub m: remaining xs)
    else do
      error ("step: insufficient arguments on stack")
  else do 
    putStr (m++" ")
    return (xs)
   where 
    rs = [q | (q,r,s) <- i,m == q]
    ys = [r | (q,r,s) <- i,m == q] !! 0
    ws = [s | (q,r,s) <- i,m == q] !! 0
    buildSub  = zip (ys) (take (length ys) xs)
    applySub m = sub (buildSub) ws
    remaining xs = drop (length ys) xs

--this is the driver function for the IO
function :: [Inst] -> [Expr] -> IO ()
function i [] =  putStr ("\n")
function i ((A m n):xs) = function i (m:n:xs)
function i ((V m):xs) = do 
  if  rs /= []
    then
    if length xs >= length ys
     then function i (applySub m: remaining xs)
     else do
      error ("step: insufficient arguments on stack")
  else do 
    putStr (m++" ")
    function i xs
   where 
    rs = [q | (q,r,s) <- i,m == q]
    ys = [r | (q,r,s) <- i,m == q] !! 0
    ws = [s | (q,r,s) <- i,m == q] !! 0
    buildSub  = zip (ys) (take (length ys) xs)
    applySub m = sub (buildSub) ws
    remaining xs = drop (length ys) xs
      

supernormalize :: Term -> IO ()
supernormalize x = function (ughh oki) [V "$main"]
 where 
  oki = Prog (aux names ([("$main", super x)]))
  --pattern  matching saved me. Could have rewritten a func.
  ughh (Prog haha) = haha
  aux _ [] = []
  aux xs ((x,y):rest) = [get1st (toInst xs (x,y))] ++ aux (snd (getArray xs (x,y))) (rest ++ get2nd (toInst xs (x,y)))
  get1st (a,_,_) = a
  get2nd (_,a,_) = a
  getArray xs (x,y) = splitAt (length (get2nd (toInst xs (x,y)))) xs





  

  