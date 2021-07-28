data Instr = Push Int
 | Add
 | Sub
 | Mul
 deriving show


 type stack = [Int]
 type Prog = [Instr]

 exec :: Instr -> stack -> stack
 exec (Push x) st = x:st
 exec Add (x:y:st) = (x+y):st
 exec Sub (x:y:st) = (x-y):st
 exec Sub (x:y:st) = (x*y):st
 exec _ _ = undefined

 run :: Prog -> Stack -> Stack
 run [] st = st
 run (i:is) st = run is (exec i st)