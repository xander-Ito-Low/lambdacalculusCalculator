lastElem::[a]->a
lastElem (x:[]) = x
lastElem (x:xs) = lastElem xs

lastbut1Elem::[a]->a
lastbut1Elem (x:y:[]) = x
lastbut1Elem (x:xs) = lastbut1Elem xs

elemAt::[a]->Integer->a
elemAt x xs = countdown x xs
 where 
  countdown (x:xs) 1 = x 
  countdown [] n = undefined
  countdown (x:xs) n = countdown (xs) (n-1)

count::[a]->Integer->Integer
count [] acc = acc 
count (x:xs) acc =  count xs (1+acc)