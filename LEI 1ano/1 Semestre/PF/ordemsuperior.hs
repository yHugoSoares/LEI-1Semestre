{-
map (*2) [1,2,3] = [2,4,6]
^aplica a funcao f a cada elemento

filter (>5) [1,10,20] = [10,20]
^aplica a funcao f a cada elemento, ou seja neste caso cada elemento teria de ser >5 para ficar na lista

foldl (/) 64 [4,2,4]
          ^^
       acumulador
= foldl (/) (/ 64 4) [2,4]
               64/4
= foldl (/) (/ 16 2) [4]
               16/2
= foldl (/) (/ 8 4) []
= 2
-}