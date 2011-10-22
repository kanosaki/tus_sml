
data B v = T | F v

hoge T _ = do 
    putStrLn "T"
    return 1
hoge F v = 
    do putStrLn v
       return 2

main = do putStrLn "foo"
          ret <- hoge T
          putStrLn $ show ret
