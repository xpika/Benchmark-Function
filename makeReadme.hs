import Process


insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })

main = do 
       res <- run "runhaskell TestList.hs" 
       temp <- readFile "Readme.md.template"
       writeFile "Readme.md" (unlines $ insertAt 1 res (lines temp))


