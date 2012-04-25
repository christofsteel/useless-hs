import Network.Useless
import Data.Maybe

main = do
       theServer <- initUseless
       register theServer "/" showMe
       register theServer "/change" testadd
       startServer 18080 theServer
       
showMe :: UselessSite
showMe useless request = do
       val <- requestFromUseless useless "ThisSite"
       let liste = [0,3,5,2,5,67,32,192,14312]
       return $ createBasicHTTP $ (show $ qsort liste) ++ fromMaybe "Not Visited" val

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort kleinergl ++ [x] ++ qsort groesser
                   where
                      kleinergl = [y | y <- xs, y <= x]
                      groesser  = [y | y <- xs, y > x]

testadd :: UselessSite
testadd useless request = do
 	addToUseless useless "ThisSite" "Visited" 
	return $ createBasicHTTP $ "Nichts zu sehen"
