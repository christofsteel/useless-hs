import Network.Useless
import Maybe

main = do
       theServer <- initUseless
       register theServer "/" showMe
       register theServer "/change" testadd
       startServer 8080 theServer
       
showMe :: UselessSite
showMe useless request = do
       val <- requestFromUseless useless "ThisSite"
       return $ createBasicHTTP $ fromMaybe "Not Visited" val

testadd :: UselessSite
testadd useless request = do
 	addToUseless useless "ThisSite" "Visited" 
	return $ createBasicHTTP $ "Nichts zu sehen"
