import Network.Useless
import qualified Data.Map as Map
import IO

main = do
    useless <- initUseless
    register "/test" test useless
    register "/test2" test useless
    register "/" list useless
    register "/show" showMe useless
    startServer 8080 useless

test :: UselessSite
test u req = return  HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody = testbody $ httpReqFile req}

testbody name = "<!DOCTYPE html><html>    <head>        <title>Testdingen</title>    </head>    <body>        <h1>Der Seitenname ist " ++ name ++" </h1>    </body></html>"

showMe :: UselessSite
showMe useless _ = do
	u <- getUselessData useless
	return $ createBasicHTTP $ show $ stringmap u

list :: UselessSite
list useless _ = do
	u <- getUselessData useless
	addToUseless "Key" "Value" useless
	return HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody=
	"<!DOCTYPE html>\n\r"++
	"<html>\n\r"++
	"\t<head>\n\r"++
	"\t\t<title>Uebersicht</title>\n\r"++
	"\t</head>\n\r"++
	"\t<body>\n\r"++
	"\t\t<ul>\n\r" ++
	(genList $ Map.keys $ sites u) ++
	"\t\t</ul>\n\r" ++
	"\t</body>\n\r"++
	"</html>"}

genList :: [String] -> String
genList links = foldl (\acc x -> acc ++ "\t\t\t<li><a href="++x++">"++x++"</a></li>\n\r") "" links

