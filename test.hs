import Useless
import qualified Data.Map as Map
import IO

main = do
    useless <- initUseless
    useless <- register "/test" test useless
    useless <- register "/test2" test useless
    useless <- register "/" (list useless) useless
    startServer 8080 useless

test :: HTTPRequest -> IO HTTPResponse
test req = return $ HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody = testbody $ httpReqFile req}

testbody name = "<!DOCTYPE html><html>    <head>        <title>Testdingen</title>    </head>    <body>        <h1>Der Seitenname ist " ++ name ++" </h1>    </body></html>"

list :: Map.Map String a -> HTTPRequest -> IO HTTPResponse
list useless _ = return $ HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody=
	"<!DOCTYPE html>\n\r"++
	"<html>\n\r"++
	"\t<head>\n\r"++
	"\t\t<title>Uebersicht</title>\n\r"++
	"\t</head>\n\r"++
	"\t<body>\n\r"++
	"\t\t<ul>\n\r" ++
	(genList $ Map.keys useless) ++
	"\t\t</ul>\n\r" ++
	"\t</body>\n\r"++
	"</html>"}

genList :: [String] -> String
genList links = foldl (\acc x -> acc ++ "\t\t\t<li><a href="++x++">"++x++"</a></li>\n\r") "" links

