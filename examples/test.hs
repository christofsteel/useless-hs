import Network.Useless
import qualified Data.Map as Map
import System.IO
import System.IO.Error
import Control.Exception (bracket, bracket_)

main = do
    useless <- initUseless
    register useless "/test" test
    register useless "/test2" test
    register useless "/" list
    register useless "/show" showMe
    startServer 8080 useless

test :: UselessSite
test u req = return  HTTPResponse{httpResStatus = 200, httpResHeader = Map.empty,
             httpResVersion = httpReqVersion req,
             httpResBody = testbody $ httpReqFile req}

testbody name = "<!DOCTYPE html><html>    <head>        <title>Testdingen</title>    </head>    <body>        <h1>Der Seitenname ist " ++ name ++" </h1>    </body></html>"

showMe :: UselessSite
showMe useless _ = do
        u <- getUselessData useless
        return $ createBasicHTTP $ show $ stringmap u

list :: UselessSite
list useless req = do
        u <- getUselessData useless
        addToUseless useless "Key" "Value"
        return HTTPResponse{httpResStatus=200, httpResHeader=Map.empty, httpResVersion = (httpReqVersion req), httpResBody=
        "<!DOCTYPE html>\n\r"++
        "<html>\n\r"++
        "\t<head>\n\r"++
        "\t\t<title>Uebersicht</title>\n\r"++
        "\t</head>\n\r"++
        "\t<body>\n\r"++
        "\t\t<ul>\n\r" ++
        genList (Map.keys $ sites u) ++
  "\t\t</ul>\n\r" ++ "\t</body>\n\r" ++ "</html>"}

genList :: [String] -> String
genList
  = foldl
      (\ acc x ->
         acc ++ "\t\t\t<li><a href=" ++ x ++ ">" ++ x ++ "</a></li>\n\r")
      ""

