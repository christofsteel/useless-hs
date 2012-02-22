import Useless
import IO

main = do
    useless <- initUseless
    useless <- register "/test" test useless
    startServer 8080 useless

test :: HTTPRequest -> IO HTTPResponse
test req = return $ HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody = testbody $ httpReqFile req}

testbody name = "<!DOCTYPE html><html>    <head>        <title>Testdingen</title>    </head>    <body>        <h1>Der Seitenname ist " ++ name ++" </h1>    </body></html>"
