module Useless (
HTTPRequest (HTTPRequest, httpReqMethod, httpReqVersion, httpReqFile, httpReqHeader, httpReqQueries),
HTTPResponse (HTTPResponse, httpResStatus, httpResHeader, httpResBody),
initUseless,
register,
createErrorResponse,
startServer
) where

import Network
import System.IO
import System.IO.Error
import Control.Exception (bracket, bracket_)
import Control.Concurrent
import System
import System.Directory
import Data.List
import qualified Data.Map as Map
import System.FilePath.Posix
import System.Process

-- | initUseless initializes a Useless server layout.
initUseless :: IO (Map.Map String (HTTPRequest -> IO HTTPResponse))
initUseless = return Map.empty

{- |
register adds resources to your a server layout, and returns a new server layout.
The given Function is called, when the resource is requested. It gets a HTTPRequest and has to respond with a HTTPResponse
e.g:
  server = do 
  	layout <- initUseless
  	layout <- register "/test" someFunction layout -}
register :: String -> (HTTPRequest -> IO HTTPResponse) -> Map.Map String (HTTPRequest -> IO HTTPResponse) -> IO (Map.Map String (HTTPRequest -> IO HTTPResponse))
register site f sites = return $ Map.insert site f sites

-- | startServer starts the server at a specific Port
startServer :: Integer -> Map.Map String (HTTPRequest -> IO HTTPResponse) -> IO ()
startServer port sites = withSocketsDo $ do
        putStrLn $ "Starting Server on " ++ show port
	socket <- listenOn $ PortNumber $ fromIntegral port
	mainloop socket sites

mainloop :: Socket -> Map.Map String (HTTPRequest -> IO HTTPResponse) -> IO ()
mainloop socket sites = do
	(handle, hostname, portnr) <- accept socket
	hSetBuffering handle NoBuffering
	forkIO $ bearbeite handle sites
	mainloop socket sites

bearbeite :: Handle -> Map.Map String (HTTPRequest -> IO HTTPResponse) -> IO ()
bearbeite handle sites = do
	eitherHttpRequest <- readRequest handle
	(site, httpRequest) <- case eitherHttpRequest of
	 Left i -> return (createErrorResponse i, HTTPRequest {httpReqMethod = "", httpReqVersion = "", httpReqFile = "", httpReqQueries = [], httpReqHeader = []})
	 Right httpRequest -> do
	  putStrLn $ "Serving File: " ++ httpReqFile httpRequest
          return (Map.findWithDefault (createErrorResponse 404) (httpReqFile httpRequest) sites, httpRequest)
        httpResponse <- createResponse httpRequest site
        sendResponse handle httpResponse

sendResponse :: Handle -> HTTPResponse -> IO ()
sendResponse h res = do
    hPutStr h $ createStatusLine $ httpResStatus res
    hPutStr h "\n\r"
    hPutStr h $ httpResBody res
    hFlush h
    hClose h

createStatusLine :: Integer -> String
createStatusLine n = "HTTP/1.0 "++ createStatusLine' n ++"\n\r"

createStatusLine' :: Integer -> String
createStatusLine' 200 = "200 OK"
createStatusLine' 400 = "400 BAD REQUEST"
createStatusLine' 403 = "403 FORBIDDEN"
createStatusLine' 404 = "404 NOT FOUND"
createStatusLine' 500 = "500 INTERNAL SERVER ERROR"
createStatusLine' 501 = "501 NOT IMPLEMENTED"
createStatusLine' _ = "418 I'M A TEAPOT"

{- |
'createErrorResponse' errorno returns a Function, that gets a HTTPRequest and answers with a HTTPResponse, fitting the errorno
e.g:
  	layout <- register "/forbidden" (createErrorResponse 403) layout -}
createErrorResponse :: Integer -> HTTPRequest -> IO HTTPResponse
createErrorResponse n _ = do
    putStrLn $ "Error: " ++ show n
    return HTTPResponse { httpResStatus = n, httpResHeader = [], httpResBody = statushtml n}

createResponse :: HTTPRequest -> (HTTPRequest -> IO HTTPResponse)  -> IO HTTPResponse 
createResponse request f =  f request

{- |
A HTTPRequest consits of: 
	* the Method (httpReqMethod)
		e.g: GET, POST etc
	* the HTTP version (httpReqVersion)
		e.g: HTTP/1.1, HTTP/1.0
	* the HTTP URI (httpReqFile) !Currently including all queries!
		e.g: "/test?q=Test+Thing", "/", "/index.html"
	* the Queries (httpReqQueries) !Currently unsupported!
	* the HTTP headers (httpReqHeader)
		e.g: [("host:", "localhost"), ("Foo:", "Bar,Baz")] -}
data HTTPRequest = HTTPRequest  { httpReqMethod 	:: String
				, httpReqVersion 	:: String
				, httpReqFile		:: String
				, httpReqQueries	:: [(String, String)]
				, httpReqHeader	:: [(String,String)]
				}

{- | A HTTPResponse consits of
  	the Statuscode (httpResStatus)
 		e.g: 404, 403, 418, 200, 501
 	the HTTP headers (httpResHeader)
 		e.g: [("Location:", "http:/localhost/"), ("Content-Type:", "text/html")]
 	the HTTP Body (httpResBody)
 		e.g: "<!DOCTYPE html><html><body>Hello World</body></html>" -}
data HTTPResponse = HTTPResponse	{ httpResStatus	:: Integer
					, httpResHeader :: [(String,String)]
					, httpResBody :: String
					}

statushtml n = "<!DOCTYPE html>\n\r<html>\n\r\t<head>\n\r\t\t<title>"++ createStatusLine' n ++"</title>\n\r\t</head>\n\r\t<body>\n\r\t\t<img alt=\"" ++ createStatusLine' n++ "\" src=\"http://httpcats.herokuapp.com/" ++ show n ++ "\"/>\n\r\t</body>\n\r</html>"

readRequest :: Handle -> IO (Either Integer HTTPRequest)
readRequest handle = do
	requestline <- hGetLine handle
        header <- getHeader handle
	return $ mkRequest header $ words requestline

mkRequest :: [(String,String)] -> [String] -> Either Integer HTTPRequest
mkRequest h (m:f:v:xs) = Right HTTPRequest {httpReqMethod = m, httpReqFile = f, httpReqVersion = v, httpReqHeader = h, httpReqQueries=[]}
mkRequest h xs = Left 400
--HTTPRequest {httpReqMethod = "FAIL", httpReqFile=[], httpReqVersion=[], httpReqHeader=[], httpReqQueries=[]}

getHeader :: Handle -> IO [(String, String)]
getHeader h = do
    tryheaderline <- try $ hGetLine h
    case tryheaderline of
        Left e -> return []
        Right headerline  ->             
            if length headerline == 1 then return []
             else do
              let header = mkHeader $ words headerline
              headers <- getHeader h
              return $ header:headers

mkHeader :: [String] -> (String, String)
mkHeader (l:r) = (l,unwords r)
mkHeader _ = ("","")
