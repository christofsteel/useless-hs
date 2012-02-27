-- | Useless is a Module for building Webapplications
module Network.Useless (
HTTPRequest (HTTPRequest, httpReqMethod, httpReqVersion, httpReqFile, httpReqHeader, httpReqQueries),
HTTPResponse (HTTPResponse, httpResStatus, httpResHeader, httpResBody),
initUseless,
register,
createBasicHTTP,
createErrorResponse,
addToUseless,
startServer,
UselessSite,
UselessData (UselessData, sites, stringmap),
getUselessData
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
import Control.Concurrent.MVar

{- | 
'UselessSite' is the type for your Useless functions.
> testsite :: UselessSite
> testsite useless request = return $ HTTPResponse {httpResStatus = 200, httpResHeader=[("foo:", "Bar,Baz")], httpResBody="Test123"}
-}
type UselessSite = Useless -> HTTPRequest -> IO HTTPResponse

{- |
'UselessData' consists of the sites, that can be served by your Server and a String Map, that can be used for Shared memory (Future versions will expand this)
-}
data UselessData = UselessData { sites :: Map.Map String UselessSite, stringmap :: Map.Map String String}
{- |
'Useless' is just a wrapper for 'UselessData', so that it can be used by all threads
-}
type Useless = MVar UselessData

{- |
'createBasicHTTP' creates a very Basic HTTP response from a given String
-}
createBasicHTTP :: String -> HTTPResponse
createBasicHTTP s = HTTPResponse{httpResStatus=200, httpResHeader=[], httpResBody=s}

{- |
'addToUseless' adds a Key Value Pair to a shared Useless object
> testadd :: UselessSite
> testadd useless request = do
> 	addToUseless "ThisSite" "Visited"
>	return $ createBasicHTTP $ .-}
addToUseless :: String -> String -> Useless -> IO ()
addToUseless k v useless = do
	u <- takeMVar useless
	putMVar useless $ UselessData {sites = sites u, stringmap = Map.insert k v (stringmap u)}

getUselessData :: Useless -> IO UselessData
getUselessData u = readMVar u

-- | initUseless initializes a Useless server layout.
initUseless :: IO Useless
initUseless = newMVar $ UselessData {sites = Map.empty, stringmap = Map.empty}

{- |
'register' adds resources to your a server layout, and returns a new server layout.

The given function is called, when the resource is requested. It gets a 'HTTPRequest' and has to respond with a 'HTTPResponse'

e.g:

>  server = do 
>  	layout <- initUseless
>  	layout <- register "/test" someFunction layout 
-}
register :: String -> UselessSite -> Useless -> IO ()
register site f useless = do
	u <- takeMVar useless
	putMVar useless $ UselessData {sites = Map.insert site f (sites u), stringmap = stringmap u}
--	return useless

-- | startServer starts the server at a specific Port
startServer :: Integer -> Useless -> IO ()
startServer port useless = withSocketsDo $ do
        putStrLn $ "Starting Server on " ++ show port
	socket <- listenOn $ PortNumber $ fromIntegral port
	mainloop socket useless

mainloop :: Socket -> Useless-> IO ()
mainloop socket useless = do
	(handle, hostname, portnr) <- accept socket
	hSetBuffering handle NoBuffering
	forkIO $ bearbeite handle useless
--        newuseless <- bearbeite handle useless
	mainloop socket useless

bearbeite :: Handle -> Useless -> IO ()
bearbeite handle useless = do
	eitherHttpRequest <- readRequest handle
	(site, httpRequest) <- case eitherHttpRequest of
	 Left i -> return (createErrorResponse i, HTTPRequest {httpReqMethod = "", httpReqVersion = "", httpReqFile = "", httpReqQueries = [], httpReqHeader = []})
	 Right httpRequest -> do
	  putStrLn $ "Serving File: " ++ httpReqFile httpRequest
	  u <- readMVar useless
          return (Map.findWithDefault (createErrorResponse 404) (httpReqFile httpRequest) (sites u), httpRequest)
        httpResponse <- createResponse httpRequest useless site
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
'createErrorResponse' returns a function, that gets a 'HTTPRequest' and answers with a 'HTTPResponse', for a given HTTP Status code

e.g:

>  	layout <- register "/forbidden" (createErrorResponse 403) layout 
-}
createErrorResponse :: Integer -> UselessSite
createErrorResponse n u _= do
    putStrLn $ "Error: " ++ show n
    return HTTPResponse { httpResStatus = n, httpResHeader = [], httpResBody = statushtml n}

createResponse :: HTTPRequest -> Useless -> UselessSite  -> IO HTTPResponse
createResponse request useless f =  f useless request

-- | The HTTP Request
data HTTPRequest = HTTPRequest  { httpReqMethod 	:: String
				, httpReqVersion 	:: String
				, httpReqFile		:: String
				, httpReqQueries	:: [(String, String)]
				, httpReqHeader	:: [(String,String)]
				}

-- | The HTTP Response
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
