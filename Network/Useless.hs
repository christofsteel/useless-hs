-- | Useless is a Module for building Webapplications
module Network.Useless (
HTTPRequest (HTTPRequest, httpReqMethod, httpReqVersion, httpReqFile, httpReqHeader, httpReqQueries),
HTTPResponse (HTTPResponse, httpResStatus, httpResHeader, httpResBody),
initUseless,
register,
createErrorResponse,
startServer,
UselessSite,
Useless (Useless, sites, stringmap)
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

type UselessSite = Useless -> HTTPRequest -> IO (Useless, HTTPResponse)
data Useless = Useless { sites :: Map.Map String UselessSite, stringmap :: Map.Map String String}


-- | initUseless initializes a Useless server layout.
initUseless :: IO Useless
initUseless = return $ Useless {sites = Map.empty, stringmap = Map.empty}

{- |
'register' adds resources to your a server layout, and returns a new server layout.

The given function is called, when the resource is requested. It gets a 'HTTPRequest' and has to respond with a 'HTTPResponse'

e.g:

>  server = do 
>  	layout <- initUseless
>  	layout <- register "/test" someFunction layout 
-}
register :: String -> UselessSite -> Useless -> IO Useless
register site f useless = return $ Useless {sites = Map.insert site f (sites useless), stringmap = stringmap useless}

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
--	forkIO $ bearbeite handle useless
        newuseless <- bearbeite handle useless
	mainloop socket useless

bearbeite :: Handle -> Useless -> IO Useless
bearbeite handle useless = do
	eitherHttpRequest <- readRequest handle
	(site, httpRequest) <- case eitherHttpRequest of
	 Left i -> return (createErrorResponse i, HTTPRequest {httpReqMethod = "", httpReqVersion = "", httpReqFile = "", httpReqQueries = [], httpReqHeader = []})
	 Right httpRequest -> do
	  putStrLn $ "Serving File: " ++ httpReqFile httpRequest
          return (Map.findWithDefault (createErrorResponse 404) (httpReqFile httpRequest) (sites useless), httpRequest)
        httpResponse <- createResponse httpRequest useless site
        sendResponse handle $ snd httpResponse
        return $ fst httpResponse

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
    return (u,HTTPResponse { httpResStatus = n, httpResHeader = [], httpResBody = statushtml n})

createResponse :: HTTPRequest -> Useless -> UselessSite  -> IO (Useless, HTTPResponse)
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
