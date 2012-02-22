module Useless (
HTTPRequest (HTTPRequest),
HTTPResponse (HTTPResponse),
httpResStatus,
httpResHeader,
httpResBody,
httpReqFile,
initUseless,
register,
startServer
) where

import Network
import IO
import Control.Concurrent
import System
import System.Directory
import Data.List
import qualified Data.Map as Map
import System.FilePath.Posix
import System.Process

initUseless :: IO (Map.Map String (HTTPRequest -> IO HTTPResponse))
initUseless = return $ Map.empty

register :: String -> (HTTPRequest -> IO HTTPResponse) -> Map.Map String (HTTPRequest -> IO HTTPResponse) -> IO (Map.Map String (HTTPRequest -> IO HTTPResponse))
register site f sites = return $ Map.insert site f sites

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
	httpRequest <- readRequest handle
	putStrLn $ "Serving File: " ++ httpReqFile httpRequest
        let site = Map.findWithDefault (createErrorResponse 404) (httpReqFile httpRequest) sites
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
createStatusLine n = "HTTP/1.0 "++ (createStatusLine' n) ++"\n\r"

createStatusLine' :: Integer -> String
createStatusLine' 200 = "200 OK"
createStatusLine' 400 = "400 BAD REQUEST"
createStatusLine' 403 = "403 FORBIDDEN"
createStatusLine' 404 = "404 NOT FOUND"
createStatusLine' 500 = "500 INTERNAL SERVER ERROR"
createStatusLine' 501 = "501 NOT IMPLEMENTED"
createStatusLine' _ = "418 I'M A TEAPOT"


createErrorResponse :: Integer -> HTTPRequest -> IO HTTPResponse
createErrorResponse n _ = do
    putStrLn $ "Error: " ++ show n
    return HTTPResponse { httpResStatus = n, httpResHeader = [], httpResBody = statushtml n}

createResponse :: HTTPRequest -> (HTTPRequest -> IO HTTPResponse)  -> IO HTTPResponse 
createResponse request f =  f request

data HTTPRequest = HTTPRequest  { httpReqMethod 	:: String
				, httpReqVersion 	:: String
				, httpReqFile		:: String
				, httpReqQueries	:: [(String, String)]
				, httpReqHeader	:: [(String,String)]
				}

data HTTPResponse = HTTPResponse	{ httpResStatus	:: Integer
					, httpResHeader :: [(String,String)]
					, httpResBody :: String
					}

statushtml n = "<!DOCTYPE html><html><head><title>"++ (createStatusLine' n) ++"</title></head><body><img alt=\"" ++ (createStatusLine' n)++ "\" src=\"http://httpcats.herokuapp.com/" ++ (show n) ++ "\"/></body></html>"

readRequest :: Handle -> IO HTTPRequest
readRequest handle = do
	requestline <- hGetLine handle
        header <- getHeader handle
	return $ mkRequest header $ words requestline

mkRequest :: [(String,String)] -> [String] -> HTTPRequest
mkRequest h (m:f:v:xs) = HTTPRequest {httpReqMethod = m, httpReqFile = f, httpReqVersion = v, httpReqHeader = h, httpReqQueries=[]}
mkRequest h xs = HTTPRequest {httpReqMethod = "FAIL", httpReqFile=[], httpReqVersion=[], httpReqHeader=[], httpReqQueries=[]}

getHeader :: Handle -> IO [(String, String)]
getHeader h = do
    tryheaderline <- try $ hGetLine h
    case tryheaderline of
        Left e -> return []
        Right headerline  -> do            
            if length headerline == 1 then return []
             else do
              let header = mkHeader $ words headerline
              headers <- getHeader h
              return $ header:headers

mkHeader :: [String] -> (String, String)
mkHeader (l:r) = (l,unwords r)
mkHeader _ = ("","")
