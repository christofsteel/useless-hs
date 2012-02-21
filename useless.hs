import Network
import IO
import Control.Concurrent
import System
import System.Directory
import Data.List
import System.FilePath.Posix
import System.Process

main = withSocketsDo $ do
	args <- getArgs
	if length args < 2 then do
		putStrLn $ "useless <PORT> <ROOTDIR>"
		exitWith $ ExitFailure 1
	 else do
		let port = read (args !! 0) :: Integer
		let rootdir = args !! 1
		socket <- listenOn $ PortNumber $ fromIntegral port
		mainloop socket rootdir

mainloop socket rootdir = do
	(handle, hostname, portnr) <- accept socket
	hSetBuffering handle NoBuffering
	forkIO $ bearbeite handle rootdir
	mainloop socket rootdir

bearbeite handle rootdir = do
	httpRequest <- readRequest handle
	putStrLn $ "Serving File: " ++ rootdir ++ httpReqFile httpRequest
	httpResponse <- writeResponse httpRequest rootdir
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


data HTTPRequest = HTTPRequest  { httpReqMethod 	:: String
				, httpReqVersion 	:: String
				, httpReqFile		:: String
				, httpReqQueries	:: [(String, String)]
				, httpReqHeader	:: [(String,[String])]
				}

data HTTPResponse = HTTPResponse	{ httpResStatus	:: Integer
					, httpResHeader :: [(String,[String])]
					, httpResBody :: String
					}

isInDirectory :: FilePath -> FilePath -> IO Bool
isInDirectory file root = do
	canfile <- canonicalizePath file
	canroot <- canonicalizePath root
	return $ isPrefixOf canroot canfile
        

statushtml n = "<!DOCTYPE html><html><head><title>"++ (createStatusLine' n) ++"</title></head><body><img alt=\"" ++ (createStatusLine' n)++ "\" src=\"http://httpcats.herokuapp.com/" ++ (show n) ++ "\"/></body></html>"

writeResponse :: HTTPRequest -> String -> IO HTTPResponse
writeResponse req rootdir = do
    if (httpReqMethod req) /= "GET" then
      return $ HTTPResponse {httpResStatus = 501, httpResBody = statushtml 400 }
     else do
        let filepath = rootdir ++ httpReqFile req
	fileExists <- doesFileExist filepath
	if fileExists == False then 
		return $ HTTPResponse {httpResStatus = 404, httpResBody = statushtml 404 }
	 else do
		inDirectory <- isInDirectory filepath rootdir
		if inDirectory == False then
			return $ HTTPResponse {httpResStatus = 403, httpResBody = statushtml 403 }
                 else do
                    file <- try $ cgi filepath
                    case file of
                        Left e -> return $ HTTPResponse {httpResStatus = 500, httpResBody = statushtml 500}
                        Right file -> return $ HTTPResponse {httpResStatus = 200, httpResBody = file}
			

cgi filepath = if (takeExtension filepath == ".php") then
    readProcess "php" [filepath] ""
   else
    readFile filepath

readRequest :: Handle -> IO HTTPRequest
readRequest handle = do
	requestline <- hGetLine handle        
	putStrLn "Incoming Connection"
	return $ mkRequest $ words requestline

mkRequest :: [String] -> HTTPRequest
mkRequest (m:f:v:xs) = HTTPRequest {httpReqMethod = m, httpReqFile = f, httpReqVersion = v}
mkRequest xs = HTTPRequest {httpReqMethod = "FAIL"}
