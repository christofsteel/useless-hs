import Network.Useless
import qualified Data.Map as Map

main = do
  theServer <- initUseless
  register theServer "/" (foobar "A String")
  startServer 18080 theServer

foobar :: String -> UselessSite
foobar s useless req = return HTTPResponse 
  { httpResStatus = 200
  , httpResVersion = httpReqVersion req
  , httpResHeader = Map.fromList [("Content-Type", "text/html"), ("Transfer-Encoding", "identity")]
  , httpResBody = "<!DOCTYPE html><html><head><title>"++ s ++"</title></head><body>You are using "++ show (httpReqVersion req) ++
  ", but I'm ignoring that, sorry :(</body></html>"
  }
