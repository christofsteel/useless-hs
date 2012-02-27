import Network.Useless

main = do
  theServer <- initUseless
  register theServer "/" (foobar "A String")
  startServer 8080 theServer

foobar :: String -> UselessSite
foobar s useless req = return HTTPResponse 
  { httpResStatus = 200
  , httpResHeader = []
  , httpResBody = "<!DOCTYPE html><html><head><title>"++ s ++"</title></head><body>You are using "++httpReqVersion req++", but I'm ignoring that, sorry :(</body></html>"
  }
