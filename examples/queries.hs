import Network.Useless

main = do
	useless <- initUseless
	register useless "/" showq
	startServer 8080 useless

showq :: UselessSite
showq useless req = return $ createBasicHTTP $ show $ getQueries req
