import Network.Useless

main = do
    useless <- initUseless
    register useless "/" $ uselessFile "text/html" "file.html"
    startServer 8080 useless
