splitLines :: String -> [String]
splitLines "" = []
splitLines text = line:(splitLines (chompHead remainder))
    where (line,remainder) = break isLineTerminator text


isLineTerminator = flip elem ['\r','\n']

chompHead :: String -> String
chompHead ( '\r':'\n':xs ) = xs
chompHead ( '\r':xs      ) = xs
chompHead ( '\n':xs      ) = xs
chompHead xs               = xs
