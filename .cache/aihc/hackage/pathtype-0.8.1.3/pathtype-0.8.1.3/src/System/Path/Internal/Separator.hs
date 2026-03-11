module System.Path.Internal.Separator where


extension :: Char
extension = '.'

searchPath :: Char
searchPath = ':'

isExtension :: Char -> Bool
isExtension = (== extension)

isSearchPath :: Char -> Bool
isSearchPath = (== searchPath)
