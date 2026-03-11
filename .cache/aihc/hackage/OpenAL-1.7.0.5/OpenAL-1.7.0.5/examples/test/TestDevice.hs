import Control.Concurrent ( threadDelay )
import Control.Monad ( unless )
import Data.Maybe ( listToMaybe )
import System.Environment ( getArgs )
import Sound.OpenAL

type DeviceSpecifier = Maybe String

showDevice :: DeviceSpecifier -> String
showDevice Nothing = "default"
showDevice (Just d) = "'" ++ d ++ "'"

orElse :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElse f g = f >>= maybe g (return . Just)

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

getDeviceSpec :: String -> IO [String] -> IO DeviceSpecifier
getDeviceSpec what getter = do
   deviceSpecs <- getter
   unless (null deviceSpecs) $ do
      putStrLn $ "Found " ++ show (length deviceSpecs) ++ " " ++ what ++ ":"
      mapM_ (putStrLn . ("   " ++)) deviceSpecs
   return $ listToMaybe deviceSpecs

main :: IO ()
main = do
   d <- getDeviceSpec "commandline arguments" getArgs `orElse`
        getDeviceSpec "enumerated devices" (get allDeviceSpecifiers)
   putStrLn $ "Using " ++ showDevice d ++ " device"
   device <- check "openDevice" $ openDevice d
   threadDelay 1000000
   check "closeDevice" $ fmap boolToMaybe $ closeDevice device
