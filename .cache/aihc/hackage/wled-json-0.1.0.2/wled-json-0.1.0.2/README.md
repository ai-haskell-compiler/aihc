# wled-json ✨

Haskell bindings for controlling WLED devices, with a focus on Octolamps

<p align="center">
  <img src="https://raw.githubusercontent.com/alaendle/wled-json/main/images/octolamp.jpg" alt="Octolamp showing french flag" width="50%"/>
</p>

## Introduction 💡

This Haskell library provides a convenient interface for interacting with WLED devices, particularly those based on the popular Octolamp design. It leverages the WLED API (https://kno.wled.ge/) to allow you to control various aspects of your LED lights, including:

- **Color:** Set the color of your lights using various color spaces (RGB, HSV, etc.)
- **Brightness:** Adjust the overall brightness of your lights

## Usage 📲

```haskell
import           Control.Concurrent (threadDelay)
import           WLED.Device        (getLampState, setLampState)
import           WLED.Lamp.Octocat  (octocatSpec)
import           WLED.Pattern.Flags (bulgaria)
import           WLED.Types         (diff)

main :: IO ()
main = do
    -- Connect to a WLED device
    lampState <- getLampState wledUrl
    case lampState of
        Left errMsg -> putStrLn errMsg
        Right initialState -> do
            -- Display the Bulgarian flag on an Octolamp
            _ <- setLampState wledUrl (bulgaria octocatSpec)

            -- Just sleep five seconds
            threadDelay 5000000

            -- Restore the initial state
            Right currentState <- getLampState wledUrl
            _ <- setLampState wledUrl (diff currentState initialState)
            pure ()
  where
    wledUrl = "http://192.168.178.34"
```

## Features 👍

- Comprehensive WLED API coverage
- Type-safe interactions
- Easy-to-use API
- Specific support for Octolamps
- Reusable pattern for other lamp designs

## Contributing 🤝

Contributions are welcome! Please feel free to open an issue or submit a pull request.

## License ✌️

[BSD-3-Clause](./LICENSE)
