module Main where

import Yesod
import Render (render)
import Fractals (drawMandelbrot, screen)
import Codec.Picture (writePng, generateImage, encodePng)

data App = App
instance Yesod App

mkYesod "App" [parseRoutes| 
                / HomeR GET 
                /hey/#String HeyR GET 
                /mandelbrot MandelbrotR GET
                |]

mandelbrotImage = generateImage (render drawMandelbrot) (fst screen) (snd screen)
bytes = encodePng mandelbrotImage
               

getHomeR = defaultLayout $ [whamlet| <p> Hello World! |]
getHeyR name = defaultLayout $ [whamlet| <p> Hello, #{name}! |]

getMandelbrotR = sendResponse $ toTypedContent (typePlain, toContent bytes)

main = warpEnv App