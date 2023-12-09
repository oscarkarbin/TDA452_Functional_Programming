import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

--6A GUI (1) Intro to GUIs

-- GUI - Graphical user interface
{-
We define a layout.


Event-Based-Programming. Nothing happens to the gui aslong nothing happens. No events etc.


ThreePenny-GUI
-}

--6A GUI (2) Intro to Threepenny GUI

setup :: Window -> UI()
setup window = do
    return window # set UI.title "Hangman"
    button <- UI.button # set UI.text "Guess!" 
    getBody window #+ [element button]   -------- #+ append a ui element to a list

    on UI.click button $ \event ->                   -- This is how we attact an event to an element in this case a button
        do liftIO $ print "Button Clicked"           -- print is an IO monad. To use it in the UI monad we need liftIO function!
    return ()


main :: IO ()
main = startGUI defaultConfig setup