
open mouseLibrary.IOConnectors
open mouseLibrary.InteractiveLoop
module Program = let [<EntryPoint>] main _ = 
    let outputStream = ConsoleOutput() :> OutputStream
    let inputStream = ConsoleInput() :> InputStream
    let mouseInteractivePlayer = MouseInteractivePlayer(inputStream,outputStream)
    outputStream.TextOutput("welcome to the mouse in a box game.")
    mouseInteractivePlayer.Play() 

    0
