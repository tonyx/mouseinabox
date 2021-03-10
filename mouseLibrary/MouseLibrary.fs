namespace mouseLibrary

open System
open System.Text.RegularExpressions

module Mouse =
    type Position = int*int
    type CellState = 
        | Locked 
        | Free
    type Movement = 
        | Left 
        | Right 
        | Forward
    type Cell = 
        {
            CellState: CellState 
            Position: Position
        }
    type Box = 
        {
            Cells: List<Cell> 
            XDim: int
            YDim: int
         }
    let makeBox xDim yDim = 
        let cells = 
            [
                for x in [0 .. xDim] do 
                    for y in [0 .. yDim] -> 
                    {
                        CellState=Free; Position=(x,y)
                    }
            ]
        {
            Cells = cells
            XDim = xDim
            YDim = yDim
        }
    type Orientation = 
        | South 
        | North 
        | East 
        | West

    type PlainMouse = 
        { 
            Orientation: Orientation 
            Position: Position
        }

    type Mouse = 
        | Active of PlainMouse 
        | Escaped of PlainMouse

    let makeMovingMouse (x, y) orientation =
        Active {
            Orientation=orientation
            Position = (x, y)
        }

    let lockCell (x, y) box =
        {
            box with Cells = 
                {
                    CellState = Locked
                    Position = (x, y)
                }
                :: (box.Cells 
                   |> List.filter 
                        (fun cell -> fst cell.Position <> x || snd cell.Position <> y))
        }

    let isCellLocked ((x, y): Position) (box: Box) =
        box.Cells 
        |> List.find 
            (fun cell -> fst cell.Position = x && snd cell.Position = y) 
            |> fun item -> item.CellState = Locked

    let go direction (x, y) =
        match direction with
        | North -> (x, y + 1)
        | South -> (x, y - 1)
        | West ->  (x - 1, y)
        | East ->  (x + 1, y)

    let turn orientation plainMouse =
        let turnRight plainMouse =
            match plainMouse.Orientation with
                | North ->  {plainMouse with Orientation = East}
                | East ->   {plainMouse with Orientation = South}
                | South ->  {plainMouse with Orientation = West}
                | West ->   {plainMouse with Orientation = North}
                
        let turnLeft plainMouse =
            match plainMouse.Orientation with
                | North ->  {plainMouse with Orientation = West}
                | West ->   {plainMouse with Orientation = South}
                | South ->  {plainMouse with Orientation = East}
                | East ->   {plainMouse with Orientation = North}

        match orientation with
            | Right ->  plainMouse |> turnRight
            | Left ->   plainMouse |> turnLeft
            | _ -> failwith "unexpected case"

    let isInBorder box direction plainMouse =
        match direction with
            | North ->  box.YDim = snd plainMouse.Position
            | South ->  snd plainMouse.Position = 0
            | East ->   fst plainMouse.Position = box.XDim
            | West ->   fst plainMouse.Position = 0        

    let move movement (mouse, box) =
        match mouse with
        | Active activeMouse ->
            match movement with
            | Forward ->
                match (activeMouse |> isInBorder box activeMouse.Orientation) with
                    true when (box |> isCellLocked activeMouse.Position) ->
                        mouse, box 
                    | true -> Escaped activeMouse, box |> lockCell activeMouse.Position
                    | _ -> Active {activeMouse with Position = activeMouse.Position |> go activeMouse.Orientation}, box
            | _ ->
                Active (activeMouse |> turn movement), box
        | Escaped activeMouse ->
            Escaped activeMouse, box

module World =
    open Mouse

    type Command = 
       SetTheBox of int*int 
       | SetTheMousePosition of Position*Orientation
       | TurnLeft
       | TurnRight
       | MoveForward
       | LogPosition

    let orientationToShortCharNotation (orientation:Orientation) =
        match orientation with
            | North -> 'N'
            | South -> 'S'
            | East -> 'E'
            | West -> 'W' 

    let mouseToShortTextNotation (mouse: Mouse) =
        let toTextNotation(aliveMouse: PlainMouse) =
            sprintf "%d%d%c" (fst aliveMouse.Position) (snd aliveMouse.Position) (orientationToShortCharNotation aliveMouse.Orientation)
        match mouse with
        | Active activeMouse -> toTextNotation(activeMouse)
        | Escaped escapedMouse -> sprintf "%s%s" (toTextNotation(escapedMouse)) "ESCAPED"

    let processCommand command (mouse, box, logs) =
        match command with
            | SetTheBox (x,y) -> (mouse, (makeBox x y), logs)
            | SetTheMousePosition ((x,y),orientation) -> ((makeMovingMouse (x,y) orientation), box, logs)
            | TurnRight ->  
                let (mouseNew, boxNew) = (mouse, box) |> move Right
                mouseNew,boxNew,logs
            | TurnLeft ->   
                let (mouseNew, boxNew) = (mouse, box) |> move Left
                mouseNew,boxNew,logs
            | MoveForward -> 
                let (mouseNew, boxNew) = (mouse, box) |> move Forward
                mouseNew, boxNew, logs
            | LogPosition -> 
                mouse, box, logs@[mouseToShortTextNotation mouse]

    type World = 
        {
            Mouse: Mouse 
            Box: Box 
            Commands: Command list 
            Logs: string list 
        } 

    let makeWorld mouse box =
        {
            Mouse=mouse
            Box=box
            Commands=[]
            Logs=[]
        }

    let defaultWorld() = 
        makeWorld (makeMovingMouse (0,0) North) (makeBox 5 5)    

    let addCommand command world  =
        {
            world with Commands = world.Commands@[command]
        }

    let update world =
        match world.Commands with
        | [] -> world
        | H::T ->
            let (newMouse, newTank, logs) = processCommand H (world.Mouse, world.Box, world.Logs) 
            {
                world with 
                    Mouse = newMouse 
                    Box = newTank 
                    Commands=T
                    Logs = logs 
            }    

    let processAllCommands world =
        world.Commands |> List.fold (fun x _ -> x |> update) world

module StringCommandParsing =
    open World
    open Mouse
    let stringCoordsToSetTheBoxCommand (inString:string) =
        let converted = inString |> Seq.toList
        let (x,y) =
            match converted with
            | X::Y::_ -> (X |> Char.GetNumericValue |> int, Y |> Char.GetNumericValue |> int)
            | _ -> failwith ("error in parsing the string: " + inString)
        SetTheBox (x,y) 

    let stringToSetTheMouseCommand (inString:string) =
        let (x,y,z) =
            match (inString |> Seq.toList)   with
                | X::Y::Z::_ ->
                    let orientation = match Z with
                        | 'E' -> East
                        | 'S' -> South
                        | 'N' -> North
                        | 'W' -> West
                        | _ -> failwith (sprintf "%s %c %s" "parse error: the " Z "char does not correspond to a cardinal point")
                    X |> Char.GetNumericValue |> int, Y |> Char.GetNumericValue |> int, orientation
                | _ -> failwith (sprintf "%s %s" "error in input string " inString) 
        SetTheMousePosition((x,y),z)

    let stringToMoveTheMouseCommands (inString:string) =
        let charToCommand (c:char) =
            match c with
                | 'R' -> TurnRight
                | 'L' -> TurnLeft
                | 'F' -> MoveForward
                | _ -> failwith (sprintf "%s %c %s %s" "parse error, unexpected" c "char in string " inString)
        let converted = inString |> Seq.toList
        (converted |> List.map (charToCommand))@[LogPosition]

    let parseSetPositionAndMovementsTextCommands (inString: string) =
        try
            let splittedString = Regex.Split(inString," +")
            let setPositionStringCommand = splittedString.[0]
            let movementsCommands = splittedString.[1]
            stringToSetTheMouseCommand(setPositionStringCommand)::
                stringToMoveTheMouseCommands(movementsCommands)
        with
        | _ -> failwith "an error occurred in setting the box size"

    let safeGetHeadOfLog world =
        match world.Logs with
        | H::_ -> H
        | _ -> ""

    let safeConsumeHeadOfLog world =
        match world.Logs with
        | _::T -> {world with Logs = T}
        | _ -> world

    let orientationToShortCharNotation orientation =
        match orientation with
            | North -> 'N'
            | South -> 'S'
            | East -> 'E'
            | West -> 'W'

    let mouseToShortTextNotation mouse =
        let toTextNotation(aliveMouse:PlainMouse) =
            sprintf "%d%d%c" 
                (fst aliveMouse.Position) 
                (snd aliveMouse.Position) 
                (orientationToShortCharNotation aliveMouse.Orientation)
        match mouse with
        | Active plainMouse -> toTextNotation(plainMouse)
        | Escaped plainMouse -> sprintf "%s%s" (toTextNotation(plainMouse)) "ESCAPED"

module IOConnectors =
    
    type OutputStream =
        abstract TextOutput: string -> unit

    type InputStream =
        abstract TextInput: unit -> string

    type ConsoleOutput() =
        interface OutputStream with
           member this.TextOutput(x) = printf "%s\n" x

    type ConsoleInput() =
        interface InputStream with
            member this.TextInput() = Console.ReadLine()

module InteractiveLoop =
    open IOConnectors
    open StringCommandParsing
    open World
    type MouseInteractivePlayer(inputStream: InputStream, outputStream: OutputStream) =
        let mutable world = defaultWorld()
        member this.InputStream = inputStream
        member this.OutputStream = outputStream
          
        member this.PlayFirstStep() =
            outputStream.TextOutput("write the upper left box coordinates")
            let boxCoordinates = inputStream.TextInput()
            try
                let setTheBoxCommand = stringCoordsToSetTheBoxCommand boxCoordinates
                world <- {world with Commands = [setTheBoxCommand]} |> processAllCommands
            with 
                | _ -> failwith "an error occurred"
        
        member this.PlayFollowUpSteps() =
            outputStream.TextOutput("write the mouse coordinate and the sequence of movements")
            try
                let firstCommandLine = inputStream.TextInput()
                if (firstCommandLine.Trim()) <> "" then 
                    let commands = parseSetPositionAndMovementsTextCommands firstCommandLine
                    world <- {world with Commands = commands} |> processAllCommands
                    let output = safeGetHeadOfLog world
                    world <- safeConsumeHeadOfLog world
                    outputStream.TextOutput ("the mouse state now is: "+output)
            with 
                | _ -> 
                    let _ = outputStream.TextOutput("something went wrong, try again");
                    ()
           
        member this.Play() =
            this.PlayFirstStep()
            while true do
                this.PlayFollowUpSteps()
                ()


