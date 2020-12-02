
## An F# exercise.

A box is modeled like a rectangular grid with cells having coordinates like (0,0)  (the south west corner) and (N,M) (the north east corner).

The user can set N and N.

The user can place a mouse in the box providing its position (i.e the cell) and its orientation: South, North, East, West. 

The user also can give the following commands to move the mouse:

Turn the mouse Left

Turn the mouse Right 

Move Forward

For instance, if the mouse is oriented to the east and its position is (x,y) then its position after a forward command will be (x+1, y).

The cells of the box can be free or locked. In the beginning, all the cells are free.

If the mouse receives a command that makes it go outside of the box (i.e. the target cell will be a (x,y) where x<0 or x>N or y<0 or y>M) then it can actually escape only if the starting cell is free. In that case After moving the mouse state will be Escaped, and the starting cell becomes locked.

A mouse cannot escape from a locked cell, so if it tries to do it, it will just stay there.

The user gives commands by the console using sequences of characters on multiple lines.

The first line indicates the N and M values

For instance, the sequence of characters 53 indicates that M is 5 and N is 3.

Each of the following lines is meant to indicate the position of a mouse and the sequence of its movement in the box.

The position indicated in the line is like 11E (position (1,1) oriented to East) followed by a space and a sequence of chars 'L', 'R' and 'F' (turn left, turn right, go forward).


Some examples:
set the box dimensions as 5 x 3, then place the first mouse in position 11 oriented to East and
give it the commands RFRFRFRF
The second and third mouse will be given similar commands
```
53

11E RFRFRFRF
32N FRRFLLFFRRFLL
03W LLFFFLFLFL

```

as output, we will have the final position of the mouse for each line

```
11E
33NESCAPED
23S
```



## The model
The domain model is in the module Mouse

Here some notes about:

a position is a pair of int:

```
type Position = int*int
```

A cell has a position and a state:

```
type Cell = {CellState: CellState; Position: Position}
```

The state of a cell can be Locked or free:

```
type CellState = 
    | Locked 
    | Free
```

A box is a list of cells, and a X and Y bounds:

```
    type Box = 
        {
            Cells: List<Cell> 
            XDim: int
            YDim: int
         }

```

The possible movement of the mouse are turning left, right, or going forward:

```
    type Movement = 
        | Left 
        | Right 
        | Forward
```
The orientation of the mouse: 
```
    type Orientation = 
        | South 
        | North 
        | East 
        | West

```

A "plain" mouse has a position and an orientation:

```
    type PlainMouse = 
        { 
            Orientation: Orientation 
            Position: Position
        }
```

A mouse can be active or escaped

```
    type Mouse = 
        | Active of PlainMouse 
        | Escaped of PlainMouse
```

```
    let move movement (mouse, box) =
        match mouse with
        | Active movingMouse ->
            match movement with
            | Forward ->
                match (movingMouse |> isInBorder box movingMouse.Orientation) with
                    true when (box |> isCellLocked movingMouse.Position) ->
                        mouse, box 
                    | true -> Escaped movingMouse, box |> lockCell movingMouse.Position
                    | _ -> Active {movingMouse with Position = movingMouse.Position |> go movingMouse.Orientation}, box
            | _ ->
                Active (movingMouse |> turn movement), box
        | Escaped movingMouse ->
            Escaped movingMouse, box
```



## Commands:
Here we are in the module World:

In the previous part, we have seen what is needed to make the mouse move in the box according to the commands.
We want to model also a "world" where we can deal with creating commands, process the commands, and see the results.
```
    type World = 
        {
            Mouse: Mouse 
            Box: Box 
            Commands: Command list 
            Logs: string list 
        } 
```

We see the module World where we define the commands:

```
    type Command = 
       SetTheBox of int*int 
       | SetTheMousePosition of Position*Orientation
       | TurnLeft
       | TurnRight
       | MoveForward
       | LogPosition
```

A command gets the mouse, box, and logs and returns them updated accordingly

```
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

```

Updating the world means processing the first command in the list of the commands and returning a new world with
everything updated. The new world will have a list of command the "tail" of the
commands we had in the world before updating:

```
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

```

To update the world consuming all the commands in the list we have a "processAllCommands" function:
```
    let processAllCommands world =
        world.Commands |> List.fold (fun x _ -> x |> update) world
```

We could implement an "undo"  in this way: removing the last command to the world and applying the processAllCommand again.


## Commands in string format

At an outer level, the commands are given as strings.
The module StringCommandParsing does this job with various functions about

## Abstracting input and output 

The program is console-based, but to make it independent from the concrete IO (for instance to facilitate 
testing based on mock) we have an IOConnectors module where I defined OutputStream and InputStream as interfaces, 
with concrete implementations based on Console.

## Interactive loop

The module InteractiveLoop defines the way we actually interact using the console.
There is a first step where we set the coordinates of the box:

```
        member this.PlayFirstStep() =
            outputStream.TextOutput("write the upper left box coordinates")
            let boxCoordinates = inputStream.TextInput()
            try
                let setTheBoxCommand = stringCoordsToSetTheBoxCommand boxCoordinates
                world <- {world with Commands = [setTheBoxCommand]} |> processAllCommands
            with 
                | _ -> failwith "an error occurred"
```

Each further step is about interpreting lines like 32N FRRFLLFFRRFLL

```
        member this.PlayFollowUpSteps() =
            outputStream.TextOutput("write the mouse coordinate and the sequence of movements")
                let firstCommandLine = inputStream.TextInput()
                if (firstCommandLine.Trim()) <> "" then 
                    let commands = parseSetPositionAndMovementsTextCommands firstCommandLine
                    world <- {world with Commands = commands} |> processAllCommands
                    let output = safeGetHeadOfLog world
                    world <- safeConsumeHeadOfLog world
                    outputStream.TextOutput ("the mouse state now is: "+output)
```

Playing is about starting the first step and then entering an infinite loop:

```
        member this.Play() =
            this.PlayFirstStep()
            while true do
                this.PlayFollowUpSteps()
                ()
```



