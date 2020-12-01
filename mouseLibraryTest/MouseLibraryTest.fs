module mouseLibraryTest
open mouseLibrary.Mouse
open mouseLibrary.World
open mouseLibrary.StringCommandParsing
open mouseLibrary.IOConnectors

open NUnit.Framework

module Mouse =

    [<Test>]
    let ``mouse moves from south to north`` () =
        let box = makeBox 5 5
        let mouse = makeMovingMouse (0,0) North
        let (movedMouse,_) = (mouse,box) |> move Forward
        let expectedMouseAfterMovement = makeMovingMouse (0,1) North
        Assert.AreEqual(expectedMouseAfterMovement,movedMouse)

    [<Test>]
    let ``when a mouse is stray and tries to move it stays in the same position`` () =
        let box = makeBox 5 5 
        let mouse = 
            Escaped { 
                Orientation=North
                Position=(0,0)
            }
        let (movedMouse,_) = (mouse,box) |> move Forward
        Assert.AreEqual(mouse,movedMouse)

    [<Test>]
    let ``we must know when a mouse is in a border`` () =
        let box = makeBox 5 5
        let plainMouse =
            {
                Orientation=North
                Position = (0,5)
            }
        Assert.IsTrue(plainMouse |> isInBorder box North,"should be true")

    [<Test>]
    let ``we must know when a mouse is not in a border`` () =
        let box = makeBox 5 5
        let plainMouse =
            {
                Orientation=North
                Position = (2,2)
            }
        Assert.IsFalse(plainMouse |> isInBorder box North,"should be false")
        
    [<Test>]
    let ``when the mouse is the north border and is also oriented to north, in moving forward will become stray`` () =
        let box = makeBox 5 5
        let mouse = makeMovingMouse (0,5) North
        let (movedMouse, _) = (mouse, box) |> move Forward
        let _ = 
            match movedMouse with
            | Escaped _ -> Assert.IsTrue(true) 
            | _ -> Assert.Fail("mouse should be stray")
        ()

    [<Test>] 
    let ``when a mouse escapes it will make the escape cell dirty`` () =
        let box = makeBox 5 5
        let mouse = makeMovingMouse (0,5) North
        let (_,newBox) = (mouse, box) |> move Forward
        let exitCell =
            newBox.Cells
            |> List.find (fun x -> x.Position = (0,5))
        Assert.AreEqual(Locked,exitCell.CellState)
        
    [<Test>]
    let ``a mouse fails to escape if tries to do it from a cell that is dirty`` () =
        let box = makeBox 5 5
        let box =
            {
                box with Cells =
                    [
                        {
                            Position=(0, 5)
                            CellState=Locked
                        }
                    ]
            }
        let mouse = makeMovingMouse (0, 5) North
        let (movedMouse,_) = (mouse,box) |> move Forward
        Assert.AreEqual(mouse,movedMouse)
        
    let ``we will be able to distinguish a cell that is clean``() =
        let box = makeBox 5 5
        let cleanCell =
            box.Cells
            |> List.find (fun cell -> cell.Position = (0,5))
        Assert.AreEqual(Free,cleanCell.CellState)

    [<Test>]
    let ``we will be able to distinguish a cell that is dirty``() =
        let box = makeBox 5 5
        let newBox = box |> lockCell (0, 5)
        let dirtyCell =
            newBox.Cells
            |> List.find (fun cell -> cell.Position = (0, 5))
        Assert.AreEqual(Locked,dirtyCell.CellState)


    [<Test>] 
    let ``mouse turns right`` () =
        let box = makeBox 5 5
        let box =
            {
                box with Cells = [{Position=(0, 5);CellState=Locked}]
            }
        let mouse = makeMovingMouse (0, 5) North
        let (movedMouse,_) = (mouse,box) |> move Right
        let _ = 
            match movedMouse with
            | Active X -> Assert.AreEqual(East,X.Orientation)
            | _ -> Assert.IsTrue(false)
        ()

    [<Test>] 
    let ``mouse turns left `` () =
        let box = makeBox 5 5
        let box =
            {
                box with 
                    Cells = [
                       {
                            Position=(0,5)
                            CellState=Locked}
                       ]
            }
        let mouse = makeMovingMouse (0,5) North
        let (movedMouse,_) = (mouse,box) |> move Left
        let _ = 
            match movedMouse with
            | Active X -> Assert.AreEqual(West,X.Orientation)
            | _ -> Assert.IsTrue(false)
        ()

module World =

    [<Test>]
    let ``the update applied on a world with no commands will returns the same world `` () =
        Assert.AreEqual(defaultWorld() |> update,defaultWorld())

    [<Test>]
    let ``update of the world with a SetTheBox command will returns a world having a box changed accordingly`` () =
        let world =
            {
                defaultWorld() with Commands = [SetTheBox (6, 6)]
            }
        let newWorld = world |> update
        let expectedBox = makeBox 6 6
        Assert.AreEqual(expectedBox,newWorld.Box)

    [<Test>]
    let ``update the world with a SetTheMousePosition`` () =
        let world =
            {
                defaultWorld() with Commands = [SetTheMousePosition ((1,1),North)]
            }
        let newWorld = world |> update
        let expectedMouse = makeMovingMouse (1,1) North
        Assert.AreEqual(expectedMouse,newWorld.Mouse)

    [<Test>]
    let ``update the world when the command is about turning the mouse left`` () =
        let world =
            {
                defaultWorld() with Mouse = makeMovingMouse (0,0) South; Commands = [TurnLeft]
            }
        let newWorld = world |> update
        let expectedMouse = makeMovingMouse (0,0) East
        Assert.AreEqual(expectedMouse,newWorld.Mouse)

    [<Test>]
    let ``update the world when the command is moving the mouse forward`` () =
        let world =
            {
                defaultWorld() with
                    Mouse = makeMovingMouse (0,0) East
                    Commands = [MoveForward]
                    Box = makeBox 5 5
            }
        let newWorld = world |> update
        let expectedMouse = makeMovingMouse (1,0) East
        Assert.AreEqual(expectedMouse,newWorld.Mouse)

    [<Test>]
    let ``update the world making the mouse Escape going over the border from a clean cell `` () =
        let world =
            {
                defaultWorld() with
                    Mouse = makeMovingMouse (0,5) North
                    Commands = [MoveForward]
                    Box = makeBox 5 5
            }
        let newWorld = world |> update
        let expectedMouse =
            Escaped
                {
                    Position=(0,5)
                    Orientation=North
                }
        Assert.AreEqual(expectedMouse,newWorld.Mouse)

    [<Test>]
    let ``update the world making the mouse triing to Escape going over the border from a dirty cell `` () =
        let world =
            {
                defaultWorld() with
                    Mouse = makeMovingMouse (0,5) North
                    Commands = [MoveForward]
                    Box = makeBox 5 5 |> lockCell (0,5)
            }
        let newWorld = world |> update
        let expectedMouse =
            Active
                {
                    Position=(0,5)
                    Orientation=North
                }
        Assert.AreEqual(expectedMouse,newWorld.Mouse)

module StringCommandParsing =

    [<Test>]
    let ``will be able to parse the string with the tank size by adding the equivalent command ``()=
       let expectedCommand = SetTheBox (5,5)
       let actualCommand = stringCoordsToSetTheBoxCommand "55"
       Assert.AreEqual(expectedCommand,actualCommand)

    [<Test>]
    let ``move the mouse parse command``() =
        let expectedCommands =
            [
                TurnLeft
                TurnRight
                MoveForward
                LogPosition
            ]
        let actualCommand = stringToMoveTheMouseCommands "LRF"
        Assert.AreEqual(expectedCommands,actualCommand)

    [<Test>]
    let ``parse a line about position and moving the mouse``() =
        let expectedCommands =
            [
                SetTheMousePosition ((1,1),East)
                TurnRight
                MoveForward
                TurnRight
                MoveForward
                TurnRight
                MoveForward
                TurnRight
                MoveForward
                LogPosition
            ]
        let actualCommands = parseSetPositionAndMovementsTextCommands "11E RFRFRFRF"
        Assert.AreEqual(expectedCommands,actualCommands)

    [<Test>]
    let ``logs``() =
        let commands = parseSetPositionAndMovementsTextCommands "11E RFRFRFRF" 
        let world =
            {
                defaultWorld() with
                    Box = makeBox 5 5
                    Commands = commands
            } |> processAllCommands
        Assert.AreEqual(["11E"],world.Logs)


