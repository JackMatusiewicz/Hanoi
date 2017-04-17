type Result<'T> =
    | Success of 'T
    | Failure of string

module Result =
    type ResultMonad () =
        member this.Return (x : 'T) = Success x
        member this.ReturnFrom (x : Result<'T>) = x
        member this.Bind ((a : Result<'T>), (f : 'T -> Result<'U>)) =
            match a with
            | Success value -> f value
            | Failure f -> Failure f

module Hanoi =

    open Result
    open System

    type Move = | Left | Middle | Right
    type Turn = {Pop : Move; Push : Move}
    type Disk = {Width : int}
    type Board = {
        LeftPeg : Disk list
        MiddlePeg : Disk list
        RightPeg : Disk list
    }

    let initialBoard : Board = {
        LeftPeg = [{Width = 1}; {Width = 2}; {Width = 3}; {Width = 4}; {Width = 5}]
        MiddlePeg = []
        RightPeg = []
    }

    let winningBoard : Board = {
        LeftPeg = []
        MiddlePeg = []
        RightPeg = [{Width = 1}; {Width = 2}; {Width = 3}; {Width = 4}; {Width = 5}]
    }

    let gameWon (board : Board) : bool =
        board = winningBoard

    let playTurn (board : Board) (playerChoice : Turn) : Result<Board> =
        let pushOntoStack (board : Board) (item : Disk) (location : Move) =
            match location with
            | Left ->
                match board.LeftPeg with
                    | hd :: _ when hd.Width < item.Width ->
                        Failure "Can't put a large disk ontop of a smaller one"
                    | _ -> Success {board with LeftPeg = item::board.LeftPeg}
            | Middle ->
                match board.MiddlePeg with
                    | hd :: _ when hd.Width < item.Width ->
                        Failure "Can't put a large disk ontop of a smaller one"
                    | _ -> Success {board with MiddlePeg = item::board.MiddlePeg}
            | Right ->
                match board.RightPeg with
                    | hd :: _ when hd.Width < item.Width ->
                        Failure "Can't put a large disk ontop of a smaller one"
                    | _ -> Success {board with RightPeg = item::board.RightPeg}

        match playerChoice.Pop with
        | Left ->
            match board.LeftPeg with
            | [] -> Failure "Can't pop off empty peg"
            | hd :: tl ->
                pushOntoStack {board with LeftPeg = tl} hd playerChoice.Push
        | Middle ->
            match board.MiddlePeg with
            | [] -> Failure "Can't pop off empty peg"
            | hd :: tl ->
                pushOntoStack {board with MiddlePeg = tl} hd playerChoice.Push
        | Right ->
            match board.RightPeg with
            | [] -> Failure "Can't pop off empty peg"
            | hd :: tl ->
                pushOntoStack {board with RightPeg = tl} hd playerChoice.Push

    let getPlayerTurn () : Result<Turn> =
        let convertToTurn (turn : string) : Move option =
            match turn with
            | "left" -> Some Left
            | "middle" -> Some Middle
            | "right" -> Some Right
            | _ -> None
        
        printfn "Enter the stack you want to pop from" |> ignore
        let pop = Console.ReadLine()
        printfn "Enter the stack you want to push to" |> ignore
        let push = Console.ReadLine()

        match (convertToTurn pop),(convertToTurn push) with
        | (Some pop), (Some push) -> Success {Pop = pop; Push = push}
        | _ -> Failure "Invalid input! Must be Left, Middle or Right"
    
    let printBoard (board : Board) =
        let printPeg (pegName : string) (pegContents : Disk list) =
            let rec printStack (pegContents : Disk list) =
                match pegContents with
                | [] -> printf "\n"
                | hd :: tl -> (printf "%d " hd.Width); printStack tl
            (printf "%s: " pegName); printStack pegContents
        printfn "-----------------------------"
        printPeg "L" board.LeftPeg
        printPeg "M" board.MiddlePeg
        printPeg "R" board.RightPeg
        printfn "-----------------------------"

    let gameTurn (board : Board) =
        ResultMonad () {
            let! input = getPlayerTurn ()
            let! newBoard = playTurn board input
            printBoard newBoard
            return newBoard
        }

    let startGame () =
        let rec play (board : Board) =
            let newBoard = gameTurn board
            match newBoard with
            | Failure f -> printfn "%s" f; play board
            | Success b -> 
                match b with
                | _ when (gameWon b) ->
                    printfn "CONGRATULATIONS"
                | _ -> play b
        play initialBoard

[<EntryPoint>]
let main argv =
    Hanoi.startGame ()
    0 // return an integer exit code
