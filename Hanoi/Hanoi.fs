module Hanoi

open System

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

type Move = | Left | Middle | Right

//Add a constructor to prevent Pop and Push being same.
type Turn = {Pop : Move; Push : Move}

type Result<'T> =
    | Success of 'T
    | Failure of string

let gameWon (board : Board) : bool =
    board = winningBoard

//Need to remove the copy code that will appear!
let playTurn (board : Board) (playerChoice : Turn) : Result<Board> =
    let pushOntoStack (board : Board) (item : Disk) (location : Move) =
        match location with
        | Left ->
            match board.LeftPeg with
                | hd :: _ when hd.Width < item.Width ->
                    Failure "Can't put a disk with a larger width ontop of a smaller one"
                | _ -> Success {board with LeftPeg = item::board.MiddlePeg}
        | Middle ->
            match board.MiddlePeg with
                | hd :: _ when hd.Width < item.Width ->
                    Failure "Can't put a disk with a larger width ontop of a smaller one"
                | _ -> Success {board with MiddlePeg = item::board.MiddlePeg}
        | Right ->
            match board.RightPeg with
                | hd :: _ when hd.Width < item.Width ->
                    Failure "Can't put a disk with a larger width ontop of a smaller one"
                | _ -> Success {board with RightPeg = item::board.MiddlePeg}

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
    | (Some pop), (Some push) ->
        Success {Pop = pop; Push = push}
    | _ -> Failure "Invalid input! Must be Left, Middle or Right"

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
