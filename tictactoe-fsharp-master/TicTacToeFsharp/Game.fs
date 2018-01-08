module Game


type Player = {
    Number:int
    Element:string
}

let PlayerX = { Number = 1; Element = "X" }
let PlayerO = { Number = 2; Element = "0" }

let CreateGameboard size = Array2D.create<string> size size "-"
let CountElement element array = Array.filter (fun x -> x = element) array |> Array.length
let GetRow rowNumber (gameboard : string [,]) = gameboard.[rowNumber, *]
let GetColumn colNumber (gameboard : string [,]) = gameboard.[*, colNumber]
let HasFilledRow element gameboard =
    let boardsize = Array2D.length1 gameboard
    [|0..boardsize-1|]
    |> Array.map (fun x -> GetRow x gameboard |> CountElement element = boardsize)
    |> Array.contains true

let HasFilledColumn element gameboard =
    let boardsize = Array2D.length1 gameboard
    [|0..boardsize-1|]
    |> Array.map (fun x -> GetColumn x gameboard |> CountElement element = boardsize)
    |> Array.contains true

let HasFilledUpperLeftToLowerRightDiagonal element gameboard = 
    let boardsize = Array2D.length1 gameboard
    let flatten (A : 'a [,]) = A |> Seq.cast<'a>
    gameboard
    |> Array2D.mapi (fun i j x -> i = j && x = element)
    |> flatten
    |> Seq.filter (fun x -> x = true)
    |> Seq.length
    = boardsize

let HasFilledUpperRightToLowerLeftDiagonal element gameboard =
    let boardsize = Array2D.length1 gameboard
    let flatten (A : 'a [,]) = A |> Seq.cast<'a>
    let seqX = [| 0..boardsize - 1 |]
    let seqY = Array.rev seqX
    gameboard
    |> Array2D.mapi (fun i j x -> i = seqX.[i] && j = seqY.[i] && x = element)
    |> flatten
    |> Seq.filter (fun x -> x = true)
    |> Seq.length
    = boardsize

let PrintGameboard gameboard =
    let boardsize = Array2D.length1 gameboard 
    [| 0..boardsize - 1 |] |> Array.iter (fun x -> 
                                  GetRow x gameboard |> Array.iteri (fun i y -> 
                                                            match i = boardsize - 1 with
                                                            | true -> printfn " | %s |" y
                                                            | false -> printf " | %s" y))

let InputCoordinatesValid gameboard coordX coordY =
    let outsideBounds = (coordX >= Array2D.length1 gameboard) || (coordY >= Array2D.length2 gameboard) || (coordX < 0) || (coordY < 0)
    if not outsideBounds then
        let placeFree = not (gameboard.[coordX, coordY] <> "-")
        placeFree
    else
        false

let MakeMove (gameboard : string [,]) (player : Player) coordX coordY =
    if InputCoordinatesValid gameboard coordX coordY then
        gameboard.[coordX, coordY] <- player.Element
        gameboard, true
    else
        gameboard, false

let IsGameWon gameboard =
    //This could be highly optimized, lazy impl. for now...
    let rowFilled = (HasFilledRow PlayerX.Element gameboard) || (HasFilledRow PlayerO.Element gameboard)
    let columnFilled = (HasFilledColumn PlayerX.Element gameboard) || (HasFilledColumn PlayerO.Element gameboard)
    let diagonal1Filled = (HasFilledUpperLeftToLowerRightDiagonal PlayerX.Element gameboard) || (HasFilledUpperLeftToLowerRightDiagonal PlayerO.Element gameboard)
    let diagonal2Filled = (HasFilledUpperRightToLowerLeftDiagonal PlayerX.Element gameboard) || (HasFilledUpperRightToLowerLeftDiagonal PlayerO.Element gameboard)
    
    rowFilled || columnFilled || diagonal1Filled || diagonal2Filled

let IsGameADraw gameboard =
    let flatten (A : 'a [,]) = A |> Seq.cast<'a>
    gameboard
    |> Array2D.map(fun x -> x = "-")
    |> flatten
    |> Seq.filter (fun x -> x = true)
    |> Seq.length
    = 0