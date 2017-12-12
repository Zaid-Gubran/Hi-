namespace TheStrangerThings
    module Map =

        open System
        open GameData
        open Items
        open Actions

        let map = [[JanitorsCloset; Office]
                   [EquipmentRoom; ExitDoor]]

        let printMap (x:int, y:int) : unit =
            Console.ForegroundColor <- ConsoleColor.Red
            let printCell (isHere, cell) =
                match cell with 
                | JanitorsCloset -> "   Janitor's Closet   "
                | Office -> "   Office   "
                | EquipmentRoom -> "   Equipment Room   "
                | ExitDoor -> "   Exit Door   "
                |> fun s ->
                    if isHere then
                        Console.ForegroundColor <- ConsoleColor.Blue
                        printf "%s" (s.ToUpper())
                        Console.ForegroundColor <- ConsoleColor.Red
                    else
                        printf "%s" s

            let printRow (row: (bool * mapSquare) list) =
                row |> List.iter (fun elem -> (printCell elem))

            map
            |> List.mapi (fun i row ->
                row |> List.mapi (fun j cell -> if i = x && j = y then (true, cell) else (false, cell))
            )
            |> List.iter (fun row ->
                printRow row
                printfn ""
                )

        let randomNumberGenerator = System.Random()
        let randomNumber _ = randomNumberGenerator.Next(100)

        let describeItem gameState =
            match gameState.item with
            | None -> ""
            | Some Walkietalkie -> "Find the primary coloured key to escape"
            | Some Redkey -> "Use this key to escape"
            | Some Purplekey -> "Wrong key"
            | Some MobilePhone -> "Find the primary coloured key to escape"
            | Some Knife -> "Use this as a weapon"
            | Some Baseballbat -> "Use this as a weapon"
            | Some Syringe -> "Use this as a weapon"

        let describeRoom gameState =
            match map.[gameState.x].[gameState.y] with
            | JanitorsCloset -> "\nYou are in the janitor's closet"
            | Office -> "\nYou are in the office"
            | EquipmentRoom -> "\nYou are in the equipment room"
            | ExitDoor -> "\nYou have escaped"

        let setupJanitorsCloset gameState =
            let r = randomNumber()
            let item = 
                match r with
                | x when x < 40 -> None
                | x when x < 65 -> Some Walkietalkie
                | _ ->
                    if gameState.hasWalkietalkie then {gameState with item = None }
                    else {gameState with item = Some Walkietalkie}

        let setupEquipmentRoom gameState =