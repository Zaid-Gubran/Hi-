namespace TheStrangerThings
    module Actions =

        open GameData

        let pickupItem gameState item = 
            match item with
            | Walkietalkie -> { gameState with hasWalkietalkie = true }
            | Redkey -> { gameState with hasRedkey = true }
            | Purplekey -> { gameState with hasPurplekey = true }
            | Mobilephone -> { gameState with hasMobilephone = true }
            | Knife -> { gameState with hasKnife = true }
            | Baseballbat -> { gameState with hasBaseballbat = true }
            | Syringe -> { gameState with hasSyringe = true }

        let dropItem gameState item =
            match item with
            | Walkietalkie -> { gameState with hasWalkietalkie = false }
            | Redkey -> { gameState with hasRedkey = false }
            | Purplekey -> { gameState with hasPurplekey = false }
            | Mobilephone -> { gameState with hasMobilephone = false }
            | Knife -> { gameState with hasKnife = false }
            | Baseballbat -> { gameState with hasBaseballbat = false }
            | Syringe -> { gameState with hasSyringe = false}

        let getValueOrDefault defaultValue optionValue = defaultArg optionValue defaultValue

        let takeAction gameState action =
            match action with 
            | GoNorth -> if gameState.x > 0 then { gameState with x = gameState.x-1 } else gameState
            | GoSouth -> if gameState.x < 9 then { gameState with x = gameState.x+1 } else gameState
            | GoWest -> if gameState.x > 0 then { gameState with y = gameState.y-1 } else gameState
            | GoEast -> if gameState.x < 6 then { gameState with y = gameState.y+1 } else gameState
            | PickUpItem -> gameState.item |> Option.map (pickupItem gameState) |> getValueOrDefault gameState
            | DropItem -> gameState.item |> Option.map (dropItem gameState) |> getValueOrDefault gameState





