namespace TheStrangerThings
    module GameData =

        type mapSquare = JanitorsCloset | Office | EquipmentRoom | ExitDoor

        type Item = {
                    name:string;
                    }

        type Action = GoNorth | GoSouth | GoWest | GoEast | PickUpItem | DropItem 

        type gameState = { x:int; y:int
                           item: Item option
                           hasWalkietalkie:bool
                           hasRedkey:bool
                           hasPurplekey:bool
                           hasMobilephone:bool
                           hasKnife:bool
                           hasBaseballbat:bool
                           hasSyringe:bool
                           hasLost:bool
                           hasKilledCreature:bool
                           hasEscaped:bool
                           }

        let readInput input =
            match input with
            | "N" -> Some GoNorth
            | "S" -> Some GoSouth
            | "W" -> Some GoWest
            | "E" -> Some GoEast
            | "PICK UP" -> Some PickUpItem
            | "DROP" -> Some DropItem
            | _ -> None

