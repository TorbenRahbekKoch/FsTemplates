namespace ColorPuzzle
module GameBoard = 

    type TileColor = TileColor of int
    with member x.value() = 
            match x with
            | TileColor(n) -> n

    type Position = {
        x : int
        y : int
    }

    type Tile = {
        color    : TileColor
        position : Position
    }
        
    let randomGenerator = System.Random()

    let getRandom n =
        randomGenerator.Next n

    let createRandomColor numberOfColors =
        let colorIndex = getRandom numberOfColors
        TileColor colorIndex

    type GameBoard = {
        sizeX         : int
        sizeY         : int  
        currentColor  : TileColor
        currentTiles  : Set<Tile>
        tiles         : Tile array array
        numberOfMoves : int
        score         : int  
    }

    let getNeighborTiles sizeX sizeY position (tiles: Tile array array) =
        let point = (position.x, position.y)
        let leftNeighbor =
            match point with
            | (x, y) when x > 0 -> Some(tiles.[y].[x-1])
            | _ -> None

        let rightNeighbor =
            match point with
            | (x, y) when x < sizeX-1 -> Some(tiles.[y].[x+1])
            | _ -> None
             
        let topNeighbor =
            match point with
            | (x, y) when y > 0 -> Some(tiles.[y-1].[x])
            | _ -> None
             
        let bottomNeighbor =
            match point with
            | (x, y) when y < sizeY-1 -> Some(tiles.[y+1].[x])
            | _ -> None             

        let neighborTiles = [leftNeighbor]
                             @[rightNeighbor]
                             @[topNeighbor]
                             @[bottomNeighbor]
                             |> List.choose id
                             |> List.map(fun tile -> tiles.[tile.position.y].[tile.position.x])
        neighborTiles
                             

    let calculateCurrentTileSet sizeX sizeY (tiles: Tile array array) : Set<Tile> = 
        let rec findSameColoredNeighbors currentTile (alreadyFound: Set<Tile>) : Tile list =
            let neighborTiles = tiles
                                |> getNeighborTiles sizeX sizeY currentTile.position 
                                |> List.filter (fun tile -> tile.color = currentTile.color)
                                |> List.filter (fun item -> not (Set.contains item alreadyFound))

            let nowFound = Set.union (Set.ofList neighborTiles) alreadyFound
            
            neighborTiles
            |> List.collect (fun tile -> findSameColoredNeighbors tile nowFound )
            |> Set.ofList
            |> Set.union alreadyFound
            |> List.ofSeq

        let rootTile = tiles.[0].[0]
        findSameColoredNeighbors rootTile (Set [rootTile]) 
        |> Set.ofList

        
    let createGameBoard sizeX sizeY numberOfColors =
        let tiles = [| for y in 0..sizeY-1 
                        -> [|for x in 0..sizeX-1 
                              -> {color = createRandomColor numberOfColors; position = {x = x; y = y} }|]  |] 
        { 
            currentColor = tiles.[0].[0].color
            sizeX        = sizeX;
            sizeY        = sizeY 
            tiles        = tiles
            currentTiles = calculateCurrentTileSet sizeX sizeY tiles
            numberOfMoves = 0
            score         = 0
        }

    let paintBoardWithColor gameBoard newColor =
        if gameBoard.currentColor = newColor then
            gameBoard
        else
            let isNeighborToActive tile =            
                let neighborTiles = getNeighborTiles gameBoard.sizeX gameBoard.sizeY tile.position gameBoard.tiles
                neighborTiles
                |> List.exists(fun tileToCheck -> Set.contains tileToCheck gameBoard.currentTiles)  

            let sizeX = gameBoard.sizeX
            let sizeY = gameBoard.sizeY

            // Tiles in currentTileSet must  be changed to newColor
            // All other Tiles stay the same
            // and then currentTileSet is recalculated

            let tiles = [| for y in 0..sizeY-1 
                            -> [|for x in 0..sizeX-1 
                                    -> let tile = gameBoard.tiles.[y].[x]
                                       if Set.contains tile gameBoard.currentTiles then
                                            { color = newColor; position = tile.position }
                                       elif isNeighborToActive tile && tile.color = newColor then
                                            { color = newColor; position = tile.position }
                                       else
                                          tile
                               |]
                        |]         
            
            let distinctColors = tiles
                                 |> Array.collect(fun row -> row |> Array.map(fun tile -> tile.color))
                                 |> Seq.distinct
                                 |> List.ofSeq

            let isFinished = distinctColors.Length = 1

            let oldCurrentTilesWithNewColor = Set.map(fun tile -> { tile with color = newColor }) gameBoard.currentTiles
            let newCurrentTiles = calculateCurrentTileSet sizeX sizeY tiles

            if newCurrentTiles = oldCurrentTilesWithNewColor then
                gameBoard
            elif isFinished then
                { gameBoard with 
                    currentColor = newColor
                    tiles = tiles 
                    currentTiles = newCurrentTiles
                    numberOfMoves = gameBoard.numberOfMoves
                    score = gameBoard.score + (gameBoard.sizeX * gameBoard.sizeY) - gameBoard.numberOfMoves
                 }
            else            
                { gameBoard with 
                    currentColor = newColor
                    tiles = tiles 
                    currentTiles = newCurrentTiles
                    numberOfMoves = gameBoard.numberOfMoves + 1
                 }

                    

    module Persistence =

        let tileToList tile =
            [tile.color.value()]
            @[tile.position.x]
            @[tile.position.y]

        let tileRowToList tileRow : int list =
            tileRow 
            |> List.collect(tileToList)

        let tilesToList tiles : int list =
            tiles 
            |> Array.toList
            |> List.collect(fun row -> row |> Array.toList |> tileRowToList)


        let saveGameBoard gameBoard =
            [gameBoard.sizeX]
              @[gameBoard.sizeY]
              @[gameBoard.currentColor.value()]
              @[gameBoard.numberOfMoves]
              @[gameBoard.score]
              @(tilesToList gameBoard.tiles)
              |> List.toArray

        let loadTile (saved: int[]) position =
            { 
                color = TileColor(saved.[position + 0])
                position = { x = saved.[position + 1]; y = saved.[position + 2]}
            }

        let loadGameBoard (saved: int[]) =
            let sizeX = saved.[0]
            let sizeY = saved.[1]
            let currentColor = TileColor(saved.[2])
            let numberOfMoves = saved.[3]
            let score = saved.[4]

            let tileSize = 3
            let tileStart = 5

            let tiles = [| for y in 0..sizeY-1 ->
                            [|for x in 0..sizeX-1 ->
                                let position = tileStart + (y * sizeX * tileSize) + (x * tileSize)
                                loadTile saved position
                            |]
                        |]
            {
                sizeX = sizeX
                sizeY = sizeY
                currentColor = currentColor
                numberOfMoves = numberOfMoves
                score = score
                tiles = tiles
                currentTiles = calculateCurrentTileSet sizeX sizeY tiles
            }

    // Testing save/load:
//        let created = createGameBoard 3 3 10
//        let saved = saveGameBoard created
//        let loaded = loadGameBoard saved
//        if created <> loaded then
//            failwith "Save/Load failed"
