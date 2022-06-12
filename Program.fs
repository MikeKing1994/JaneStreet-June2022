type Square = 
    {
        X: int
        Y: int
        Val: int option
        RegionIndex: int
    }

module Square = 
    let create x y v region = 
        {
            X = x
            Y = y
            Val = v
            RegionIndex = region
        }

    let print s = 
        match s.Val with 
        | None -> sprintf "_"
        | Some i -> sprintf "%d" i

    let printRegion s = sprintf "%d" s.RegionIndex

    let calculateRegion x y = 
        let regions = [
            [ 2; 2; 2; 2; 10; 10; 17; 17; 17; 17 ]
            [ 2; 2; 2; 10; 10; 17; 17; 23; 23; 17 ]
            [ 2; 2; 7; 10; 9; 16; 17; 23; 23; 17 ]
            [ 1; 2; 7; 9; 9; 9; 20; 22; 22; 22 ]
            [ 1; 5; 4; 9; 13; 15; 20; 20; 24; 24 ]
            [ 1; 4; 4; 8; 8; 15; 19; 14; 14; 24 ]
            [ 1; 1; 4; 6; 8; 14; 14; 14; 24; 24 ]
            [ 1; 1; 6; 6; 12; 12; 14; 18; 24; 24 ]
            [ 1; 1; 3; 3; 3; 11; 18; 18; 11; 11 ]
            [ 1; 3; 3; 3; 11; 11; 11; 11; 11; 11 ]
        ]

        regions[y][x]


let mutable grid: Square list = []

module Grid = 
    let appendEmptySquare grid x y region = 
        let s = Square.create x y None region
        s::grid

    let appendSquare grid x y v region = 
        let s = Square.create x y (Some v) region
        s::grid

    let printRow (grid: Square list) i = 
        let row = grid |> List.where (fun s -> s.Y = i) |> List.sortBy (fun s -> s.X) 

        printfn "%s | %s | %s | %s | %s | %s | %s | %s | %s | %s" 
            (Square.print row[0])
            (Square.print row[1])
            (Square.print row[2])
            (Square.print row[3])
            (Square.print row[4])
            (Square.print row[5])
            (Square.print row[6])
            (Square.print row[7])
            (Square.print row[8])
            (Square.print row[9])

    let printRegionRow (grid: Square list) i = 
        let row = grid |> List.where (fun s -> s.Y = i) |> List.sortBy (fun s -> s.X) 

        printfn "%s | %s | %s | %s | %s | %s | %s | %s | %s | %s" 
            (Square.printRegion row[0])
            (Square.printRegion row[1])
            (Square.printRegion row[2])
            (Square.printRegion row[3])
            (Square.printRegion row[4])
            (Square.printRegion row[5])
            (Square.printRegion row[6])
            (Square.printRegion row[7])
            (Square.printRegion row[8])
            (Square.printRegion row[9])

    let print (grid: Square list) = 
        printRow grid 9
        printRow grid 8
        printRow grid 7
        printRow grid 6
        printRow grid 5
        printRow grid 4
        printRow grid 3
        printRow grid 2
        printRow grid 1
        printRow grid 0

        
    let printRegion (grid: Square list) = 
        printRegionRow grid 9
        printRegionRow grid 8
        printRegionRow grid 7
        printRegionRow grid 6
        printRegionRow grid 5
        printRegionRow grid 4
        printRegionRow grid 3
        printRegionRow grid 2
        printRegionRow grid 1
        printRegionRow grid 0

for x in 0..9 do
    for y in 0..9 do 
        grid <-
            match x, y with 
            | 0, 5 -> Grid.appendSquare grid x y 6 (Square.calculateRegion x y) 
            | 1, 9 -> Grid.appendSquare grid x y 3 (Square.calculateRegion x y)
            | 1, 2 -> Grid.appendSquare grid x y 2 (Square.calculateRegion x y)
            | 2, 5 -> Grid.appendSquare grid x y 1 (Square.calculateRegion x y)
            | 3, 6 -> Grid.appendSquare grid x y 1 (Square.calculateRegion x y)
            | 3, 8 -> Grid.appendSquare grid x y 4 (Square.calculateRegion x y)
            | 4, 0 -> Grid.appendSquare grid x y 5 (Square.calculateRegion x y)
            | 5, 9 -> Grid.appendSquare grid x y 7 (Square.calculateRegion x y)
            | 6, 1 -> Grid.appendSquare grid x y 6 (Square.calculateRegion x y)
            | 6, 3 -> Grid.appendSquare grid x y 2 (Square.calculateRegion x y)
            | 7, 4 -> Grid.appendSquare grid x y 3 (Square.calculateRegion x y)
            | 8, 0 -> Grid.appendSquare grid x y 2 (Square.calculateRegion x y)
            | 8, 7 -> Grid.appendSquare grid x y 2 (Square.calculateRegion x y)
            | 9, 4 -> Grid.appendSquare grid x y 6 (Square.calculateRegion x y)
            | _ -> Grid.appendEmptySquare grid x y (Square.calculateRegion x y) 

Grid.print grid
Grid.printRegion grid


        



