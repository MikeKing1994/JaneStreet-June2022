type Square = 
    {
        X: int
        Y: int
        Val: int option
    }

module Square = 
    let create x y v = 
        {
            X = x
            Y = y
            Val = v
        }

    let print s = 
        match s.Val with 
        | None -> sprintf "_"
        | Some i -> sprintf "%d" i

let mutable grid: Square list = []

module Grid = 
    let appendEmptySquare grid x y = 
        let s = Square.create x y None
        s::grid

    let appendSquare grid x y v = 
        let s = Square.create x y (Some v)
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

for x in 0..9 do
    for y in 0..9 do 
        grid <-
            match x, y with 
            | 0, 5 -> Grid.appendSquare grid x y 6
            | 1, 9 -> Grid.appendSquare grid x y 3
            | 1, 2 -> Grid.appendSquare grid x y 2
            | 2, 5 -> Grid.appendSquare grid x y 1
            | 3, 6 -> Grid.appendSquare grid x y 1
            | 3, 8 -> Grid.appendSquare grid x y 4
            | 4, 0 -> Grid.appendSquare grid x y 5
            | 5, 9 -> Grid.appendSquare grid x y 7
            | 6, 1 -> Grid.appendSquare grid x y 6
            | 6, 3 -> Grid.appendSquare grid x y 2
            | 7, 4 -> Grid.appendSquare grid x y 3
            | 8, 0 -> Grid.appendSquare grid x y 2
            | 8, 7 -> Grid.appendSquare grid x y 2
            | 9, 4 -> Grid.appendSquare grid x y 6
            | _ -> Grid.appendEmptySquare grid x y 
        //if x = 0 && y = 5 then grid <- Grid.appendSquare grid x y 6
        //if x = 1 && y = 9 then grid <- Grid.appendSquare grid x y 3
        //if x = 1 && y = 2 then grid <- Grid.appendSquare grid x y 2
        //if x = 2 && y = 5 then grid <- Grid.appendSquare grid x y 1
        //if x = 3 && y = 6 then grid <- Grid.appendSquare grid x y 1
        //if x = 3 && y = 8 then grid <- Grid.appendSquare grid x y 4
        //if x = 4 && y = 0 then grid <- Grid.appendSquare grid x y 5
        //if x = 5 && y = 9 then grid <- Grid.appendSquare grid x y 7
        //if x = 6 && y = 1 then grid <- Grid.appendSquare grid x y 6
        //if x = 6 && y = 3 then grid <- Grid.appendSquare grid x y 2
        //if x = 7 && y = 4 then grid <- Grid.appendSquare grid x y 3
        //if x = 8 && y = 0 then grid <- Grid.appendSquare grid x y 2
        //if x = 8 && y = 7 then grid <- Grid.appendSquare grid x y 2
        //if x = 9 && y = 4 then grid <- Grid.appendSquare grid x y 6
        //else grid <- Grid.appendEmptySquare grid x y 

Grid.print grid


        



