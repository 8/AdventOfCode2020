open System
open System.IO
open System.Text.RegularExpressions

module Day1 =

  let exampleInput = [| 1721; 979; 366; 299; 675; 1456 |]
  let filePath = "../../../day1.txt"
  let realInput () = File.ReadLines(filePath) |> Seq.toArray |> Array.map int
  
//  let printDistinct elements =
//    let distinctElements = elements |> Array.distinct |> Array.length
//    printfn $"{distinctElements}/{elements.Length}"

//  printDistinct exampleInput
//  printDistinct (realInput ())

  module Part1 =
    let result (entries : int * int) = fst entries * snd entries 
    let findEntries (array : int array) : (int * int) =
      let isMatch (i1, i2) = i1 + i2 = 2020 
      array
      |> Array.allPairs array
      |> Array.find isMatch

    let run () =
      let exampleSolution = findEntries exampleInput |> result
      let solution = findEntries (realInput ()) |> result
      printfn $"Day1 - Part1 - Solution for Example Input: {exampleSolution}"
      printfn $"Day1 - Part1 - Solution for Real Input: {solution}"

  module Part2 =
    let result (entries : int * int * int) =
      let (i1, i2, i3) = entries
      i1 * i2 * i3
      
    let findEntries (array : int array) : (int * int * int) =
      let isMatch (i1, i2, i3) = i1 + i2 + i3 = 2020
      let allTuples = seq {
        for item1 in array do
          for item2 in array do
            for item3 in array do
              if isMatch (item1, item2, item3) then
                yield (item1, item2, item3)
      }
      allTuples |> Seq.head

    let run () =
      let exampleSolution = findEntries exampleInput |> result
      let solution = findEntries (realInput ()) |> result
      printfn $"Day1 - Part2 - Solution for Example Input: {exampleSolution}"
      printfn $"Day1 - Part2 - Solution for Example Input: {solution}"

//Day1.Part1.run ()
//Day1.Part2.run ()

module Day2 =
  let exampleInput = [| "1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc" |]
  let filePath = "../../../day2.txt"
  let lines () = File.ReadAllLines filePath |> Seq.toArray
  
  module Part1 =
    
    type Policy = {
      Min : int
      Max : int
      Char: char
    } 
    module Policy =
      let fromString (s: string) =
        let regex = Regex @"^(?<min>\d+)-(?<max>\d+)\s(?<char>[A-z])$"
        let m = regex.Match s
        let min = m.Groups.["min"].Value |> int
        let max = m.Groups.["max"].Value |> int
        let char = m.Groups.["char"].Value.[0]
        {
          Min = min
          Max = max
          Char = char
        }
      let print policy =
        printfn $"Min: {policy.Min}, Max: {policy.Max}, Char: {policy.Char}"
      let isValid policy (password : string) : bool =
        password.ToCharArray()
        |> Array.filter (fun c -> c = policy.Char)
        |> Array.length
        |> (fun count -> (count >= policy.Min && count <= policy.Max))

    let countValid entries =
      let isValid (entry : string) =
        let parts = entry.Split(": ", StringSplitOptions.None)
        let policy, password = Policy.fromString(parts.[0]), parts.[1]
        Policy.isValid policy password
      entries
      |> Array.filter isValid
      |> Array.length

    let run () =
      let exampleSolution = exampleInput |> countValid
      let solution = lines () |> countValid
      printfn $"Day2 - Part1 - Solution for ExampleInput: {exampleSolution}"
      printfn $"Day2 - Part1 - Solution for Real Input: {solution}"

  module Part2 =
    type Policy = {
      Pos1 : int
      Pos2 : int
      Char : char
    }
    
    module Policy =
      let fromString (s : string) : Policy =
        let regex = Regex @"^(?<pos1>\d+)-(?<pos2>\d+)\s(?<char>[A-z])$"
        let m = regex.Match s
        let pos1 = m.Groups.["pos1"].Value |> int
        let pos2 = m.Groups.["pos2"].Value |> int
        let char = m.Groups.["char"].Value.[0]
        {
          Pos1 = pos1
          Pos2 = pos2
          Char = char
        }
    
    type Entry = {
      Policy : Policy
      Password : string
    }
    
    module Entry =
      let fromString (s : string) : Entry =
        let parts = s.Split(": ")
        let (s1, s2) = parts.[0], parts.[1]
        {
          Policy = Policy.fromString(s1)
          Password = s2
        }
      let isValid (entry : Entry) : bool =
        let isPositionValid3 (password : string) (char : char) (pos : int) =
          password.[pos-1] = char
        let isPositionValid = isPositionValid3 entry.Password entry.Policy.Char
        isPositionValid entry.Policy.Pos1 <> isPositionValid entry.Policy.Pos2
    
    let validEntries (lines : string array) : int =
      lines
      |> Array.map Entry.fromString
      |> Array.filter Entry.isValid 
      |> Array.length

    let run () =
      let exampleSolution = exampleInput |> validEntries
      let solution = (lines ()) |> validEntries
      printfn $"Day2 - Part2 - Solution for ExampleInput: {exampleSolution}"
      printfn $"Day2 - Part2 - Solution for Real Input: {solution}"

//Day2.Part1.run ()
//Day2.Part2.run ()

module Day3 =
  
  module Part1 =

    type Position = {
      X : int
      Y : int
    }
    type MapOfTrees = char [,]
    module MapOfTrees =
      let fromLines (lines : string array) =
        let width, height = lines.[0].Length, lines.Length 
        let a = Array2D.create width height ' '
        let setChar y x char = a.[x,y] <- char
        let setLine y (line:string) = line.ToCharArray() |> Array.iteri (setChar y)
        lines |> Array.iteri setLine
        a

      let fromFilePath (filePath : string) : MapOfTrees = 
        let lines = File.ReadLines filePath |> Seq.toArray
        fromLines lines

      let is (c : char) (map : MapOfTrees) (pos : Position) : bool =
        map.[pos.X % map.GetLength(0), pos.Y] = c
      
      let isTree = is '#'
      let isFree = is '.'
     
    type Movement = {
      Right : int
      Down : int
    }

    module Position =
      let move (position : Position) (movement : Movement) : Position =
        {
          X = position.X + movement.Right
          Y = position.Y + movement.Down
        }

    let movement = {
      Right = 3
      Down = 1
    }
    
    let countTrees2 (movement : Movement) (map : MapOfTrees) : int =
      let mutable pos = {
        X = 0
        Y = 0
      }
      let isLastLine y = y = map.GetLength(1) - 1
      
      let mutable treeCount = 0
      while not (isLastLine pos.Y) do
        pos <- Position.move pos movement
        treeCount <- treeCount +
          match MapOfTrees.isTree map pos with
          | true -> 1
          | false -> 0
      treeCount
      
    let countTrees = countTrees2 movement
      
    let run () =
      
      let exampleSolution = (MapOfTrees.fromFilePath "../../../day3-testinput.txt") |> countTrees
      let realSolution = (MapOfTrees.fromFilePath "../../../day3-realinput.txt" ) |> countTrees
      printfn $"Day 3 - Part1 - Solution for ExampleInput: {exampleSolution}"
      printfn $"Day 3 - Part1 - Solution for RealInput: {realSolution}"
    

Day3.Part1.run ()