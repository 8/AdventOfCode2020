open System
open System.Collections.Generic
open System.Diagnostics
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
      
    let run () =
      let countTrees = countTrees2 movement
      let exampleSolution = (MapOfTrees.fromFilePath "../../../day3-testinput.txt") |> countTrees
      let realSolution = (MapOfTrees.fromFilePath "../../../day3-realinput.txt" ) |> countTrees
      printfn $"Day 3 - Part1 - Solution for ExampleInput: {exampleSolution}"
      printfn $"Day 3 - Part1 - Solution for RealInput: {realSolution}"
  
  module Part2 =
    open Part1

    let movements = [|
      { Right = 1; Down = 1 }
      { Right = 3; Down = 1 }
      { Right = 5; Down = 1 }
      { Right = 7; Down = 1 }
      { Right = 1; Down = 2 }
    |]
    
    let run () =
      let result (map : MapOfTrees) : uint64 =
        let countTrees movement = countTrees2 movement map
        movements
        |> Array.map countTrees
        |> Array.fold (fun i1 i2 -> i1 * uint64 i2) 1UL
      
      let exampleSolution = (MapOfTrees.fromFilePath "../../../day3-testinput.txt") |> result
      let realSolution = (MapOfTrees.fromFilePath "../../../day3-realinput.txt" ) |> result
      printfn $"Day 3 - Part2 - Solution for ExampleInput: {exampleSolution}"
      printfn $"Day 3 - Part2 - Solution for RealInput: {realSolution}"

//Day3.Part1.run ()
//Day3.Part2.run ()

module Day4 =
  
  let testFilePath = "../../../day4-testinput.txt"
  let realFilePath = "../../../day4-realinput.txt"

  module Part1 =
    
    type KeyValue = { Key : string; Value : string }
    type Passport = { KeyValues: KeyValue array }
    
    module KeyValue =
      let format kv =
        $"{kv.Key}: {kv.Value}"

      let fromString (s : string) : KeyValue =
        let key, value =
          let parts = s.Split(":")
          parts.[0], parts.[1]
        { Key = key; Value = value }
        
      type Height = {
        Value : int
        Unit: string
      }

    module Passport =
      let fromStrings block =
        let kvs =
          block
          |> Array.filter (fun s -> s <> "")
          |> Array.collect (fun line -> line.Split(" "))
          |> Array.map KeyValue.fromString
          |> Array.sortBy (fun kv -> kv.Key)
        { KeyValues = kvs }
      
      let format passport =
        passport.KeyValues
        |> Array.map KeyValue.format
        |> String.concat Environment.NewLine

    module Passports =
      let fromLines lines =
        let passportBlocks (lines : string seq) : string array seq =
          seq {
            let mutable lineBuffer = []
            for line in lines do
              lineBuffer <- lineBuffer @ [line]
              if line = "" then
                yield lineBuffer |> List.toArray
                lineBuffer <- []
            if lineBuffer.Length <> 0 then
              yield lineBuffer |> List.toArray
          }

        lines
        |> passportBlocks
        |> Seq.map Passport.fromStrings
      
      let fromFile filePath =
        File.ReadLines filePath |> fromLines

    let isValid (passport : Passport) =
        let requiredKeys = set ([| "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; |])
        
        let keys =
          passport.KeyValues
          |> Array.map (fun kv -> kv.Key)
          |> set
          
        let missingKeys =
          keys
          |> Set.difference requiredKeys
          
        missingKeys.IsEmpty || missingKeys.Count = 1 && missingKeys.Contains("cid")

    let countValidPassports (passports : Passport seq) =
      passports
      |> Seq.filter isValid
      |> Seq.length

    let run () =

//      testFilePath
//      |> Passports.fromFile
//      |> Seq.map Passport.format
//      |> Seq.iter (printfn "%s\n")

      let exampleSolution = testFilePath |> Passports.fromFile |> countValidPassports
      let realSolution = realFilePath |> Passports.fromFile |> countValidPassports
      printfn $"Day 4 - Part1 - Number of valid Passports in ExampleInput: {exampleSolution}"
      printfn $"Day 4 - Part1 - Number of valid Passports in RealSolution: {realSolution}"
      
  module Part2 =
    open Part1

    module KeyValue =
      open Part1.KeyValue
      
      let isValid kv =
        let parseYear s =
          match Regex.IsMatch(s, "^\d{4}$") with
          | true -> Some (int s)
          | false -> None
        let between (min : int) (max : int) (n : int) =
          n >= min && n <= max
        let isValidYear min max s =
          s
          |> parseYear
          |> function
            | Some year when year |> between min max -> true
            | _ -> false
        let isValidEyeColor color =
          let validColors = set([|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|])
          validColors.Contains color
        let isValidCid s = true

        let heightFromString s =
          Regex.Match(s, "^(?<height>\d+)(?<unit>.+)$")
          |> function
            | m when m.Success = true -> Some {
              Value = (int m.Groups.["height"].Value)
              Unit = m.Groups.["unit"].Value }
            | _ -> None
        
        let isHeightValid (height : Height) : bool =
          match height.Unit with
          | "cm" -> height.Value |> between 150 193
          | "in" -> height.Value |> between 59 76
          | _ -> false
          
        let isHairColorValid (s : string) =
          Regex.IsMatch(s, "^#[0-9a-f]{6}$")
          
        let isValidPid (s : string) =
          Regex.IsMatch(s, "^\d{9}$")

        match kv.Key with
        | "byr" -> kv.Value |> isValidYear 1920 2002
        | "iyr" -> kv.Value |> isValidYear 2010 2020
        | "eyr" -> kv.Value |> isValidYear 2020 2030
        | "hgt" -> kv.Value |> heightFromString |> Option.map isHeightValid |> Option.defaultValue false
        | "hcl" -> kv.Value |> isHairColorValid
        | "ecl" -> kv.Value |> isValidEyeColor
        | "pid" -> kv.Value |> isValidPid
        | "cid" -> kv.Value |> isValidCid
        | _ -> false

    let isPassportValid passport =
      let isValidPart1 = Part1.isValid
      let isValidPart2 (passport : Passport) =
        let invalidKeyValues = passport.KeyValues |> Array.filter (fun kv -> not (KeyValue.isValid kv))
//        passport.KeyValues |> Array.forall KeyValue.isValid
        invalidKeyValues |> Array.length = 0
      isValidPart1 passport && isValidPart2 passport 

    let countValidPassports (passports : Passport seq) =
      passports
      |> Seq.filter isPassportValid
      |> Seq.length
    
    let run () =
      let exampleSolution = testFilePath |> Passports.fromFile |> countValidPassports
      let realSolution = realFilePath |> Passports.fromFile |> countValidPassports
      printfn $"Day 4 - Part2 - Number of valid Passports in ExampleInput: {exampleSolution}"
      printfn $"Day 4 - Part2 - Number of valid Passports in RealSolution: {realSolution}"

//Day4.Part1.run ()
//Day4.Part2.run ()

module Day5 =
  
  let inputFile = "../../../day5-realinput.txt"
  
  module Part1 =
    
    let parseSeatId (s : string) : int =
      let value index char  =
        match index with
        | 0 | 1 | 2 ->
          match char with
          | 'L' -> 0
          | 'R' -> 1 <<< index
          | _ -> failwith $"Invalid character {char} at Position {index}"
        | _ ->
          match char with
          | 'F' -> 0
          | 'B' -> 1 <<< index
          | _ -> failwith $"Invalid character {char} at Position {index}"
        
      s.ToCharArray()
      |> Array.rev
      |> Array.mapi value
      |> Array.sum

    let test () =  
      let testInput = [|
        "BFFFBBFRRR"
        "FFFBBBFRRR"
        "BBFFBBFRLL"
      |]
      
      testInput
      |> Array.map (fun input -> {| Input = input; SeatId = parseSeatId input |})
      |> Array.iter (fun item -> printfn $"parsed: {item.Input} => {item.SeatId}")

    let run () =
      let lines = File.ReadLines inputFile
      let highestSeatId =
        lines
        |> Seq.map parseSeatId
        |> Seq.max

      printfn $"Day 5 - Part 1 - highest seat ID is: {highestSeatId}"
  
  module Part2 =

    let run () =
      let seatIds =
        File.ReadLines inputFile 
        |> Seq.map Part1.parseSeatId
        |> Seq.toArray
        |> Array.sort

      let findMissingSeatId (seatIds : int array) : int =
        let isIndex (seatIds : int array) (index : int) : bool =
            index > 0 &&
            index + 1 < seatIds.Length &&
            seatIds.[index-1] + 2 = seatIds.[index]
        let index =
          seq { 0 .. seatIds.Length - 1 }
          |> Seq.find (isIndex seatIds)
        seatIds.[index-1] + 1

      let missingSeatId = findMissingSeatId seatIds
      
      printfn $"Day 5 - Part 2 - seat ID is: {missingSeatId}"
    
//Day5.Part1.test ()
//Day5.Part1.run ()
//Day5.Part2.run ()

module Day6 =
  
  let filePathTest = "../../../day6-testinput.txt"
  let filePathReal = "../../../day6-realinput.txt"
  
  module Part1 =

    let group (lines : string array) : string array array =
      lines
        |> Array.indexed
        |> Array.filter (fun (index, line) -> line = "")
        |> Array.map fst
        |> (fun indices -> [|
          yield 0
          yield! indices
          yield lines.Length
        |] )
      |> Array.pairwise
      |> Array.map (fun (prev, curr) ->
        lines.[prev..curr]
        |> Array.filter (fun l -> l <> "")
        )

    let countDistincts (group : string array) : int =
      String.concat "" group
      |> Array.ofSeq
      |> Array.distinct
      |> Array.length

    let solve filePath =
      File.ReadLines filePath 
      |> Seq.toArray
      |> group
      |> Array.map countDistincts
      |> Array.sum

    let run () =
      let exampleSolution = solve filePathTest
      printfn $"Day 6 - Part 1 - Sum of Group Yesses for example Input: {exampleSolution}"
      
      let realSolution = solve filePathReal
      printfn $"Day 6 - Part 1 - Sum of Group Yesses for real Input: {realSolution}"

  module Part2 =

    let solve filePath =
      
      let sets =
        File.ReadAllLines filePath
        |> Part1.group
        |> Array.map (Array.map (fun s -> set (s.ToCharArray())))
      
      sets
      |> Array.map Set.intersectMany
      |> Array.map Set.count
      |> Array.sum
    
    let run () =
      let exampleSolution = solve filePathTest
      printfn $"Day 6 - Part 2 - Sum of Group Everyone Yesses for example Input: {exampleSolution}"
      
      let realSolution = solve filePathReal
      printfn $"Day 6 - Part 2 - Sum of Group Everyone Yesses for real Input: {realSolution}"

//Day6.Part1.run ()
//Day6.Part2.run ()

module Day7 =
  module Part1 =
    
    let bagName = "shiny gold"
    let filePath fileName = Path.Combine("../../../", fileName)
    let testFile = filePath "day7-input0.txt"
    let realFile = filePath "day7-input1.txt"

    type BagRelation = {
      BagName : string
      ContainedBags : (int * string) array
    }
    
    module BagRelation =
      let fromLine line =
        let r = Regex(@"^(?<Name>\w+\s\w+) bags contain (((?<Count>\d+)\s(?<ContainedBag>\w+\s\w+) bags?(,\s)?)+|no other bags)\.$")
        let m = r.Match(line)
        if not m.Success then
          raise <| ArgumentOutOfRangeException(nameof line, $"cannot parse line '{line}'")

        let name = m.Groups.["Name"].Value
        let containedBags =
          let containedBagNames =
            m.Groups.["ContainedBag"].Captures
            |> Seq.toArray
            |> Array.map (fun c -> c.Value)
          let containedBagCounts =
            m.Groups.["Count"].Captures
            |> Seq.toArray
            |> Array.map (fun c -> c.Value)
            |> Array.map int
          containedBagNames |> Array.zip containedBagCounts 
          
        {
          BagName = name
          ContainedBags = containedBags 
        }

    let run () =

      let solve bag file : int =
        let bagRelations =
          File.ReadAllLines file
          |> Array.map BagRelation.fromLine
        
        (* create a lookup from contained bag to container *)
        let contained2Container =
          bagRelations
          |> Array.map (fun bagRelation ->
                        bagRelation.ContainedBags
                        |> Array.map (fun (_, containedBag) -> containedBag, bagRelation.BagName))
          |> Array.concat
          |> Array.groupBy fst
          |> Array.map (fun (group, tuples) -> group, tuples |> Array.map snd)
          |> dict
          
        let countParentBags =
          let getParents (lookup: IDictionary<string, string[]>) bag : string list =
            if lookup.ContainsKey bag then
              lookup.[bag] |> Array.toList
            else
              []

          let rec parentsForBag (lookup : IDictionary<string, string[]>) bag =
            let parents = getParents lookup bag
            let ancestors = List.map (parentsForBag lookup) parents |> List.concat
            parents
            |> List.append ancestors 
            |> List.distinct

          let bags = parentsForBag contained2Container bag

          bags
          |> List.length
        
        countParentBags

      solve bagName testFile
      |> printfn "Day 7 - Part 1 - Count of Outmost bags for example input: %i"
      
      solve bagName realFile
      |> printfn "Day 7 - Part 1 - Count of Outmost bags for example input: %i"

  module Part2 =
    
    let run () =
      
      let solve bagName filePath =
        
        let bagRelations =
          File.ReadLines filePath
          |> Seq.map Part1.BagRelation.fromLine
          |> Seq.toArray

        let lookup =
          bagRelations
          |> Array.map (fun relation -> relation.BagName, relation)
          |> dict
          
        let rec countBags (lookup : IDictionary<string, Part1.BagRelation>) bag =
          lookup.[bag].ContainedBags
          |> Array.map (fun r ->
            snd r
            |> countBags lookup
            |> fun count -> count * fst r
          )
          |> Array.sum
          |> (+) 1

        countBags lookup bagName - 1

      solve Part1.bagName Part1.testFile
      |> printfn "Day 7 - Part 2 - Count the number of bags in test file: %i"
      
      solve Part1.bagName "../../../day7-part2-testinput.txt"
      |> printfn "Day 7 - Part 2 - Count the number of bags in 2nd test fil: %i"
      
      solve Part1.bagName Part1.realFile
      |> printfn "Day 7 - Part 2 - Count the number of bags in real file: %i"

//Day7.Part1.run ()
//Day7.Part2.run ()

module Day8 =

  type Operation = 
    | NOP
    | ACC
    | JMP
  type Instruction = {
    Operation : Operation
    Argument : int
  }
  
  type Program = {
    Instructions : Instruction array
  }
  
  type EipHistory = {
    Set : int Set
    List : int array
  }
  
  type State = {
    ACC : int
    EIP : int
    EipHistory : EipHistory
  }
  
  module Operation =
    let fromString (s : string) =
      match s with
      | "nop" -> NOP
      | "acc" -> ACC
      | "jmp" -> JMP
      | _ -> raise <| ArgumentOutOfRangeException(nameof s, $"Cannot parse operation '{s}'")

  module Instruction =
    let fromLine line =
      let m = Regex.Match(line, @"^(?<op>nop|acc|jmp)\s(?<arg>[-+]\d+)$")
      if not m.Success then
        raise <| ArgumentOutOfRangeException(nameof line, $"Cannot parse instruction '{line}'")
      let op = Operation.fromString m.Groups.["op"].Value
      let arg = m.Groups.["arg"].Value |> int
      {
        Operation = op
        Argument = arg
      }

  module EipHistory =
    let add history eip = {
        history with
          Set = history.Set.Add eip
          List = [|eip|] |> Array.append history.List
        }
    let init = { Set = set([]); List = [||] }
  
  module State =
    let init = { ACC = 0; EIP = 0; EipHistory = EipHistory.init }
    
  module Program =
    let fromFile file =
      File.ReadAllLines file
      |> Array.map Instruction.fromLine
      |> fun instructions -> { Instructions = instructions }

    let step program state =
      let instruction = program.Instructions |> Array.tryItem state.EIP
      match instruction with
      | None -> None
      | Some instruction ->
        match state.EipHistory.Set.Contains state.EIP with
        | true -> None
        | false ->
          match instruction.Operation with
          | ACC -> Some { state with EipHistory = EipHistory.add state.EipHistory state.EIP; EIP = state.EIP + 1; ACC = state.ACC + instruction.Argument; }
          | NOP -> Some { state with EipHistory = EipHistory.add state.EipHistory state.EIP; EIP = state.EIP + 1;}
          | JMP -> Some { state with EipHistory = EipHistory.add state.EipHistory state.EIP; EIP = state.EIP + instruction.Argument;}

    let rec exe program state =
      let newState = step program state
      match newState with
      | None -> state
      | Some s -> exe program s

  module Part1 =

    let run () =
      let solve file =
        let p = Program.fromFile file
        let result = Program.exe p State.init
        result.ACC
      
      solve "../../../day8-input0.txt"
      |> printfn "Day 8 - Part 1 - Accumulator in test input at forever loop: %i"
      
      solve "../../../day8-input1.txt"
      |> printfn "Day 8 - Part 1 - Accumulator in real input at forever loop: %i"

  module Part2 =
    
    let solve file =
      let p = Program.fromFile file
      
      let rec findWrongInstruction program programPatched (patchedHistory : int Set) =
        let hasReachedEnd (state: State) =
          state.EIP = program.Instructions.Length
        let patchProgram (p : Program) index = {
            p with 
              Instructions = [|
                yield! p.Instructions.[0..index-1]
                yield { p.Instructions.[index] with
                          Operation =
                            match p.Instructions.[index].Operation with
                            | NOP -> JMP
                            | ACC -> ACC
                            | JMP -> NOP
                      } 
                yield! p.Instructions.[index + 1..]
              |]
          }
        let findPatchIndex program (state : State) (patchedHistory : int Set) =
          state.EipHistory.List
          |> Array.findBack (fun i ->
            not (patchedHistory.Contains(i)) &&
            match program.Instructions.[i].Operation with
            | JMP -> true
            | NOP -> true
            | ACC -> false)
        
        let state = Program.exe programPatched State.init
        
        if hasReachedEnd state then
          state
        else
          let patchIndex = findPatchIndex program state patchedHistory
          let patchedProgram = patchProgram program patchIndex
          findWrongInstruction program patchedProgram (patchedHistory.Add patchIndex)
        
      let finalState = findWrongInstruction p p Set.empty
      finalState.ACC
    
    let run () =
      solve "../../../day8-input0.txt"
      |> printfn "Day 8 - Part 2 - Accumulator in test input at end: %i"
      
      solve "../../../day8-input1.txt"
      |> printfn "Day 8 - Part 2 - Accumulator in real input at end: %i"
//Day8.Part1.run ()
//Day8.Part2.run ()

module Day9 =
  module Part1 =
    let readNumbers filePath =
      File.ReadAllLines filePath
      |> Array.map int64

    let findInvalidNumber preambleLength numbers =
      numbers
      |> Array.indexed
      |> Array.skip preambleLength
      |> Array.find (fun (index, num) ->
        let previousNumbers = numbers.[index - preambleLength..index - 1]
        Array.allPairs previousNumbers previousNumbers
        |> Array.exists (fun (i1, i2) -> i1 + i2 = num)
        |> not
        )
      |> snd

    let run () =
      let solve preambleLength filePath =
        readNumbers filePath
        |> findInvalidNumber preambleLength
      let printResult preambleLength file =
        solve preambleLength file
        |> printfn "Day 9 - Part 1 - First number that is not the sum of the %i numbers before it: %i" preambleLength

      printResult 5 "../../../day9-input0.txt"
      printResult 25 "../../../day9-input1.txt"

  module Part2 =
    let run () =
      let solve preambleLength filePath =
        let numbers = Part1.readNumbers filePath
        let invalidNumber = Part1.findInvalidNumber preambleLength numbers
        let findContiguosSet (numbers : Int64 array) (number : Int64) =
            seq {
              for length = 2 to numbers.Length do
                numbers
                |> Seq.windowed length
            }
            |> Seq.concat
          |> Seq.find (fun a -> Array.sum a = number) 
        let sumOfSmallesLargest set =
          Array.min set + Array.max set 

        invalidNumber
        |> findContiguosSet numbers
        |> sumOfSmallesLargest

      let printResult preambleLength file =
        solve preambleLength file
        |> printfn "Day 9 - Part 2 - The sum of smallest and largest Number is: %i"

      printResult 5 "../../../day9-input0.txt"
      printResult 25 "../../../day9-input1.txt"

//Day9.Part1.run ()
//Day9.Part2.run ()

module Day10 =
  module Part1 =
    let readAdapters file = File.ReadAllLines file |> Array.map int
    
    let run () =
      let solve file =
        let adapters = readAdapters file
        
        [|
           yield 0
           yield! adapters
           yield (adapters |> Array.max) + 3
        |]
        |> Array.sort
        |> Array.pairwise
        |> Array.map (fun (i1, i2) -> i2 - i1)
        |> Array.groupBy id
        |> fun groups -> (groups |> Array.find (fun (k, v) -> k = 1) |> (fun (k, v) -> v.Length )) *
                         (groups |> Array.find (fun (k, v) -> k = 3) |> (fun (k, v) -> v.Length ))
        
      solve "../../../day10-input0.txt"
      |> printfn "Day 10 - Part 1 - test input 0: %i"

      solve "../../../day10-input1.txt"
      |> printfn "Day 10 - Part 1 - test input 1: %i"
      
      solve "../../../day10-input2.txt"
      |> printfn "Day 10 - Part 1 - real input: %i"
  
  module Part2 =
    let run () =
      let solve file =
        let adapters = File.ReadAllLines file |> Array.map int64
        let jolts = ([|
           yield 0L
           yield! adapters |> Array.sort
           yield (adapters |> Array.max) + 3L
        |])
        
        let diffs = 
          jolts
          |> Array.pairwise
          |> Array.map (fun (i1, i2) -> i2 - i1)

        let streaks =
          diffs
          |> (fun diffs -> [|
            let mutable count = 0L
            let mutable item = None
            for d in diffs do
              if Option.isNone item then
                item <- Some d

              if item = Some d then
                item <- Some d
                count <- count + 1L
              else
                yield Option.get item, count
                count <- 1L
                item <- Some d

            if Option.isSome item then
              yield Option.get item, count
          |])
          
        let streakLength2Multiplier length =
          match length with
          | 1L -> 1L
          | 2L -> 2L
          | 3L -> 4L
          | 4L -> 7L
          | 5L -> 13L
          | _ -> raise <| ArgumentOutOfRangeException (nameof length, $"Mapping for streak Length {length} not implemented")
        
        let result =
          streaks
          |> Array.filter (fun (item, count) -> item = 1L)
          |> Array.map snd
          |> Array.map streakLength2Multiplier
          |> Array.fold (*) 1L
          
        result

      solve "../../../day10-input0.txt"
      |> printfn "Day 10 - Part 2 - test input 0: %i"

      solve "../../../day10-input1.txt"
      |> printfn "Day 10 - Part 2 - test input 1: %i"
      
      solve "../../../day10-input2.txt"
      |> printfn "Day 10 - Part 2 - real input: %i"

//Day10.Part1.run ()
//let sw = Stopwatch.StartNew()
//Day10.Part2.run ()
//sw.Stop()
//printfn $"{sw.ElapsedMilliseconds}ms"

module Day11 =
  type Seat = 
    | Floor
    | Empty
    | Occupied

  module Seat =
    let fromChar c =
      match c with
      | 'L' -> Empty
      | '#' -> Occupied
      | '.' -> Floor
      | _ -> raise <| ArgumentOutOfRangeException (nameof c, $"Unknown character: {c}")
    let print (seats : Seat [,]) =
      for y = 0 to seats.GetLength(1) - 1 do
        for x = 0 to seats.GetLength(0) - 1 do
          let c =
            match seats.[x,y] with
            | Empty -> 'L'
            | Occupied -> '#'
            | Floor -> '.'

          printf "%c" c
        printfn ""
      printfn ""
      ()
    

  module Seats =
    let fromFile filePath =
      let transpose (a : Seat [,]) =
        let a' = Array2D.create (a.GetLength(1)) (a.GetLength(0)) Empty
        for i in 0..a.GetLength(0) - 1 do
          for j in 0..a.GetLength(1) - 1 do
            a'.[j,i] <- a.[i,j]
        a'

      File.ReadAllLines filePath
      |> Array.map (fun line -> line.ToCharArray())
      |> Array.map (Array.map Seat.fromChar)
      |> array2D
      |> transpose

    let tick seats =

      let neighbors (array : Seat[,]) (x, y) =
        let width = array.GetLength(0)
        let height = array.GetLength(1)
        seq {
          for y' = max (y - 1) 0 to min (y + 1) (height - 1) do
            for x' = max (x - 1) 0 to min (x + 1) (width - 1) do
              if not (x' = x && y' = y) then 
                yield array.[x',y']
        }

      let countOccupiedNeighbors (array : Seat[,]) (x,y) =
        neighbors array (x,y)
        |> Seq.map (function | Occupied -> 1 | _ -> 0)
        |> Seq.sum
      
      let seats' = seats |> Array2D.copy

      for x = 0 to seats.GetLength(0) - 1 do
        for y = 0 to seats.GetLength(1) - 1 do
          seats'.[x,y] <-
            match seats.[x,y] with
            | Floor -> Floor
            | Occupied
            | Empty ->
              match countOccupiedNeighbors seats (x, y) with
              | 0 -> Occupied
              | n when n >= 4 -> Empty
              | _ -> seats.[x,y]

      seats'

    let doTick seats =
      seq {
        let mutable seats' = seats
        while true do
          seats' <- tick seats'
          yield seats'
      }
      
    let equals (seats1 : Seat [,]) (seats2 : Seat [,]) =
      if (seats1.GetLength(0) <> seats2.GetLength(0)) ||
         (seats1.GetLength(1) <> seats2.GetLength(1)) then
         false
      else
        seq {
          for x in 0..seats1.GetLength(0) - 1 do
            for y in 0..seats1.GetLength(1) - 1 do
              yield seats1.[x,y] = seats2.[x,y]
        }
        |> Seq.forall id

  module Part1 =
    let run () =
      
      let solve filePath =

        let seats = Seats.fromFile filePath
        
//        seats |> Seat.print
//        Seats.doTick seats
//        |> Seq.take 5
//        |> Seq.iter Seat.print

        seats
        |> Seats.doTick
        |> Seq.pairwise
        |> Seq.find (fun (seats1, seats2) -> Seats.equals seats1 seats2)
        |> snd
//        |> fun s ->
//          Seat.print s
//          s
        |> Seq.cast<Seat>
        |> Seq.toArray
        |> Array.filter (fun seat -> seat = Occupied)
        |> Array.length

      solve "../../../day11-input0.txt"
      |> printfn "Day 11 - Part 1 - real test: %i"
      
      solve "../../../day11-input1.txt"
      |> printfn "Day 11 - Part 1 - real input: %i"
      
Day11.Part1.run ()
