open System

type 't BTree =
    | Node of 't * 't BTree * 't BTree
    | Nil

let rec insert (tree: 't BTree) (value: 't) : 't BTree when 't: comparison =
    match tree with
    | Nil -> Node(value, Nil, Nil)
    | Node(x, left, right) ->
        if value < x then Node(x, insert left value, right)
        else Node(x, left, insert right value)

let rec map (mapping: 'a -> 'b) (tree: 'a BTree) : 'b BTree =
    match tree with
    | Nil -> Nil
    | Node(x, left, right) -> Node(mapping x, map mapping left, map mapping right)

let rec sum (tree: int BTree) : int =
    match tree with
    | Nil -> 0
    | Node(x, left, right) -> x + sum left + sum right

let prependDigit (digit: int) (number: int) : int =
    let rec countDigits n =
        if abs n < 10 then 1
        else 1 + countDigits (n / 10)
    
    let digits = countDigits number
    let multiplier = pown 10 digits
    
    if number >= 0 then digit * multiplier + number
    else -(digit * multiplier + abs number)

let printTree (tree: int BTree) : unit =
    let rec print indent tree =
        match tree with
        | Nil -> ()
        | Node(value, left, right) ->
            print (indent + "  ") right
            printfn "%s%d" indent value
            print (indent + "  ") left
    print "" tree

let rec readInt (prompt: string) : int =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | (true, value) -> value
    | _ ->
        printfn "Ошибка: введите целое число"
        readInt prompt

[<EntryPoint>]
let main args =
    let count = readInt "Введите количество элементов: "
    let random = Random()
    let sourceList = List.init count (fun _ -> random.Next(-150, 160))
    
    printfn "Исходный список %A" sourceList
    
    let sourceTree = List.fold insert Nil sourceList
    printfn "\nИсходное дерево:"
    printTree sourceTree
    
    let sourceSum = sum sourceTree
    printfn "\nСумма элементов исходного дерева: %d" sourceSum
    
    let digit = readInt "\nВведите цифру для приписывания (0-9): "
    
    let newTree = map (prependDigit digit) sourceTree
    printfn "\nНовое дерево (с приписанной цифрой %d в начало):" digit
    printTree newTree
    
    let newSum = sum newTree
    printfn "\nСумма элементов нового дерева: %d" newSum
    
    0