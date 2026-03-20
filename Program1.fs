open System

type 't BTree =
    | Node of 't * 't BTree * 't BTree
    | Nil

let rec insert (tree: 't BTree) (value: 't) : 't BTree
        when 't: comparison =
    match tree with
    | Nil -> Node(value, Nil, Nil)
    | Node(x, left, right) ->
        if value < x then
            Node(x, insert left value, right)
        else
            Node(x, left, insert right value)

let rec sum (tree: int BTree) : int =
    match tree with
    | Nil -> 0
    | Node(x, left, right) ->
        x + sum left + sum right

let rec fold (folder: 'state -> 'a -> 'state)
        (state: 'state) (tree: 'a BTree) : 'state =
    match tree with
    | Nil -> state
    | Node(x, left, right) ->
        let leftState = fold folder state left
        let currentState = folder leftState x
        fold folder currentState right

let digitCount (n: int) : int =
    if n = 0 then 1
    else
        let rec countDigits num =
            if abs num < 10 then 
                1
            else 
                1 + countDigits (num / 10)
        countDigits (abs n)

let countByDigitLength (targetLength: int) (tree: int BTree) : int =
    let folder count value =
        if digitCount value = targetLength then
            count + 1
        else
            count
    fold folder 0 tree

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
    let sourceList =
        List.init count (fun _ -> random.Next(-150, 160))
    
    printfn "Исходный список %A" sourceList
    
    let sourceTree = List.fold insert Nil sourceList
    printfn "\nИсходное дерево:"
    printTree sourceTree
    
    let sourceSum = sum sourceTree
    printfn "\nСумма элементов исходного дерева: %d"
        sourceSum
    
    printfn "\n Подсчет элементов заданной значности "
    let targetLength =
        readInt "Введите значность числа (например, 3 для трехзначных): "
    
    let countByLength =
        countByDigitLength targetLength sourceTree
    printfn "Количество %d-значных чисел в дереве: %d"
        targetLength countByLength
    
    0
