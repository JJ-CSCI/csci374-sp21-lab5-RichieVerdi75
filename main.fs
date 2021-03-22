module Assignment

type Tree =
    | Node of Tree * Tree
    | Leaf of int

let rec prod (t:Tree) :int =
    let (l,f) match t with
    | Leaf a -> a // matching the Leaf subtype
    | Node (l, r) -> l * r // or matching the Node subtype
    | _ -> 0
    
let rec map (f:int->int) (t:Tree) :Tree =
    match t with
    | Leaf a -> a
    | Node (l, r) -> l * r
    | _ -> t

let rec foldStr (nf:string -> string -> string) (lf:int->string) (t:Tree) :string =
    match t with
    | Leaf a -> a
    | Node (l, r) -> l * r
    | _ -> ""

// let result (r:Error) :int =
//     match r with
//       OK -> 0
//       Err n -> n
