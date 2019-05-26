module BinaryTrees
open System
open System.Threading.Tasks

type Tree = Leaf | Node of Tree * Tree

let rec makeTree depth =
    match depth with
    | 0 -> Leaf
    | _ ->
        let depth = depth - 1
        Node (makeTree depth, makeTree depth)

let rec checkTree tree =
    match tree with
    | Leaf -> 1
    | Node(left, right) -> 1 + checkTree left + checkTree right

let makeAndCheckTree = makeTree >> checkTree

let rec iterativeCheck depth iterations check =
    match iterations with
    | 0 -> check
    | _ -> iterativeCheck depth (iterations - 1) (check + (makeAndCheckTree depth))

[<EntryPoint>]
let main argv =
    let minDepth = 4
    let maxDepth = max (minDepth + 2) (int argv.[0])
    let stretchDepth = maxDepth + 1
    let stretchTree = Task.Run(fun () ->
        sprintf "stretch tree of depth %i\t check: %i\n" stretchDepth (makeAndCheckTree stretchDepth))
    let longLivedTree = Task.Run(fun () ->
        let longLivedTree = makeTree maxDepth
        sprintf "long lived tree of depth %i\t check: %i\n" maxDepth (checkTree longLivedTree), longLivedTree)
    let iterationTrees = Array.Parallel.init ((maxDepth - minDepth) / 2 + 1) (fun i ->
        let depth = minDepth + 2 * i
        let iterations = 1 <<< (maxDepth  - depth + minDepth)
        sprintf "%d\t trees of depth %d\t check: %i\n" iterations depth (iterativeCheck depth iterations 0))
    stretchTree.Result |> Console.Write
    iterationTrees |> Array.iter Console.Write
    longLivedTree.Result |> fst |> Console.Write
    exit 0
