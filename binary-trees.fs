module BinaryTrees

[<AllowNullLiteral>]
type Tree(left: Tree, right: Tree) =
    member __.Check() =
        if isNull left then
            1
        else
            1 + left.Check() + right.Check()

let rec makeTree depth =
    match depth with
    | 0 -> Tree(null, null)
    | _ ->
        let depth = depth - 1
        Tree(makeTree depth, makeTree depth)

let makeAndCheckTree depth = (makeTree depth).Check()

let rec iterativeCheck depth iterations check =
    match iterations with
    | 0 -> check
    | _ -> iterativeCheck depth (iterations - 1) (check + (makeAndCheckTree depth))

[<EntryPoint>]
let main argv =
    let minDepth = 4
    let maxDepth = max (minDepth + 2) (int argv.[0])
    let stretchDepth = maxDepth + 1
    printf "stretch tree of depth %i\t check: %i\n"
        <| stretchDepth
        <| makeAndCheckTree stretchDepth
    let longLivedTree = makeTree maxDepth
    for depth in minDepth..2..maxDepth do
        let iterations = 1 <<< (maxDepth  - depth + minDepth)
        printf "%d\t trees of depth %d\t check: %i\n"
            <| iterations
            <| depth
            <| iterativeCheck depth iterations 0
    printf "long lived tree of depth %i\t check: %i\n"
        <| maxDepth
        <| longLivedTree.Check()
    0
