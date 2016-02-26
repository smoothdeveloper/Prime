// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open Prime

/// Haskell-style Either type.
type Either<'l, 'r> =
    | Right of 'r
    | Left of 'l

[<RequireQualifiedAccess>]
module Either =
    
    /// Monadic bind for Either.
    let bind a f =
        match a with
        | Right r -> f r
        | Left l -> Left l

    /// Monadic return for Either.
    let returnM a = Right a

    /// Monadic 'return from' for Either.
    let returnFrom a = a

    /// Builds an either monad.
    type EitherBuilder () =
        member inline this.Bind (a, f) = bind a f
        member inline this.Return a = returnM a
        member inline this.ReturnFrom a = returnFrom a
        member this.Using (d, b) = use u = d in b u
        member this.Delay f = f ()
        member this.Zero () = Left ()
        member this.Yield a = Right a
        member this.YieldFrom e = e
        member this.Combine (a, b) =
            match (a, b) with
            | (Right _, Right _) -> Right (a, b)
            | (Right _, Left _) -> Left (a, b)
            | (Left _, Right _) -> Left (a, b)
            | (Left _, Left _) -> Left (a, b)

    /// The computation expression builder for Either.
    let either = EitherBuilder ()

    /// Query whether an Either value is a Left value.
    let isLeft either =
        match either with
        | Right _ -> false
        | Left _ -> true
    
    /// Query whether an Either value is a Right value.
    let isRight either =
        match either with
        | Right _ -> true
        | Left _ -> false

    /// Get the Left value of an Either value, failing if not available.
    let getLeftValue either =
        match either with
        | Right _ -> failwith "Could not get Left value from a Right value."
        | Left l -> l

    /// Get the Right value of an Either value, failing if not available.
    let getRightValue either =
        match either with
        | Right r -> r
        | Left _ -> failwith "Could not get Right value from a Left value."

    /// Get only the Left values of a sequence of an Either value.
    let getLeftValues eithers =
        List.foldBack
            (fun either lefts -> match either with Right _ -> lefts | Left left -> left :: lefts)
            (List.ofSeq eithers)

    /// Get only the Right values of a sequence of an Either value.
    let getRightValues eithers =
        List.foldBack
            (fun either rights -> match either with Right right -> right :: rights | Left _ -> rights)
            (List.ofSeq eithers)

    /// Map over the left side of an Either value.
    let mapLeft mapper either =
        match either with
        | Right r -> Right r
        | Left l -> Left ^ mapper l

    /// Map over the right side of an Either value.
    let mapRight mapper either =
        match either with
        | Right r -> Right ^ mapper r
        | Left l -> Left l

    /// Split a sequences of Either values into a pair of left and right value lists.
    let split eithers =
        List.foldBack
            (fun either (ls, rs) ->
                match either with
                | Right r -> (ls, r :: rs)
                | Left l -> (l :: ls, rs))
            (List.ofSeq eithers)
            ([], [])