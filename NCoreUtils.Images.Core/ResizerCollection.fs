namespace NCoreUtils.Images

open System.Collections
open System.Collections.Concurrent
open System.Collections.Generic
open System.Linq
open System.Runtime.InteropServices
open NCoreUtils
open NCoreUtils.Images.Internal
open System.Runtime.CompilerServices


type
  [<Sealed>]
  ResizerCollectionBuilder =
    val internal collection : ConcurrentDictionary<CaseInsensitive, IResizerFactory>
    new () = { collection = ConcurrentDictionary () }

    member this.Add (name : string, factory : IResizerFactory) =
      match this.collection.TryAdd (CaseInsensitive name, factory) with
      | true -> this
      | _    -> invalidOpf "Resizer factory has already been registered for %s" name
    member this.Build () = ResizerCollection this

and
  [<Sealed>]
  ResizerCollection =
    val private collection : Map<CaseInsensitive, IResizerFactory>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    internal new (builder : ResizerCollectionBuilder) = { collection = builder.collection |> Seq.fold (fun map kv -> Map.add kv.Key kv.Value map) Map.empty }
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryGetValue name =
      let mutable value = Unchecked.defaultof<_>
      match this.TryGetValue (name, &value) with
      | true -> ValueSome value
      | _    -> ValueNone
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.TryGetValue (name : string, [<Out>] factory : byref<_>) =
      this.collection.TryGetValue (CaseInsensitive name, &factory)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member private this.GetEnumerator () = (this.collection :> seq<_>).Select(fun kv -> KeyValuePair (kv.Key.ToLowerString (), kv.Value)).GetEnumerator()
    interface IEnumerable with
      member this.GetEnumerator () = this.GetEnumerator () :> _
    interface IReadOnlyDictionary<string, IResizerFactory> with
      member this.Count   = Map.count this.collection
      member this.Keys    = this.collection |> Seq.map (fun kv -> kv.Key.ToLowerString ())
      member this.Values  = this.collection |> Seq.map (fun kv -> kv.Value)
      member this.Item
        with get name =
          match this.TryGetValue name with
          | ValueSome f -> f
          | _           -> sprintf "No resizer factory registered for %s" name |> KeyNotFoundException |> raise
      member this.ContainsKey name = Map.containsKey (CaseInsensitive name) this.collection
      member this.TryGetValue (name, [<Out>] factory : byref<_>) = this.TryGetValue (name, &factory)
      member this.GetEnumerator () = this.GetEnumerator ()
