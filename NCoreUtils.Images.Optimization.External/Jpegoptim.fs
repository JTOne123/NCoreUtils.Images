namespace NCoreUtils.Images.Optimization

open System.Diagnostics
open System.IO
open System.Text
open NCoreUtils
open NCoreUtils.Images
open System

type JpegoptimOptimization (logger : ILog<JpegoptimOptimization>) =

  static let asyncCopyTo (counter : int64 ref) (destination : Stream) (source : Stream) = async {
    let  buffer = Array.zeroCreate 4096
    let! read0 = source.AsyncRead (buffer, 0, 4096)
    let  read = ref read0
    while !read <> 0 do
      counter := !counter + (int64 !read)
      do! destination.AsyncWrite (buffer, 0, !read)
      let! readN = source.AsyncRead (buffer, 0, 4096)
      read := readN }


  member __.AsyncResOptimize (input : Stream, output : Stream) =
    let startInfo =
      ProcessStartInfo (
        FileName               = "jpegoptim",
        Arguments              = "-q -f --strip-all --all-progressive --stdin --stdout",
        RedirectStandardInput  = true,
        RedirectStandardOutput = true,
        RedirectStandardError  = true,
        CreateNoWindow         = true,
        UseShellExecute        = false)
    let p = new Process (StartInfo = startInfo)
    match p.Start () with
    | false ->
      eprintfn "Unable to start jpegoptim"
      ImageResizerError.optimizationFailedf "jpegoptim" "Failed to start process with [FileName = %s, Arguments = %s]" startInfo.FileName startInfo.Arguments
      |> Error
      |> async.Return
    | _ ->
      let before = ref 0L
      let after = ref 0L
      async {
        p.EnableRaisingEvents <- true
        let inline kill () = p.Kill ()
        // logger.LogDebug (null, sprintf "Executing jpegoptim on source (%d bytes)" input.Length)
        let stopwatch = Stopwatch ()
        stopwatch.Start ()
        let! asyncOutput =
          async {
            do! asyncCopyTo after output p.StandardOutput.BaseStream
            do! output.AsyncFlush ()
            output.Close () }
          |> Async.StartChildAsTask
        let! asyncError = async {
          use buffer = new MemoryStream ()
          do! Stream.asyncCopyTo buffer p.StandardError.BaseStream
          return buffer.ToArray () |> Encoding.UTF8.GetString } |> Async.StartChildAsTask
        let! asyncInput = async {
          do! asyncCopyTo before p.StandardInput.BaseStream input
          do! p.StandardInput.BaseStream.AsyncFlush ()
          p.StandardInput.BaseStream.Close () }  |> Async.StartChildAsTask
        do! Async.AwaitEvent (p.Exited, kill) |> Async.Ignore
        do! Async.AwaitTask asyncInput
        do! asyncOutput |> Async.AwaitTask
        let! error  = asyncError  |> Async.AwaitTask
        stopwatch.Stop ()
        match p.ExitCode with
        | 0 ->
          let oldSize = !before
          let newSize = !after
          let decrease = (1.0 - (float newSize / float oldSize)) * 100.0
          logger.LogDebug (null, sprintf "Successfully optimized image (jpegoptim, %d bytes -> %d bytes, %.2f%% decrease)" oldSize newSize decrease)
          return Ok ()
        | exitCode ->
          return
            ImageResizerError.optimizationFailedf "jpegoptim" "Process exited with code %d, stderr = %s" exitCode error
            |> Error }
  member this.AsyncOptimize (input, output) = async {
    let! res = this.AsyncResOptimize (input, output)
    return
      match res with
      | Ok result -> result
      | Error err ->
        eprintfn "%A" err
        err.RaiseException () }
  interface IImageOptimization with
    member __.Dispose () = ()
    member __.Supports imageType = StringComparer.OrdinalIgnoreCase.Equals ("jpg", imageType) || StringComparer.OrdinalIgnoreCase.Equals ("jpeg", imageType)
    // member this.AsyncResOptimize data = this.AsyncResOptimize data
    // member this.AsyncOptimize data = this.AsyncOptimize data
    member this.AsyncPerform (input, output) = this.AsyncOptimize (input, output)

