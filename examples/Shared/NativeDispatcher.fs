namespace Ranvier.D5.Examples

open System
open Ranvier.View.WebCore
open Ranvier.D5.View

[<RequireQualifiedAccess>]
module HostDispatcher =
    /// <summary>Uses WebCore's existing owner queue and animation-frame callback. Dispose layout owners before disposing the host.</summary>
    let forHost (host:WebCoreHost) =
        {Post=fun callback ->
            match host.TryPost(callback,Int32.MaxValue) with
            | OwnerPostAdmission.Accepted -> ()
            | _ -> raise(ObjectDisposedException "WebCoreHost")
         RequestFrame=fun callback -> host.RequestAnimationFrame(fun _ -> callback()) |> ignore}
