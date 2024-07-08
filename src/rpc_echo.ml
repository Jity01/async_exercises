open! Core
open Async

module Echo = struct
  module Query = struct
    type t = string [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = { time : Time_ns_unix.t; message : Query.t } [@@deriving sexp_of, bin_io]
  end
end

module RainbowColor = struct
  module T = struct
    type t = Red | Orange | Yellow | Green | Blue | Indigo | Violet [@@deriving sexp, bin_io, equal]
  end
  include T
  include Sexpable.To_stringable (T)
  let arg_type = Command.Arg_type.create of_string
end

let get_colors () = [
  RainbowColor.Red
  ; RainbowColor.Orange
  ; RainbowColor.Yellow
  ; RainbowColor.Blue
  ; RainbowColor.Green
  ; RainbowColor.Indigo
  ; RainbowColor.Violet]

module Rpcs = struct
  let echo_rpc =
    Rpc.Rpc.create
      ~name:"ping"
      ~version:0
      ~bin_query:Echo.Query.bin_t
      ~bin_response:Echo.Response.bin_t
  ;;

  let rainbow_rpc =
    Rpc.Rpc.create
      ~name:"ping_2"
      ~version:0
      ~bin_query:RainbowColor.bin_t
      ~bin_response:RainbowColor.bin_t
end

module Server = struct
  let handle_echo (_client : unit) (query : Echo.Query.t) =
    print_s [%message "Received echo query" (query : Echo.Query.t)];
    let response = { Echo.Response.time = Time_ns_unix.now (); Echo.Response.message = query } in
    return response
  ;;

  let handle_rainbow (_client : unit) (query : RainbowColor.t) =
    print_s [%message "Received rainbow query" (query : RainbowColor.t)];
    let (colors : RainbowColor.t list) = get_colors () in
    let colors_without_target = List.filter colors ~f:(fun color -> not (RainbowColor.equal query color)) in
    let response = List.random_element_exn colors_without_target in
    return response
  ;;

  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:[ Rpc.Rpc.implement Rpcs.echo_rpc handle_echo; Rpc.Rpc.implement Rpcs.rainbow_rpc handle_rainbow  ]
  ;;

  let command =
    Command.async
      ~summary:"Client"
      (let%map_open.Command () = return ()
       and port = flag "-port" (required int) ~doc:"INT server port" in
       fun () ->
         let%bind server =
           Rpc.Connection.serve
             ~implementations
             ~initial_connection_state:(fun _client_identity _client_addr ->
               (* This constructs the "client" values which are passed to the
                  implementation function above. We're just using unit for now. *)
               ())
             ~where_to_listen:(Tcp.Where_to_listen.of_port port)
             ()
         in
         Tcp.Server.close_finished server)
  ;;
end

module Client = struct
  module Echo = struct
    let command =
      Command.async
        ~summary:"client-echo"
        (let%map_open.Command () = return ()
         and server =
          flag "-server" (required host_and_port) ~doc:"host_and_port of remote server"
         and message =
          flag "-message" (required string) ~doc:"message from client"
         in
         fun () ->
           let (query : Echo.Query.t ) = message in
           let%bind response =
             Rpc.Connection.with_client
               (Tcp.Where_to_connect.of_host_and_port server)
               (fun conn -> Rpc.Rpc.dispatch_exn Rpcs.echo_rpc conn query)
           in
           let response = Result.ok_exn response in
           print_s [%message "RESPONSE" (response : Echo.Response.t)];
           return ())
  end

  module Rainbow = struct
    let command =
      Command.async
        ~summary:"client-rainbow"
        (let%map_open.Command () = return ()
         and server =
          flag "-server" (required host_and_port) ~doc:"host_and_port of remote server"
         and color =
          flag "-color" (required RainbowColor.arg_type) ~doc:"ROYGBIV color"
         in
         fun () ->
           let query = color in
           let%bind response =
             Rpc.Connection.with_client
               (Tcp.Where_to_connect.of_host_and_port server)
               (fun conn -> Rpc.Rpc.dispatch_exn Rpcs.rainbow_rpc conn query)
           in
           let response = Result.ok_exn response in
           print_s [%message "RESPONSE" (response : RainbowColor.t)];
           return ())
  end
  ;;
end

let command =
  Command.group ~summary:"RPC" [ "server", Server.command; "client-echo", Client.Echo.command; "client-rainbow", Client.Rainbow.command ]
;;
