open! Core

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "concurrency", Concurrency_exercises.command; "rpc-echo", Rpc_echo.command ]
;;
