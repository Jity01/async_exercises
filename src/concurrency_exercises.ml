open! Core
open Async

(* Async can be a complicated thing to reason about, especially if this is your first time
   working with concurrent code. To practice reading and thinking about asynchronous code,
   here are a few demos. Take some time to read through each of the modules. It's worth
   building a solid understanding of async now before diving into the async exercises. *)

let do_a_thing (n : int) : unit Deferred.t =
  let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec n) in
  Core.printf "Finished waiting %i seconds\n%!" n;
  return ()
;;

(* [Async_puzzles] contains some examples of code using async, where it might not be
   obvious what happens or in what order. Some of these examples also feature functions
   from the [Deferred] module that you might not be familiar with yet. Feel free to ask a
   TA if you have any questions.

   For each example, go through the code and see if you can work out what should get
   printed and in what order. How long should the code take to run?

   Does what happens match what you expected? Do you understand why? *)

module Async_puzzles = struct
  module Puzzle0 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let%bind () =
        Deferred.bind (do_a_thing 5) ~f:(fun () ->
          Deferred.bind (do_a_thing 2) ~f:(fun () -> do_a_thing 2))
      in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle1 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let%bind () = do_a_thing 3 in
      let%bind () = do_a_thing 2 in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle2 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let a = do_a_thing 3 in
      let b = do_a_thing 2 in
      let%bind (), () = Deferred.both a b in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle3 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let a = do_a_thing 3 in
      let%bind () = do_a_thing 2 in
      let%bind () = a in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle4 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let b = do_a_thing 2 in
      let%bind () = do_a_thing 3 in
      let%bind () = b in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle5 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let%bind _ = Deferred.all [do_a_thing 2; do_a_thing 2; do_a_thing 2; do_a_thing 4] in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle6 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let _ = do_a_thing 2 in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  module Puzzle7 = struct
    let run () =
      let start = Time_ns_unix.now () in
      let%bind _ = Deferred.any [do_a_thing 1; do_a_thing 2; do_a_thing 3] in
      let stop = Time_ns_unix.now () in
      let time_elapsed = Time_ns_unix.diff stop start in
      Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
      return ()
    ;;
  end

  let create_command_from_run_function run =
    Command.async
      ~summary:"A demo for reasoning about async"
      [%map_open.Command
        let () = return () in
        run]
      ~behave_nicely_in_pipeline:true
  ;;

  let command =
    Command.group
      ~summary:"Some demos for reasoning about async"
      [ "0", create_command_from_run_function Puzzle0.run
      ; "1", create_command_from_run_function Puzzle1.run
      ; "2", create_command_from_run_function Puzzle2.run
      ; "3", create_command_from_run_function Puzzle3.run
      ; "4", create_command_from_run_function Puzzle4.run
      ; "5", create_command_from_run_function Puzzle5.run
      ; "6", create_command_from_run_function Puzzle6.run
      ; "7", create_command_from_run_function Puzzle7.run
      ]
  ;;
end

let command = Async_puzzles.command
