open Common
open Types

(** Signals a waiting request with its corresponding response
   if it exists. Otherwise returns None. *)
let handle_response node res =
  let open Message in
  match Hashtbl.find_opt !node.request_table res.id with
  | Some waiting_request -> Lwt_condition.signal waiting_request res
  | None -> ()

let log node msg =
  let current_time =
    Unix.time () |> Unix.localtime |> fun tm ->
    Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec in
  let addr = Printf.sprintf "%s:%d" !node.address.address !node.address.port in
  Lwt_io.printf "[%s @ %s] %s" addr current_time msg

(* Preprocess a message, log some information abouit it, then handle it
   based on its category. The "rules" are as follows:

   Response: send the message to a "handle_response" function which wakes
   up a sleeping request function with the response it was waiting for.

   Request: run the message handler on the incoming request and, if the message
   handler returned a response, send it to the requester.

   Failure_detection: send the message to the Failure_detector.handle_message function

   Post: check if the post has been seen (or if its outdated). If not, then handle the post with
   the message handler, then disseminate it to this node's peers by reposting it.

   Otherwise, we just apply the message handler and that's it.
*)
let process_message node message preprocessor msg_handler =
  let open Message in
  let message = preprocessor message in
  let%lwt () =
    log node
      (Printf.sprintf "Processing message of category %s from %d...\n"
         (Message.show_category message.category)
         message.sender.port) in
  let%lwt () =
    match message.category with
    | Response -> Lwt.return (handle_response node message)
    | Request -> (
      let%lwt () =
        log node
          (Printf.sprintf "%s:%d : Processing request from %s:%d\n"
             !node.address.address !node.address.port message.sender.address
             message.sender.port) in
      let%lwt state = Mutex.lock !node.state in
      match msg_handler state message with
      | Some response ->
        Mutex.unlock !node.state;
        response
        |> Client.create_response node message
        |> Networking.send_to node
      | None ->
        Mutex.unlock !node.state;
        Lwt.return ())
    | Failure_detection -> Failure_detector.handle_message node message
    | Post ->
      if not (Disseminator.seen !node.disseminator message) then (
        let%lwt () =
          log node
            (Printf.sprintf "%s:%d : Processing post from %s:%d\n"
               !node.address.address !node.address.port message.sender.address
               message.sender.port) in
        let%lwt state = Mutex.lock !node.state in
        let _ = msg_handler state message in
        let%lwt () = log node "Adding message to broadcast queue\n" in
        Client.post node message;
        Lwt.return (Mutex.unlock !node.state))
      else
        log node "Got post but saw it already\n"
    | _ ->
      let%lwt state = Mutex.lock !node.state in
      let _ = msg_handler state message in
      Lwt.return (Mutex.unlock !node.state) in
  Lwt.return ()

let print_logs node =
  let open Message in
  let%lwt () = log node "Running server\n" in
  let%lwt () =
    !node.disseminator
    |> Disseminator.broadcast_queue
    |> List.map (fun { category; _ } -> Message.show_category category)
    |> String.concat " "
    |> Printf.sprintf "Broadcast Queue: %s\n"
    |> log node in
  Lwt.return ()

(* Spend n seconds trying to receive a new message *)
let get_new_message node n message_ref =
  let get_msg node =
    let%lwt message = Networking.recv_next node in
    Lwt.return (message_ref := Some message) in
  Lwt.pick [get_msg node; Lwt_unix.sleep n]

(* Sever procedure:
   0. Log the current node address, time, and broadcast queue. These are all things
      that can be logged irrespective of whether or not a new message arrives.
   1. Run the failure detector
   2. If 2 seconds have passed since the last time the disseminator was run, run the disseminator
      again.
   3. Spend no more than 3 seconds trying to receive a new message.
   4. If a message was received, process it with the given preprocessor and msg_handler.
      If the new message is a request, this may result in a response being issued. If the
      new message is a post, the post may be disseminated further.
   5. Repeat the process *)
let rec run last node preprocessor msg_handler =
  (* Step 0 *)
  let%lwt () = print_logs node in

  (* Step 1 *)
  let%lwt () = Failure_detector.failure_detection node in

  let%lwt () = Networking.disseminate node in

  (* Step 2 *)
  (* let%lwt last =
     if Unix.gettimeofday () -. last >= 0.2 then
       let%lwt () = Lwt_io.printf "Disseminating top\n" in
       let%lwt () = Networking.disseminate node in
       Lwt.return (Unix.gettimeofday ())
     else
       Lwt.return last in *)

  (* let localtime = Unix.localtime last in
     let currenttime = Unix.localtime (Unix.gettimeofday ()) in
     let%lwt () = Lwt_io.printf "Current: %02d:%02d:%02d\n" (currenttime.tm_hour) (currenttime.tm_min) (currenttime.tm_sec) in
     let%lwt () = Lwt_io.printf "Last: %02d:%02d:%02d\n" (localtime.tm_hour) (localtime.tm_min) (localtime.tm_sec) in
     let%lwt () = Lwt_io.printf "Diff: %f\n" (Unix.gettimeofday () -. last) in *)

  (* Step 3 *)
  let new_message = ref None in
  let%lwt () = get_new_message node 3. new_message in

  (* Step 4 *)
  let%lwt () =
    match !new_message with
    | Some msg -> process_message node msg preprocessor msg_handler
    | None -> Lwt.return () in

  (* Step 5 *)
  run last node preprocessor msg_handler
