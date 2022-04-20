open Common
open Types

(* let route node router msg =
   let open Message in
   let msg = router msg in
   Inbox.push !node.inbox msg.category msg *)

(** Signals a waiting request with its corresponding response
   if it exists. Otherwise returns None. *)
let handle_response node res =
  let open Message in
  match Hashtbl.find_opt !node.request_table res.id with
  | Some waiting_request -> Lwt_condition.signal waiting_request res
  | None -> ()

(* Sever procedure:
   1. Receive the next incoming message
   2. Route the message
   3. Grab the next response if it exists and send it to the request waiting for it
   4. Grab the next request if it exists and send it to the message handler along with the
     node's state
   5. Send the encoded response from the message handler to the requester *)
let rec run last node preprocessor msg_handler =
  let open Message in
  let current_time =
    Unix.time () |> Unix.localtime |> fun tm ->
    Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec in
  let addr = Printf.sprintf "%s:%d" !node.address.address !node.address.port in
  let%lwt () = Lwt_io.printf "[%s @ %s] Running server\n" addr current_time in
  let%lwt () =
    !node.disseminator.Disseminator.pool
    |> List.map (fun p -> p.Disseminator.remaining)
    |> List.map string_of_int
    |> String.concat " "
    |> Lwt_io.printf "%s\n" in
  let%lwt () =
    !node.disseminator
    |> Disseminator.broadcast_queue
    |> List.map (fun { category; _ } -> Message.show_category category)
    |> String.concat " "
    |> Lwt_io.printf "Broadcast Queue: %s\n\n\n" in
  (* let%lwt () =
    !node.peers
    |> Base.Hashtbl.keys
    |> List.map Address.show
    |> String.concat " "
    |> Lwt_io.printf "%s\n" in *)
  let%lwt () = Failure_detector.failure_detection node in
  let%lwt last =
    if Unix.time () -. last > 2.0 then
      let%lwt () = Networking.disseminate node in
      Lwt.return (Unix.time ())
    else
      Lwt.return last
  in
  let process_message =
    let%lwt message = Networking.recv_next node in
    let message = preprocessor message in
    let%lwt () = Lwt_io.printf "Processing message of category %s...\n" (Message.show_category message.category) in
    let%lwt () =
      match message.category with
      | Response -> Lwt.return (handle_response node message)
      | Request -> (
        let%lwt () =
          Lwt_io.printf "%s:%d : Processing request from %s:%d\n"
            !node.address.address !node.address.port message.sender.address
            message.sender.port in
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
            Lwt_io.printf "%s:%d : Processing post from %s:%d\n"
              !node.address.address !node.address.port message.sender.address
              message.sender.port in
          let%lwt state = Mutex.lock !node.state in
          let _ = msg_handler state message in
          let%lwt () = Lwt_io.printf "Adding message to broadcast queue\n" in
          Client.post node message;
          Lwt.return (Mutex.unlock !node.state))
        else
          Lwt_io.printf "Got post but saw it already\n"
      | _ ->
        let%lwt state = Mutex.lock !node.state in
        let _ = msg_handler state message in
        Lwt.return (Mutex.unlock !node.state) in
    Lwt.return () in
  let%lwt () = Lwt.pick [process_message; Lwt_unix.sleep 3.] in
  run last node preprocessor msg_handler
