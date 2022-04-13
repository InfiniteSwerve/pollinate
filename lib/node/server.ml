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
let run node preprocessor msg_handler =
  let open Message in
  let rec server () =
    let%lwt message = Client.recv_next node in
    let message = preprocessor message in
    let%lwt () =
      match message.category with
      | Response -> Lwt.return (handle_response node message)
      | Request -> (
        let%lwt state = Mutex.lock !node.state in
        match msg_handler state message with
        | Some response ->
          Mutex.unlock !node.state;
          response |> Client.create_response node message |> Client.send_to node
        | None ->
          Mutex.unlock !node.state;
          Lwt.return ())
      | Failure_detection -> Failure_detector.handle_message node message
      | _ ->
        let%lwt state = Mutex.lock !node.state in
        let _ = msg_handler state message in
        Lwt.return (Mutex.unlock !node.state) in

    let%lwt () = Failure_detector.failure_detection node in
    server () in
  Lwt.async server
