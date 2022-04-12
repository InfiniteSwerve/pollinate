open Common
open Common.Util
open Types

let route node router msg =
  let open Message in
  let msg = router msg in
  Inbox.push !node.inbox msg.category msg

(** Signals a waiting request with its corresponding response
   if it exists. Otherwise returns None. *)
let handle_response request_table res =
  let open Message in
  let* res in
  let* waiting_request = Hashtbl.find_opt request_table res.id in
  Some (Lwt_condition.signal waiting_request res)

(* Sever procedure:
   1. Receive the next incoming message
   2. Route the message
   3. Grab the next response if it exists and send it to the request waiting for it
   4. Grab the next request if it exists and send it to the message handler along with the
     node's state
   5. Send the encoded response from the message handler to the requester *)
let run node router msg_handler =
  let take_opt l f =
    let rec aux pre post =
      match post with
      | [] -> (None, l)
      | h :: t when f h -> (Some h, pre @ t)
      | h :: t -> aux (pre @ [h]) t in
    aux [] l in

  let take_message new_messages category =
    take_opt new_messages (fun m -> Message.(m.category = category)) in

  let rec server () =
    let%lwt message = Client.recv_next node in
    let%lwt () = route node router message in

    let%lwt new_messages = Inbox.new_messages !node.inbox in
    let%lwt new_messages =
      match take_message new_messages Message.Failure_detection with
      | (Some message, l) ->
        let%lwt () = Failure_detector.handle_message node message in
        Lwt.return l
      | (None, l) -> Lwt.return l in

    let%lwt () = Failure_detector.failure_detection node in
    let%lwt () = Failure_detector.suspicious_detection node in

    let request = List.find_opt (fun m -> Message.(m.category = Message.Request)) new_messages in
    let (next_response, new_messages) = take_message new_messages Message.Response in
    let _ = handle_response !node.request_table next_response in

    let%lwt () =
      let%lwt state = Mutex.lock !node.state in
      match (request, (msg_handler state new_messages)) with
      | (Some request, Some response) ->
        let response = Client.create_response node request response in
        let%lwt () = Client.send_to node response in
        Lwt.return ()
      | _ -> Lwt.return ()
      in
    Mutex.unlock !node.state;
    server () in
  Lwt.async server
