open Common
open Types

let address_of { address; _ } = address

let peer_from { address; peers; _ } =
  Peer.
    {
      address;
      status = Alive;
      last_suspicious_status = None;
      neighbors = peers;
    }

let add_peer node (peer : Peer.t) =
  Base.Hashtbl.add node.peers ~key:peer.address ~data:peer

let create_request node recipient payload =
  Mutex.with_lock !node.current_request_id (fun id ->
      id := !id + 1;
      Lwt.return
        Message.
          {
            category = Message.Request;
            id = !id;
            sender = !node.address;
            recipient;
            payload;
          })

let create_response node request payload =
  Message.
    {
      category = Message.Response;
      id = request.id;
      sender = !node.address;
      recipient = request.sender;
      payload;
    }

let request node request recipient =
  let%lwt message = create_request node recipient request in
  let%lwt () = Networking.send_to node message in
  let condition_var = Lwt_condition.create () in
  Hashtbl.add !node.request_table message.id condition_var;
  Lwt_condition.wait condition_var

let post node ?category payload =
  match category with
  | Some category -> Disseminator.post !node.disseminator category payload
  | None -> Disseminator.post !node.disseminator Message.Post payload

let broadcast_request node req recipients =
  List.map (request node req) recipients
