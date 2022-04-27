open Common
open Types

module Message = Message
module Client = Client
module Failure_detector = Failure_detector

type 'a t = 'a Types.node

let init ~state ?(init_peers = []) Address.{ address; port} =
  let open Util in
  let%lwt socket = Net.create_socket port in
  let peers =
    Base.Hashtbl.create ~growth_allowed:true ~size:0 (module Address) in
  let _ =
    init_peers
    |> List.map (fun addr ->
           Base.Hashtbl.add peers ~key:addr ~data:(Peer.from addr)) in
  let node =
    ref
      {
        address = Address.create address port;
        current_request_id = Mutex.create (ref 0);
        request_table = Hashtbl.create 20;
        socket = Mutex.create socket;
        state = Mutex.create (ref state);
        failure_detector =
          Failure_detector.make
            {
              protocol_period = 9;
              round_trip_time = 3;
              suspicion_time = 9;
              helpers_size = 3;
            };
        peers;
        disseminator = Disseminator.create ~num_rounds:10 ~epoch_length:30.;
      } in
  Lwt.return node

let run_server ?(preprocessor = fun m -> m) ~msg_handler node =
  Server.run (Unix.time ()) node preprocessor msg_handler
