open Common
open Common.Util
open Types
open Lwt_unix

let send_to node message =
  let open Message in
  let payload = Encoding.pack Message.bin_writer_t message in
  let len = Bytes.length payload in
  let addr = Address.to_sockaddr message.recipient in
  Mutex.unsafe !node.socket (fun socket ->
      let%lwt _ = sendto socket payload 0 len [] addr in
      Lwt.return ())

let recv_next node =
  let open Lwt_unix in
  let open Util in
  (* Peek the first 8 bytes of the incoming datagram
     to read the Bin_prot size header. *)
  let size_buffer = Bytes.create Encoding.size_header_length in
  let%lwt node_socket = Mutex.lock !node.socket in
  (* Flag MSG_PEEK means: peeks at an incoming message.
     The data is treated as unread and the next recvfrom()
     or similar function shall still return this data.
     Here, we only need the mg_size.
  *)
  let%lwt _ =
    recvfrom node_socket size_buffer 0 Encoding.size_header_length [MSG_PEEK]
  in
  let msg_size =
    Encoding.read_size_header size_buffer + Encoding.size_header_length in
  let msg_buffer = Bytes.create msg_size in
  (* Now that we have read the header and the message size, we can read the message *)
  let%lwt _ = recvfrom node_socket msg_buffer 0 msg_size [] in
  let message = Encoding.unpack Message.bin_read_t msg_buffer in
  Mutex.unlock !node.socket;
  Lwt.return message

(** Basic random shuffle, see https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle*)
let knuth_shuffle known_peers =
  let shuffled_array = Array.copy (Array.of_list known_peers) in
  let initial_array_length = Array.length shuffled_array in
  for i = initial_array_length - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = shuffled_array.(k) in
    shuffled_array.(k) <- shuffled_array.(i);
    shuffled_array.(i) <- x
  done;
  Array.to_list shuffled_array

(* Regarding the SWIM protocol, if peer A cannot get ACK from peer B (timeout):
    A sets B as `suspicious`
    A randomly picks one (or several, should it also be randomly determined?) peer(s) from its list
    and ask him/them to ping B.*)

(** This function return the random peer, to which we will ask to ping the first peer *)
let rec pick_random_neighbors neighbors number_of_neighbors =
  let addresses = neighbors |> Base.Hashtbl.keys |> knuth_shuffle in
  match addresses with
  | [] -> failwith "pick_random_peers"
  | elem :: _ ->
    if number_of_neighbors = 1 then
      [elem]
    else
      elem :: pick_random_neighbors neighbors (number_of_neighbors - 1)

let broadcast node category payload (recipients : Address.t list) =
  recipients
  |> List.map (fun recipient ->
         Message.
           { category; id = -1; sender = !node.address; recipient; payload })
  |> List.map (fun msg ->
         let%lwt () = send_to node msg in
         Lwt.return ())

let disseminate node =
  let dissemination_group = pick_random_neighbors !node.peers 10 in
  let _ =
    Disseminator.broadcast_queue !node.disseminator
    |> List.map (fun (category, payload) ->
           broadcast node category payload dissemination_group)
    |> List.concat in
  Lwt.return ()
