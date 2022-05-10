open Common
open Common.Util
open Types
open Lwt_unix

let send_to node message =
  let open Message in
  let payload = Encoding.pack Message.bin_writer_t message in
  let len = Bytes.length payload in
  let addrs = List.map Address.to_sockaddr message.recipients in
  Mutex.unsafe !node.socket (fun socket ->
      let%lwt _ =
        Lwt_list.map_p (fun addr -> sendto socket payload 0 len [] addr) addrs
      in
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

let rec take n l =
  match l with
  | [] -> []
  | h :: t when n > 0 -> h :: take (n - 1) t
  | _ -> []

(** This function return the random peer, to which we will ask to ping the first peer *)
let pick_random_neighbors neighbors number_of_neighbors =
  neighbors |> Base.Hashtbl.keys |> knuth_shuffle |> take number_of_neighbors
(* match addresses with
   | [] -> []
   | elem :: _ ->
     if number_of_neighbors = 1 then
       [elem]
     else
       elem :: pick_random_neighbors neighbors (number_of_neighbors - 1) *)

let broadcast node message (recipients : Address.t list) =
  let%lwt () =
    recipients
    |> List.map (fun Address.{ port; _ } -> string_of_int port)
    |> String.concat " ; "
    |> Lwt_io.printf "[%d] Disseminating post to peers: [%s]\n"
         !node.address.port in
  let message = Message.{ message with recipients } in
  let%lwt () = send_to node message in
  Lwt.return ()

let disseminate node =
  let dissemination_group = pick_random_neighbors !node.peers 2 in
  let _ =
    Disseminator.broadcast_queue !node.disseminator
    |> List.map (fun message -> broadcast node message dissemination_group)
  in
  Lwt.return (!node.disseminator <- Disseminator.next_round !node.disseminator)