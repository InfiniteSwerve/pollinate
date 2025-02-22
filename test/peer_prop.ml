open QCheck2.Gen

module SUT = Pollinate.Peer

let add_peer =
  QCheck2.Test.make ~count:1000
    ~name:"add_peer on empty neighbors leads to size of 1"
    (pair Generators.peer_gen Generators.peer_gen) (fun (peer, neighbor) ->
      let _ = SUT.add_neighbor peer neighbor in
      Base.Hashtbl.length peer.neighbors == 1)

let add_same_peer =
  QCheck2.Test.make ~count:1000
    ~name:"add_peer the same peer more than once, only add it once"
    (pair Generators.peer_gen Generators.peer_gen) (fun (peer, neighbor) ->
      let _ = SUT.add_neighbor peer neighbor in
      let _ = SUT.add_neighbor peer neighbor in
      let _ = SUT.add_neighbor peer neighbor in
      Base.Hashtbl.length peer.neighbors == 1)

let add_peers =
  QCheck2.Test.make ~count:1000
    ~name:
      "add_peers results in neighors having the same length that given \
       neighbors to add"
    (pair Generators.peer_gen (QCheck2.Gen.list Generators.peer_gen))
    (fun (peer, neighbors) ->
      let _ = SUT.add_neighbors peer neighbors in
      Base.Hashtbl.length peer.neighbors == List.length neighbors)

let from =
  QCheck2.Test.make ~count:1000
    ~name:"from function succesfully creates a peer with the given address"
    Generators.address_gen (fun address ->
      (SUT.from address).address == address)

let get_neighbor_empty_hashtbl =
  QCheck2.Test.make ~count:1000
    ~name:"get_neighbor on a peer without neighbor, returns None"
    (pair Generators.peer_gen Generators.address_gen)
    (fun (peer, neighbor_to_find) ->
      let neighbor_opt = SUT.get_neighbor peer neighbor_to_find in
      neighbor_opt == None)

let get_neighbor =
  QCheck2.Test.make ~count:1000
    ~name:
      "get_neighbor successfully returns the correct peer on a valid address"
    (pair Generators.peer_gen Generators.peer_gen)
    (fun (peer, neighbor_to_find) ->
      let _ = SUT.add_neighbor peer neighbor_to_find in
      let neighbor_opt = SUT.get_neighbor peer neighbor_to_find.address in
      match neighbor_opt with
      | Some neighbor -> neighbor.address = neighbor_to_find.address
      | _ -> failwith "unreachable: get_neighbor")

let () =
  let failure_detector_prop =
    List.map QCheck_alcotest.to_alcotest
      [
        add_peer;
        add_same_peer;
        add_peers;
        from;
        get_neighbor_empty_hashtbl;
        get_neighbor;
      ] in
  Alcotest.run "Peer tests" [("peer.ml", failure_detector_prop)]
