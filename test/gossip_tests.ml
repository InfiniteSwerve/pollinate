open Lwt.Infix
open Commons
open Pollinate
open Pollinate.Node

module Gossip_tests = struct
  let local_address port = Address.{ address = "127.0.0.1"; port }

  (* Initializes a group of nodes connected as shown here: https://tinyurl.com/tcy8dxu8 *)
  let ( node_a,
        node_b,
        node_c,
        node_d,
        node_e,
        node_f,
        node_g,
        node_h,
        node_i,
        node_j ) =
    Lwt_main.run
      begin
        let ( addr_a,
              addr_b,
              addr_c,
              addr_d,
              addr_e,
              addr_f,
              addr_g,
              addr_h,
              addr_i,
              addr_j ) =
          ( local_address 4000,
            local_address 4001,
            local_address 4002,
            local_address 4003,
            local_address 4004,
            local_address 4005,
            local_address 4006,
            local_address 4007,
            local_address 4008,
            local_address 4009 ) in

        let%lwt node_a =
          Node.init ~state:[]
            ~init_peers:[addr_b; addr_c; addr_e; addr_h]
            addr_a in
        let%lwt node_b =
          Node.init ~state:[] ~init_peers:[addr_a; addr_d; addr_e] addr_b in
        let%lwt node_c =
          Node.init ~state:[] ~init_peers:[addr_a; addr_f; addr_g] addr_c in
        let%lwt node_d = Node.init ~state:[] ~init_peers:[addr_b] addr_d in
        let%lwt node_e =
          Node.init ~state:[] ~init_peers:[addr_a; addr_b] addr_e in
        let%lwt node_f = Node.init ~state:[] ~init_peers:[addr_c] addr_f in
        let%lwt node_g = Node.init ~state:[] ~init_peers:[addr_c] addr_g in
        let%lwt node_h =
          Node.init ~state:[] ~init_peers:[addr_a; addr_i; addr_j] addr_h in
        let%lwt node_i = Node.init ~state:[] ~init_peers:[addr_h] addr_i in
        let%lwt node_j = Node.init ~state:[] ~init_peers:[addr_h] addr_j in
        Lwt.return
          ( node_a,
            node_b,
            node_c,
            node_d,
            node_e,
            node_f,
            node_g,
            node_h,
            node_i,
            node_j )
      end

  let nodes =
    [
      node_a;
      node_b;
      node_c;
      node_d;
      node_e;
      node_f;
      node_g;
      node_h;
      node_i;
      node_j;
    ]
  let node_ports nodes =
    List.map
      (fun n ->
        let addr = Client.address_of !n in
        addr.port)
      nodes

  let disseminate_from node =
    let _ =
      List.map
        (Node.run_server ~preprocessor:Commons.preprocessor
           ~msg_handler:Commons.msg_handler)
        nodes in
    let message = String.to_bytes "hello" |> Client.create_post node in
    Client.post node message;

    let%lwt () = Lwt_unix.sleep (0.2 *. 10.) in

    let all_seen () =
      nodes
      |> List.filter (fun n -> Node.seen n message)
      |> node_ports
      = node_ports nodes in

    let rounds = ref 0 in

    let%lwt () =
      while%lwt !rounds < 4 && not (all_seen ()) do
        rounds := !rounds + 1;
        Lwt_unix.sleep (0.2 *. 10.)
      done in

    nodes
    |> List.filter (fun n -> Node.seen n message)
    |> node_ports
    |> Lwt.return
end

let test_disseminate_from_a _ () =
  Gossip_tests.disseminate_from Gossip_tests.node_a
  >|= Alcotest.(check (list int))
        "All nodes have seen the message"
        Gossip_tests.(node_ports nodes)

let test_disseminate_from_b _ () =
  Gossip_tests.disseminate_from Gossip_tests.node_b
  >|= Alcotest.(check (list int))
        "All nodes have seen the message"
        Gossip_tests.(node_ports nodes)

let test_disseminate_from_c _ () =
  Gossip_tests.disseminate_from Gossip_tests.node_c
  >|= Alcotest.(check (list int))
        "All nodes have seen the message"
        Gossip_tests.(node_ports nodes)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Client tests"
       [
         ( "gossip dissemination",
           [
             Alcotest_lwt.test_case "Dissemination from B" `Quick
               test_disseminate_from_b;
             Alcotest_lwt.test_case "Dissemination from C" `Quick
               test_disseminate_from_c;
           ] );
       ]
