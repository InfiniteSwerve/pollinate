type pool_elt = {
  message : Message.t;
  remaining : int;
}

module DigestSet = Set.Make (Digest)

type t = {
  round : int;
  pool : pool_elt list;
  num_rounds : int;
  epoch_length : float;
  seen : DigestSet.t;
}

let create ~num_rounds ~epoch_length =
  { round = 0; pool = []; num_rounds; epoch_length; seen = DigestSet.empty }

let next_round disseminator =
  let round = disseminator.round + 1 in
  let pool =
    disseminator.pool
    |> List.map (fun ({ remaining; _ } as elt) ->
           { elt with remaining = remaining - 1 })
    |> List.filter (fun elt -> elt.remaining > 0 && elt.message.timestamp > (Unix.time () -. disseminator.epoch_length)) in

  { disseminator with round; pool }

let post disseminator message =
  let open Message in

  let time = Unix.time () in
  if message.timestamp > (time -. disseminator.epoch_length) then
    let pool =
      { message; remaining = disseminator.num_rounds } :: disseminator.pool in
    let digest_of_post = Message.hash_of message in
    let seen = DigestSet.add digest_of_post disseminator.seen in
    { disseminator with pool; seen }
  else
    disseminator

let broadcast_queue disseminator =
  List.map (fun e -> e.message) disseminator.pool

let seen disseminator message =
  let open Message in
  let time = Unix.time () in
  if message.timestamp > (time -. disseminator.epoch_length) then
    let hash = Message.hash_of message in
    DigestSet.mem hash disseminator.seen
  else
    false