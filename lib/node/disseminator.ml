type pool_elt = {
  message : Message.t;
  remaining : int;
}

module DigestSet = Set.Make(Digest)

type t = {
  mutable round : int;
  mutable pool : pool_elt list;
  mutable num_rounds : int;
  mutable seen : DigestSet.t
}

let create num_rounds = { round = 0; pool = []; num_rounds ; seen = DigestSet.empty }

let next_round disseminator =
  disseminator.round <- disseminator.round + 1;

  disseminator.pool <-
    disseminator.pool
    |> List.map (fun ({ remaining; _ } as elt) ->
           { elt with remaining = remaining - 1 })
    |> List.filter (fun elt -> elt.remaining > 0)

let post disseminator message =
  disseminator.pool <-
    { message ; remaining = disseminator.num_rounds }
    :: disseminator.pool;
  let digest_of_post = Message.hash_of message in
  disseminator.seen <- DigestSet.add digest_of_post disseminator.seen

let broadcast_queue disseminator =
    List.map (fun e -> e.message) disseminator.pool

let seen disseminator message =
  let hash = Message.hash_of message in
  DigestSet.mem hash disseminator.seen