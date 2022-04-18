type pool_elt = {
  message : Message.t;
  remaining : int;
}
type t = {
  mutable round : int;
  mutable pool : pool_elt list;
  mutable num_rounds : int;
}

let create num_rounds = { round = 0; pool = []; num_rounds }

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
    :: disseminator.pool

let broadcast_queue disseminator =
    List.map (fun e -> e.message) disseminator.pool
