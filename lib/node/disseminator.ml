type pool_elt = { category : Message.category ; payload : bytes ; remaining : int }
type t = {
  mutable round : int;
  mutable pool : pool_elt list;
  mutable num_rounds : int;
}

let create num_rounds =
  { round = 0; pool = []; num_rounds }

let next_round disseminator =

  disseminator.round <- disseminator.round + 1;
  
  disseminator.pool <-
    disseminator.pool
    |> List.map (fun ({ remaining ; _ } as elt) -> { elt with remaining = remaining - 1}) 
    |> List.filter (fun elt -> elt.remaining > 0) 

let post disseminator category payload =
  disseminator.pool <- { category; payload ; remaining = disseminator.num_rounds } :: disseminator.pool

let broadcast_queue disseminator =
  List.map (fun { category ; payload ; _ } -> (category, payload)) disseminator.pool