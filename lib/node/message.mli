(** Messages received by the node, whether they're requests,
responses, or protocol-specific messages. For consumer use
only when implementing a preprocessing function for the
node. *)
open Common

(** Messages are requests or responses,
determining how they're stored and
where they're handled *)
type category =
  | Uncategorized
  | Request
  | Response
  | Post
  | Failure_detection
  | Custom            of string
[@@deriving bin_io, show]

(** Messages received from peers which are processed by the node's
    message handler*)
type t = {
  category : category;
  id : int;
  timestamp : float;
  sender : Address.t;
  recipients : Address.t list;
  payload : bytes;
}
[@@deriving bin_io]

val hash_of : t -> Digest.t