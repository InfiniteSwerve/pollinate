open Common
open Bin_prot.Std

type category =
  | Uncategorized
  | Request
  | Response
  | Post
  | Failure_detection
  | Custom            of string
[@@deriving bin_io, show]

type t = {
  category : category;
  id : int;
  timestamp : float;
  sender : Address.t;
  recipients : Address.t list;
  payload : bytes;
}
[@@deriving bin_io]
