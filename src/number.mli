exception Not_rational

module Number : sig
  type t =
    Integer of int
  | Rational of int * int
  | Float of float
  | Complex of float * float
  val sum              : t -> t -> t
  val sub              : t -> t -> t
  val mul              : t -> t -> t
  val div              : t -> t -> t
  val string_of_number : t -> string
end
