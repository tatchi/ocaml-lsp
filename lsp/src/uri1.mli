type t

val of_path : string -> t

val to_path : t -> string

val path : t -> string

module Private : sig
  val win32 : bool ref
end
