type t

val zero : t

val ( + ) : t -> t -> t
(** [a + b] is field addition in GF(256). *)

val ( - ) : t -> t -> t
(** [a - b] is field subtraction in GF(256). *)

val ( * ) : t -> t -> t
(** [a * b] is field multiplication in GF(256). *)

val ( / ) : t -> t -> t
(** [a / b] is [a] times the multiplicative inverse of [b]. Raises
    [Division_by_zero] if [b = 0], as 0 is not in the multiplicative group of
    GF(256). *)

val ( ** ) : t -> int -> t
(** [x ** i] is [x * x * ... * x] (i times), a.k.a. [x] to the [i]-th power.
    Note that [i] is any integer, not necessarily a member of GF(256). *)

val from_byte : int -> t
(** [from_byte b] is the element of GF(256) corresponding to the byte [b].
    Raises [Invalid_argument] if [b] is not in the range [0..255]. *)

val to_byte : t -> int
(** [to_byte x] is the byte corresponding to the element [x] of GF(256). *)

val pp : Format.formatter -> t -> unit
(** [pp fmt x] pretty-prints the element [x] of GF(256) to the formatter [fmt].
*)
