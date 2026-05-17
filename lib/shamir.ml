let max_shares = 32

type polynomial = Gf256.t list
(** Representation of a degree N polynomial as a list of its N coefficients.
    [[a0; a1; ...; ak]] is [f(x) = \sum_{i=0}^{n} ai x^i]. *)

let make_random_polynomial s t =
  s :: List.init (max 0 (t - 2)) (fun _ -> Random.get_random_byte ())
  |> List.map Gf256.from_byte

let evaluate_poly p x =
  List.mapi (fun i a -> Gf256.(a * (x ** i))) p
  |> List.fold_left Gf256.( + ) Gf256.zero

let%expect_test "evaluate known polynomial at x=0" =
  let p = [ 1; 2; 3; 4; 5 ] |> List.map Gf256.from_byte in
  let result = Gf256.to_byte (evaluate_poly p Gf256.zero) in
  print_int result;
  [%expect {| 1 |}]

let share_byte b n t =
  let p = make_random_polynomial b t in
  List.init n (fun i ->
      let x = Gf256.from_byte (i + 1) in
      (x, evaluate_poly p x))

let share s n t =
  let rec transpose per_party per_byte =
    match per_byte with
    | [] | [] :: _ -> List.rev per_party
    | per_byte ->
        transpose
          (List.map List.hd per_byte :: per_party)
          (List.map List.tl per_byte)
  in
  if n < 1 || n > max_shares then invalid_arg "n must be between 1 and 32"
  else if t < 1 || t > n then invalid_arg "t must be between 1 and n"
  else
    let len = Bytes.length s in
    let per_byte =
      List.init len (fun i ->
          let b = int_of_char (Bytes.get s i) in
          share_byte b n t)
    in
    transpose [] per_byte
    |> List.map (fun row -> (fst (List.hd row), List.map snd row))

module Gf256 = Gf256
