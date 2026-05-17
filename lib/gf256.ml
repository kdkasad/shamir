(* GF(256) field implementation *)

type t = int
(** [t] is the bitwise coefficient representation of polynomial t(x) in the
    GF(256) field. *)

let zero = 0

(** [modulus] is x^8 + x^4 + x^3 + x + 1, the irreducible polynomial which is
    used as the modulus for multiplication in GF(256). Note: [modulus] is not a
    member of GF(256). *)
let modulus : t = 0x11b

(** [group_add a b] is the polynomial addition of [a] and [b]. *)
let group_add a b = a lxor b

(** [group_sub a b] is the polynomial subtraction of [a] and [b]. *)
let group_sub = group_add

(** [mul_slow a b] calculates a(x) * b(x) using the Russian peasant's algorithm
    (slow). *)
let group_mul_slow a b =
  (* [mult_by_x p] is p(x) * x. *)
  let mul_by_x p =
    let p' = p lsl 1 in
    if p' > 255 then
      (* p(x)*x is larger than 255, so we take it mod [modulus]. Since p(x) has
         degree 7, p'(x) has degree 8, so p'(x) mod modulus = p'(x) - modulus.
         *)
      group_sub p' modulus
    else p'
  in
  let rec mul_slow' a b acc =
    (* If the LSB of [b] is set, i.e. b(x) has a +1 term, then add a(x) to the
      result. Then repeat with the next bit of [b] using a(x) * x and so on. If
      not set, simply skip adding a(x) to the result. *)
    if b = 0 then acc
    else
      let acc' = if b land 1 = 1 then group_add acc a else acc in
      mul_slow' (mul_by_x a) (b lsr 1) acc'
  in
  mul_slow' a b 0

let%expect_test "mul_slow" =
  print_int (group_mul_slow 0x53 0xCA);
  [%expect {| 1 |}]

(** [alpha] is a generator of GF(256) \ \{0} *)
let alpha = 3

let%expect_test "alpha generates GF(256)" =
  (* Generates the list of the first 255 powers of alpha *)
  let rec generate i alpha_i lst =
    if i > 255 then lst
    else generate (i + 1) (group_mul_slow alpha_i alpha) (alpha_i :: lst)
  in
  generate 1 alpha [] |> List.sort_uniq Int.compare |> List.length |> print_int;
  [%expect {| 255 |}]

(** [exp_table.(i)] is [alpha]^[i]. *)
let exp_table =
  let a = Array.make 256 0 in
  let rec init i acc =
    if i >= Array.length a then ()
    else (
      a.(i) <- acc;
      init (i + 1) (group_mul_slow acc alpha))
  in
  init 0 1;
  a

(** [log_table.(i)] is x such that [alpha]^x = [i]. I.e., [log.(exp.(i)) = i]
    for all [i]. Note: [log.(0)] is undefined, as 0 is not in the cyclic group.
*)
let log_table =
  let a = Array.make 256 0 in
  (* We skip i=255 because that yields log(1) = 255, but log(1)=0 is more efficient. *)
  for i = 1 to 254 do
    a.(exp_table.(i)) <- i
  done;
  a

let print_int_list lst =
  Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int
    (Format.get_std_formatter ())
    lst

let%expect_test "exponentiation table" =
  print_int_list (Array.to_list exp_table);
  [%expect
    "\n\
    \ 1 3 5 15 17 51 85 255 26 46 114 150 161 248 19 53 95 225 56 72 216 115 149\n\
    \ 164 247 2 6 10 30 34 102 170 229 52 92 228 55 89 235 38 106 190 217 112 \
     144\n\
    \ 171 230 49 83 245 4 12 20 60 68 204 79 209 104 184 211 110 178 205 76 \
     212 103\n\
    \ 169 224 59 77 215 98 166 241 8 24 40 120 136 131 158 185 208 107 189 220 \
     127\n\
    \ 129 152 179 206 73 219 118 154 181 196 87 249 16 48 80 240 11 29 39 105 \
     187\n\
    \ 214 97 163 254 25 43 125 135 146 173 236 47 113 147 174 233 32 96 160 \
     251 22\n\
    \ 58 78 210 109 183 194 93 231 50 86 250 21 63 65 195 94 226 61 71 201 64 \
     192\n\
    \ 91 237 44 116 156 191 218 117 159 186 213 100 172 239 42 126 130 157 188 \
     223\n\
    \ 122 142 137 128 155 182 193 88 232 35 101 175 234 37 111 177 200 67 197 84\n\
    \ 252 31 33 99 165 244 7 9 27 45 119 153 176 203 70 202 69 207 74 222 121 \
     139\n\
    \ 134 145 168 227 62 66 198 81 243 14 18 54 90 238 41 123 141 140 143 138 \
     133\n\
    \ 148 167 242 13 23 57 75 221 124 132 151 162 253 28 36 108 180 199 82 246\n\
    \ 1\n\
    \ "]

(** [mul a b] is a(x) * b(x) in GF(256), calculated quickly using lookup tables.
*)
let group_mul a b =
  if a = 0 || b = 0 then 0
  else
    (* a * b = alpha^x1 * alpha^x2 = alpha^((x1 + x2) mod 255) *)
    let log_sum = log_table.(a) + log_table.(b) in
    if log_sum >= 255 then exp_table.(log_sum - 255) else exp_table.(log_sum)

let%expect_test "mul = mul_slow" =
  let rec test l r =
    if l > 255 then ()
    else if r > 255 then test (l + 1) 0
    else
      let slow = group_mul_slow l r in
      let fast = group_mul l r in
      if slow <> fast then
        Format.printf "mul_slow %i %i = %i but mul %i %i = %i\n" l r slow l r
          fast
      else test l (r + 1)
  in
  test 0 0;
  [%expect {||}]

(* a * x = alpha^(e + d) = 1 ==> d = -log(a) <==> x = alpha^(255 - log(a)) *)
let group_inv x =
  if x = 0 then raise Division_by_zero else exp_table.(255 - log_table.(x))

let%expect_test "x * inv x = 1" =
  for x = 1 to 255 do
    let xinv = group_inv x in
    if group_mul x xinv <> 1 then
      Format.printf "mul %i (inv %i) = mul %i %i should be 1 but is %i\n" x x x
        xinv (group_mul x xinv)
    else ()
  done

let group_div a b = group_mul a (group_inv b)

(* x^e = (alpha^d)^e = alpha^((e * d) mod 255) *)
let rec group_pow x (e : int) =
  if e = 0 then 1
  else if x = 0 then 0
  else exp_table.(e * log_table.(x) mod 255)

let%expect_test "pow" =
  (* Ground-truth using repeated squaring *)
  let rec pow_rs x e =
    if e = 0 then 1
    else
      let half_pow = pow_rs x (e / 2) in
      if e mod 2 = 0 then group_mul half_pow half_pow
      else group_mul x (group_mul half_pow half_pow)
  in
  for x = 1 to 255 do
    for e = 0 to 255 do
      let expected = pow_rs x e in
      let actual = group_pow x e in
      if actual <> expected then
        Format.printf "pow %i %i = %i but pow_rs %i %i = %i\n" x e actual x e
          expected
      else ()
    done
  done;
  [%expect {||}]

(* Alias the arithmetic operators _after_ testing so we don't confuse ourselves. *)
let ( + ) = group_add
let ( - ) = group_sub
let ( * ) = group_mul
let ( / ) = group_div
let ( ** ) = group_pow

let from_byte b =
  if b < 0 || b > 255 then invalid_arg "requires 0 <= b < 256"
  else b

let to_byte = Fun.id
let pp fmt x = Format.fprintf fmt "{%02X}" x
