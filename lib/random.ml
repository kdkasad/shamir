type cache = { mutable i : int; bytes : bytes }

let cache : cache =
  let len = 256 in
  { i = len; bytes = Bytes.create len }

let refill_cached_bytes () =
  let stream = open_in_bin "/dev/urandom" in
  let len = Bytes.length cache.bytes in
  really_input stream cache.bytes 0 len;
  close_in stream;
  cache.i <- 0

let get_random_byte () =
  if cache.i = Bytes.length cache.bytes then refill_cached_bytes () else ();
  let b = Bytes.get cache.bytes cache.i in
  cache.i <- cache.i + 1;
  int_of_char b
