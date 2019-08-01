type vertex = int


module type G = sig
  val edges : v -> v list
  val vertices : v list
end

module Make (G: G) = struct
  include G
end
