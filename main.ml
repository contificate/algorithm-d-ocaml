
let () =
  let open Algorithm in
  go "f(x)" "z(f(x, u), f(x), f(x))";
  go "a(a(b, _), c)" "z(f(x, u), a(a(b, f(x)), c), a(a(b, f(x)), d))";
  go "g(_)" "f(g(g(f(g(x)))))"

