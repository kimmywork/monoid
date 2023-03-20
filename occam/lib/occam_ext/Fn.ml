let compose f g x = f (g x)
let and_then f g x = g (f x)
let ( <* ) f g = compose f g
let ( *> ) f g = and_then f g
let bind_with f = ( |> ) *> f
let bind_into l = bind_with (Fun.flip List.iter l)
let bind_over l = bind_with (Fun.flip List.map l)
let ( |*> ) x = bind_with x
let curry f a b = f (a, b)
let uncurry f (a, b) = f a b
