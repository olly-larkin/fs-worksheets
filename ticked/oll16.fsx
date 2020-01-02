let fac x = List.reduce (*) [1 .. x]

let sum (f: int -> float) n =
    List.reduce (+) (List.map f [0 .. n])

let g x i =
    ((-1.0)**(float i)) * (x ** (2.0 * (float i) + 1.0)) / (float (fac (2 * i + 1)))

let sin (x: float, n: int) =
    if n >= 1 then 
        sum (g x) n
    else
        0.0