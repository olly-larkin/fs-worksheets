let fact x = 
    List.reduce (*) [1 .. x]

let sum f n =
    [0 .. n] |> List.map f |> List.reduce (+)

let sinSeries x n =
    let g x i =
        let num x i = x ** (2.0 * (float i) + 1.0)
        let den i = fact (2 * i + 1)
        (-1.0 ** (float i)) * (num x i) / (float (den i))
    if n >= 1 
    then sum (g x) n
    else 0.0