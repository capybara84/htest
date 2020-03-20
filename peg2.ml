type parser_context = {
    input : char list;
}

let string_to_list s =
    let len = String.length s in
    let rec loop i res =
        if i = len then List.rev res
        else loop (i + 1) (s.[i] :: res)
    in
    loop 0 []

let string_of_list l =
    List.fold_right (fun x -> (^) (String.make 1 x)) l ""

let init_pc str = { input = string_to_list str }

let print_pc pc = Printf.printf "input: %s\n" (string_of_list pc.input)

let next pc = { input = List.tl pc.input }

type expression = parser_context -> bool * parser_context

type character = char -> expression

let ch c pc =
    if pc.input <> [] && List.hd pc.input = c
    then (true, next pc)
    else (false, pc)

let seq e1 e2 pc =
    let (res1, pc1) = e1 pc in
    if res1
    then e2 pc1
    else (false, pc)

let ord e1 e2 pc =
    let (res1, pc1) = e1 pc in
    if res1
    then (true, pc1)
    else e2 pc

let rec many e pc =
    let (res, pc') = e pc in
    if res
    then many e pc'
    else (true, pc)

let abc = seq (ch 'a') @@ seq (ch 'b' ) (ch 'c')
let a_or_b = ord (ch 'a') (ch 'b')

let num = ord (ch '0') @@ ord (ch '1') @@ ord (ch '2') @@ ord (ch '3') @@ ord (ch '4')
    @@ ord (ch '5') @@ ord (ch '6') @@ ord (ch '7') @@ ord (ch '8') (ch '9')
let value = seq num (many num)
let product = seq value @@ many (seq (ord (ch '*') (ch '/')) value)
let expression = seq product @@ many (seq (ord (ch '+') (ch '-')) product)

let () = print_pc (snd (ch 'a' (init_pc "abcba")))
let () = print_pc (snd (abc (init_pc "abcba")))
let () = print_pc (snd (a_or_b (init_pc "bcba")))
let () = print_pc (snd (many (ch 'a') (init_pc "aaaaa")))
let () = print_pc (snd (expression (init_pc "1+2*3")))
let () = print_pc (snd (expression (init_pc "12+3*4")))


