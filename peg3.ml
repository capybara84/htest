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

type expression = parser_context -> bool * string * parser_context

type character = char -> expression

let ch c pc =
    if pc.input <> [] && List.hd pc.input = c
    then (true, String.make 1 c, next pc)
    else (false, "", pc)

let seq e1 e2 pc =
    let (success1, result1, pc1) = e1 pc in
    if success1
    then let (success2, result2, pc2) = e2 pc1 in
        if success2
        then (true, result1 ^ result2, pc2)
        else (false, "", pc2)
    else (false, "", pc)

let ord e1 e2 pc =
    let (suc1, res1, pc1) = e1 pc in
    if suc1
    then (true, res1, pc1)
    else e2 pc

let rec many e pc =
    let rec loop acc pc =
        let (suc1, res1, pc') = e pc in
        if suc1
        then loop (acc ^ res1) pc'
        else (true, acc, pc)
    in
    loop "" pc

let abc = seq (ch 'a') @@ seq (ch 'b' ) (ch 'c')
let a_or_b = ord (ch 'a') (ch 'b')

let num = ord (ch '0') @@ ord (ch '1') @@ ord (ch '2') @@ ord (ch '3') @@ ord (ch '4')
    @@ ord (ch '5') @@ ord (ch '6') @@ ord (ch '7') @@ ord (ch '8') (ch '9')
let value = seq num (many num)
let product = seq value @@ many (seq (ord (ch '*') (ch '/')) value)
let expression = seq product @@ many (seq (ord (ch '+') (ch '-')) product)

let test e s =
    let (suc, res, pc) = e @@ init_pc s in
    if suc then begin
        print_string @@ "res: " ^ res ^ " ";
        print_pc pc
    end else
        print_endline "FAIL"

let () =
    test (ch 'a') "abcba";
    test abc "abcba";
    test a_or_b "bcba";
    test (many (ch 'a')) "aaaaa";
    test expression "1+2*3";
    test expression "12+3*4";
    ()

