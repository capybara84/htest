
exception Error of string

type token
    = TInt of int
    | TPlus | TMinus | TStar | TSlash | TLpar | TRpar

let tok_to_s = function
    | TInt n -> string_of_int n | TPlus -> "+" | TMinus -> "-" | TStar -> "*"
    | TSlash -> "/" | TLpar -> "(" | TRpar -> ")"

type lexer = {
    text : string;
    len : int;
    current : int;
}

let init_lex s = { text = s; len = String.length s; current = 0 }
let is_end lex = lex.current = lex.len
let peek lex = lex.text.[lex.current]
let next lex =
    if is_end lex then lex
    else { lex with current = lex.current + 1 }
let rec skip_newline lex =
    let lex = next lex in
    if not (is_end lex) && peek lex = '\n'
    then skip_newline lex
    else lex

let cut_token pred lex =
    let buffer = Buffer.create 5 in
    let rec aux lex =
        if is_end lex then lex
        else match peek lex with
            | ch when pred ch ->
                Buffer.add_char buffer ch;
                aux (next lex)
            | _ -> lex
    in
    let lex = aux lex in
    (Buffer.contents buffer, lex)

let lex_number lex =
    let is_digit = function '0'..'9' -> true | _ -> false in
    let s, lex = cut_token is_digit lex in
    (TInt (int_of_string s), lex)

let lexer s =
    let rec get_tokens lex acc =
        if is_end lex then
            List.rev acc
        else
            match peek lex with
            | ' ' | '\t' | '\r' -> get_tokens (next lex) acc
            | '\n' -> get_tokens (skip_newline lex) acc
            | '0'..'9' ->
                let t, lex = lex_number lex in
                get_tokens lex (t::acc)
            | '+' -> get_tokens (next lex) (TPlus::acc)
            | '-' -> get_tokens (next lex) (TMinus::acc)
            | '*' -> get_tokens (next lex) (TStar::acc)
            | '/' -> get_tokens (next lex) (TSlash::acc)
            | '(' -> get_tokens (next lex) (TLpar::acc)
            | ')' -> get_tokens (next lex) (TRpar::acc)
            | _ -> raise (Error "invalid character")
    in
    get_tokens (init_lex s) []

let test_lexer () =
    let toks = lexer "12 + 23 * (34 - 45)" in
    List.iter (fun x -> print_endline @@ tok_to_s x) toks;
    ()

type exp
    = EAdd of exp * exp
    | ESub of exp * exp
    | EMul of exp * exp
    | EDiv of exp * exp
    | EInt of int

let rec exp_to_s = function
    | EAdd (x, y) -> "(" ^ exp_to_s x ^ " + " ^ exp_to_s y ^ ")"
    | ESub (x, y) -> "(" ^ exp_to_s x ^ " - " ^ exp_to_s y ^ ")"
    | EMul (x, y) -> "(" ^ exp_to_s x ^ " * " ^ exp_to_s y ^ ")"
    | EDiv (x, y) -> "(" ^ exp_to_s x ^ " / " ^ exp_to_s y ^ ")"
    | EInt n -> string_of_int n

type parser_context = token list
type parser_expression = parser_context -> bool * exp list * parser_context

let seq e1 e2 ctx =
    let (suc1, res1, ctx1) = e1 ctx in
    if suc1 then let (suc2, res2, ctx2) = e2 ctx1 in
        if suc2 then (true, res1 @ res2, ctx2)
        else (false, [], ctx2)
    else (false, [], ctx1)

let sel e1 e2 ctx =
    let (suc1, res1, ctx1) = e1 ctx in
    if suc1 then (true, res1, ctx1)
    else e2 ctx

let rec many e ctx =
    let rec aux acc ctx =
        let (suc1, res1, ctx1) = e ctx in
        if suc1 then aux (acc @ res1) ctx1
        else (true, acc, ctx1)
    in
    aux [] ctx

let t_int ctx =
    match ctx with
    | TInt n :: rest -> (true, [EInt n], rest)
    | _ -> (false, [], ctx)

let tok_skip t ctx =
    match ctx with
    | x :: rest when x = t -> (true, [], rest)
    | _ -> (false, [], ctx)

(*
    program = {expr}
    expr = mul_expr {addop mul_expr}
    mul_expr = primary {mulop primary}
    primary = num | '(' expr ')'
*)
let rec expr x = (seq mul_expr @@ many (seq (sel (tok TPlus) (tok TMinus)) mul_expr)) x
and mul_expr x = (seq primary @@ many (seq (sel (tok TStar) (tok TSlash)) primary)) x
and primary x = (sel t_int @@ seq (tok_skip TLpar) (seq expr (tok_skip TRpar))) x

let test parse toks =
    let (suc, res, pc) = parse toks in
    if suc then begin
        print_string "res :";
        List.iter (fun x -> print_string @@ exp_to_s x ^ " ") res;
        print_newline ()
    end else
        print_endline "FAIL"

let () =
    test t_int [TInt 12];
    test (tok TLpar) [TLpar];
    test (seq (tok TLpar) t_int) [TLpar;TInt 3];
    test (seq (tok TLpar) (seq t_int (tok TRpar))) [TLpar; TInt 4; TRpar];
    test expr [TInt 12; TPlus; TInt 23; TStar; TInt 34]

(*
let test () =
    let toks = lexer "12 + 23 * (34 - 45)" in
    let (suc, res, pc) = expr toks in
    if suc then begin
        print_endline @@ "res: " ^ res
    end else
        print_endline "FAIL"
*)
