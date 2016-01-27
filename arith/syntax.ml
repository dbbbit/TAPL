type term = 
    | TmTrue
    | TmFalse
    | TmIf  of term * term * term
    | TmZero
    | TmSucc of term
    | TmPred of term
    | TmIsZero of term


let rec to_string t = 
    match t with
    | TmTrue ->
        "TmTrue"
    | TmFalse ->
        "TmFalse"
    | TmIf(a, b, c) ->
        "TmIf(" ^ to_string a ^ ", " ^ to_string b ^ ", " ^ to_string c ^ ")"
    | TmZero ->
        "TmZero"
    | TmSucc(a) ->
        "TmSucc(" ^ to_string a ^ ")"
    | TmPred(a) ->
        "TmPred(" ^ to_string a ^ ")"
    | TmIsZero(a) ->
        "TmIsZero(" ^ to_string a ^ ")"

let print_term t =
    print_endline (to_string t)
