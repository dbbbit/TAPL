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
        "true"
    | TmFalse ->
        "false"
    | TmIf(a, b, c) ->
        "(if " ^ to_string a ^ " then " ^ to_string b ^ " else " ^ to_string c ^ ")"
    | TmZero ->
        "0"
    | TmSucc(a) ->
        "(succ " ^ to_string a ^ ")"
    | TmPred(a) ->
        "(pred" ^ to_string a ^ ")"
    | TmIsZero(a) ->
        "(iszero " ^ to_string a ^ ")"

let print_term t =
    print_endline (to_string t)

exception NoRuleApplies

let rec isnumbericval t = match t with
      TmZero -> true
    | TmSucc(t1) -> isnumbericval t1
    | _ -> false

let rec isval t = match t with
      TmTrue -> true
    | TmFalse -> true
    | t when isnumbericval t -> true
    | _ -> false

let rec eval1 t = match t with
      TmIf(TmTrue, t2, t3) -> t2
    | TmIf(TmFalse, t2, t3) -> t3
    | TmIf(t1, t2, t3) ->
        let _t = eval1 t1 in
        TmIf(_t, t2, t3)
    | TmSucc(t1) ->
        let _t1 = eval1 t1 in
        TmSucc(_t1)
    | TmPred(TmZero) -> TmZero
    | TmPred(TmSucc(nvl)) when (isnumbericval nvl) -> nvl
    | TmPred(t1) ->
        let _t1 = eval1 t1 in
        TmPred(_t1)
    | TmIsZero(TmZero) -> TmTrue
    | TmIsZero(TmSucc(nvl)) when (isnumbericval nvl) -> TmFalse
    | TmIsZero(t1) ->
        let _t1 = eval1 t1 in
        TmIsZero(_t1)
    | _ -> raise NoRuleApplies

let rec eval t = 
    try
        let _t = eval1 t in
        eval _t
    with NoRuleApplies ->
        t
