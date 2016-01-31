type term =
    | TmVar of int 
    | TmAbs of term
    | TmApp of term * term

let rec term_to_string t =
    match t with
      TmVar(v) -> string_of_int v
    | TmAbs(t) -> "(\." ^ term_to_string t ^ ")"
    | TmApp(t1, t2) -> 
        "(" ^ term_to_string t1 ^ " " ^ term_to_string t2 ^ ")"

let print_term t =
    print_string (term_to_string t);
    print_newline();


(* eval *)
exception NoRuleApplies

let isval t = match t with
      TmAbs(_) -> true
    | _ -> false

let termShift d t =
    let rec walk c t = match t with
          TmVar(x) -> if x >= c then TmVar(x+d) else TmVar(x)
        | TmAbs(t1) -> TmAbs(walk (c+1) t1)
        | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    in walk 0 t

let termSubst j s t = 
    let rec walk c t = match t with
          TmVar(x) -> if x=j+c then termShift c s else TmVar(x)
        | TmAbs(t1) -> TmAbs(walk (c+1) t1)
        | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    in walk 0 t

let termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec eval1 t = match t with
      TmApp(TmAbs(t1), v2) when isval v2 -> 
        termSubstTop v2 t1
    | TmApp(v1, t2) when isval v1 -> 
        TmApp(v1, (eval1 t2))
    | TmApp(t1, t2) ->
        TmApp((eval1 t1), t2)
    | _ -> raise NoRuleApplies

let rec eval t =
    try
        let t1 = eval1 t
        in eval t1
    with NoRuleApplies -> t
