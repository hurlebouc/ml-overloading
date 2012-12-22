exception Error

type token = 
  | WITH
  | TYPE
  | STAR
  | SEMISEMI
  | RPAR
  | REC
  | RBRACKET
  | RANGLE
  | QUOTE
  | QUESTIONMARK
  | OF
  | MATCH
  | LPAR
  | LET
  | LBRACKET
  | LANGLE
  | LAM
  | IN
  | IDENTIFIER of (
# 32 "parser.mly"
       (string)
# 26 "parser.ml"
)
  | FUN
  | EOF
  | END
  | DOUBLEARROW
  | DOT
  | DEFEQ
  | CONSTRUCTOR of (
# 32 "parser.mly"
       (string)
# 37 "parser.ml"
)
  | COMMA
  | COLON
  | BEGIN
  | BAR
  | ARROW
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState182
  | MenhirState177
  | MenhirState175
  | MenhirState171
  | MenhirState169
  | MenhirState166
  | MenhirState161
  | MenhirState159
  | MenhirState157
  | MenhirState153
  | MenhirState148
  | MenhirState145
  | MenhirState138
  | MenhirState136
  | MenhirState135
  | MenhirState133
  | MenhirState131
  | MenhirState127
  | MenhirState121
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState108
  | MenhirState105
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState88
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState75
  | MenhirState72
  | MenhirState71
  | MenhirState68
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState45
  | MenhirState40
  | MenhirState39
  | MenhirState35
  | MenhirState29
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState17
  | MenhirState15
  | MenhirState8
  | MenhirState6
  | MenhirState4
  | MenhirState1
  | MenhirState0


# 6 "parser.mly"
  

open Ast

let etapp e1 ts =
  match e1 with
  | EConApp (tag, ts', []) -> 
      EConApp (tag, ts @ ts', [])
  | _ ->
      ETapp (e1, ts)

let eapp e1 e2 =
  match e1 with
  | EConApp (tag, ts, args) -> 
      EConApp (tag, ts, args @ [e2])
  | _ ->  EApp (e1, e2)

let emapp e el =
  match e with
  | EConApp (tag, ts, args) -> 
      EConApp (tag, ts, args @ el)
  | _ ->
      List.fold_left (fun e ei ->  EApp (e, ei))  e el


# 148 "parser.ml"
let _eRR =
  Error

let rec _menhir_goto_bar_branch_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bar_branch_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv975 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bar_branch_) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv973 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bar_branch_) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv969 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bar_branch_) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv967 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bar_branch_) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, e), _, bs) = _menhir_stack in
        let _v : 'tv_expression = 
# 217 "parser.mly"
    ( EMatch (e, bs) )
# 173 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv968)) : 'freshtv970)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv971 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_bar_branch_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv972)) : 'freshtv974)) : 'freshtv976)

and _menhir_goto_sch : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sch -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv955 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 193 "parser.ml"
        )) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv953 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, v), _, s) = _menhir_stack in
        let _v : 'tv_sch = 
# 140 "parser.mly"
    ( let (vs, r) = s in v::vs, r )
# 201 "parser.ml"
         in
        _menhir_goto_sch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv954)) : 'freshtv956)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv965 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 209 "parser.ml"
        )) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv963 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 217 "parser.ml"
        )) * _menhir_state * 'tv_sch) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DEFEQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv959 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 226 "parser.ml"
            )) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv957 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 233 "parser.ml"
            )) * _menhir_state * 'tv_sch) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv958)) : 'freshtv960)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv961 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 262 "parser.ml"
            )) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv962)) : 'freshtv964)) : 'freshtv966)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_IDENTIFIER_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv941 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 278 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 282 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 286 "parser.ml"
        )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv939 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 292 "parser.ml"
        )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_IDENTIFIER_ = 
# 146 "standard.mly"
    ( x :: xs )
# 298 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv940)) : 'freshtv942)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv951 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 306 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 310 "parser.ml"
        )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv949 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 318 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 322 "parser.ml"
        )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv945 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 331 "parser.ml"
            )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 335 "parser.ml"
            )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv943 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 342 "parser.ml"
            )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, head0), _, tail0) = _menhir_stack in
            let _v : 'tv_parameters_IDENTIFIER_ = let xs =
              let tail = tail0 in
              let head = head0 in
              
# 70 "parser.mly"
    ( head :: tail )
# 351 "parser.ml"
              
            in
            
# 102 "parser.mly"
    ( xs )
# 357 "parser.ml"
             in
            _menhir_goto_parameters_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv944)) : 'freshtv946)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv947 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 367 "parser.ml"
            )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 371 "parser.ml"
            )) * _menhir_state * 'tv_separated_nonempty_list_COMMA_IDENTIFIER_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv948)) : 'freshtv950)) : 'freshtv952)
    | _ ->
        _menhir_fail ()

and _menhir_goto_pattern : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_pattern -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 | MenhirState95 | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv927 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv925 * _menhir_state * 'tv_pattern) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv921 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv919 * _menhir_state * 'tv_pattern) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv920)) : 'freshtv922)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv923 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv924)) : 'freshtv926)) : 'freshtv928)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv937 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv935 * _menhir_state) * _menhir_state * 'tv_pattern) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DEFEQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv931 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv929 * _menhir_state) * _menhir_state * 'tv_pattern) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv930)) : 'freshtv932)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv933 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv934)) : 'freshtv936)) : 'freshtv938)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_let_rec_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_let_rec_definition_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv913 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv911 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv907 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv905 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv906)) : 'freshtv908)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv909 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv910)) : 'freshtv912)) : 'freshtv914)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv917 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_let_rec_definition) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv915 * _menhir_state * 'tv_let_rec_definition) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_AND_let_rec_definition_ = 
# 146 "standard.mly"
    ( x :: xs )
# 524 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_let_rec_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv916)) : 'freshtv918)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expression_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv889 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 539 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv887 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 547 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv883 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 556 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv881 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 563 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, tag), _), _, head0), _, tail0) = _menhir_stack in
            let _v : 'tv_expression1 = let es =
              let tail = tail0 in
              let head = head0 in
              
# 70 "parser.mly"
    ( head :: tail )
# 572 "parser.ml"
              
            in
            
# 197 "parser.mly"
    ( EConApp (tag, [], es) )
# 578 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv882)) : 'freshtv884)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv885 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 588 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv886)) : 'freshtv888)) : 'freshtv890)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv893 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 597 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv891 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_expression_ = 
# 146 "standard.mly"
    ( x :: xs )
# 605 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv892)) : 'freshtv894)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv903 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 613 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv901 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 621 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv897 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 630 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv895 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 637 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s, tag), _), _, ts), _), _, head0), _, tail0) = _menhir_stack in
            let _v : 'tv_expression1 = let es =
              let tail = tail0 in
              let head = head0 in
              
# 70 "parser.mly"
    ( head :: tail )
# 646 "parser.ml"
              
            in
            
# 209 "parser.mly"
    ( EConApp (tag, ts, es) )
# 652 "parser.ml"
             in
            _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv896)) : 'freshtv898)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv899 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 662 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_separated_nonempty_list_COMMA_expression_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv900)) : 'freshtv902)) : 'freshtv904)
    | _ ->
        _menhir_fail ()

and _menhir_run126 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv879 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
    ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : 'tv_expression0 = 
# 184 "parser.mly"
   ( e )
# 678 "parser.ml"
     in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv880)

and _menhir_goto_separated_nonempty_list_BAR_branch_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_BAR_branch_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv873 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_branch_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv871) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_BAR_branch_) = _v in
        ((let _v : 'tv_bar_branch_ = 
# 86 "parser.mly"
    ( xs )
# 697 "parser.ml"
         in
        _menhir_goto_bar_branch_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv872)) : 'freshtv874)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv877 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_branch_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv875 * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_BAR_branch_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_BAR_branch_ = 
# 146 "standard.mly"
    ( x :: xs )
# 713 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_branch_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv876)) : 'freshtv878)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_preceded_BAR_branch__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_preceded_BAR_branch__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv865 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_preceded_BAR_branch__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv863 * _menhir_state) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_preceded_BAR_branch__) = _v in
        ((let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
        let _v : 'tv_nonempty_list_preceded_BAR_branch__ = let x =
          let x = x0 in
          
# 86 "standard.mly"
    ( x )
# 737 "parser.ml"
          
        in
        
# 126 "standard.mly"
    ( x :: xs )
# 743 "parser.ml"
         in
        _menhir_goto_nonempty_list_preceded_BAR_branch__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv864)) : 'freshtv866)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv869 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_preceded_BAR_branch__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv867) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_preceded_BAR_branch__) = _v in
        ((let _v : 'tv_bar_branch_ = 
# 86 "parser.mly"
    ( xs )
# 758 "parser.ml"
         in
        _menhir_goto_bar_branch_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv868)) : 'freshtv870)
    | _ ->
        _menhir_fail ()

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv861 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112) : 'freshtv862)

and _menhir_goto_row : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_row -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv855 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 787 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_row) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv853 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (r : 'tv_row) = _v in
        ((let (_menhir_stack, _menhir_s, t) = _menhir_stack in
        let _v : 'tv_row = 
# 134 "parser.mly"
    ( let (ts, t0) = r in t::ts, t0 )
# 799 "parser.ml"
         in
        _menhir_goto_row _menhir_env _menhir_stack _menhir_s _v) : 'freshtv854)) : 'freshtv856)
    | MenhirState157 | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv859) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_row) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv857) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (r : 'tv_row) = _v in
        ((let _v : 'tv_sch = 
# 138 "parser.mly"
    ( [], r )
# 814 "parser.ml"
         in
        _menhir_goto_sch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv858)) : 'freshtv860)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_typ_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv815 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv813 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_typ_ = 
# 146 "standard.mly"
    ( x :: xs )
# 836 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typ_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv814)) : 'freshtv816)
    | MenhirState133 | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv847 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv845) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (ts : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let _v : 'tv_typs = 
# 128 "parser.mly"
    ( ts )
# 851 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv843) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_typs) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState84 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv825 * _menhir_state * 'tv_expression1) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv823 * _menhir_state * 'tv_expression1) * _menhir_state) * _menhir_state * 'tv_typs) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv819 * _menhir_state * 'tv_expression1) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv817 * _menhir_state * 'tv_expression1) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s, e), _), _, ts) = _menhir_stack in
                let _v : 'tv_expression1 = 
# 192 "parser.mly"
    ( ETapp (e, ts) )
# 878 "parser.ml"
                 in
                _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv818)) : 'freshtv820)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv821 * _menhir_state * 'tv_expression1) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv822)) : 'freshtv824)) : 'freshtv826)
        | MenhirState133 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv841 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 893 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv839 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 901 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv835 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 910 "parser.ml"
                )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv833 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 917 "parser.ml"
                )) * _menhir_state) * _menhir_state * 'tv_typs) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CONSTRUCTOR _v ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
                | IDENTIFIER _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv829 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 930 "parser.ml"
                    )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState135 in
                    ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv827 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 939 "parser.ml"
                    )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | BEGIN ->
                        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | CONSTRUCTOR _v ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
                    | FUN ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | IDENTIFIER _v ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
                    | LAM ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | LET ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | LPAR ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | QUESTIONMARK ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState136
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv828)) : 'freshtv830)
                | AND | BAR | COMMA | END | EOF | IN | LBRACKET | RPAR | WITH ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv831 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 968 "parser.ml"
                    )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s, tag), _), _, ts) = _menhir_stack in
                    let _v : 'tv_expression1 = 
# 201 "parser.mly"
    ( EConApp (tag, ts, []) )
# 974 "parser.ml"
                     in
                    _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv832)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv834)) : 'freshtv836)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv837 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 988 "parser.ml"
                )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv838)) : 'freshtv840)) : 'freshtv842)
        | _ ->
            _menhir_fail ()) : 'freshtv844)) : 'freshtv846)) : 'freshtv848)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv851 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 999 "parser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv849) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_COMMA_typ_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_typ__ = 
# 59 "standard.mly"
    ( x )
# 1010 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typ__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv850)) : 'freshtv852)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_STAR_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_STAR_typ_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv807 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1024 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1028 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_STAR_typ_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv805 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_STAR_typ_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_STAR_typ_ = 
# 146 "standard.mly"
    ( x :: xs )
# 1040 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_STAR_typ_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv806)) : 'freshtv808)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv811 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1048 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1052 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_STAR_typ_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv809) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (ts : 'tv_separated_nonempty_list_STAR_typ_) = _v in
        ((let _v : 'tv_constructor_parameters = 
# 247 "parser.mly"
    ( ts )
# 1063 "parser.ml"
         in
        _menhir_goto_constructor_parameters _menhir_env _menhir_stack _v) : 'freshtv810)) : 'freshtv812)
    | _ ->
        _menhir_fail ()

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1072 "parser.ml"
) -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 1076 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv803 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1085 "parser.ml"
    )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1089 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv797 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1098 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1102 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1106 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv795 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1113 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1117 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1121 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv796)) : 'freshtv798)
    | RPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv799 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1136 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_IDENTIFIER_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1142 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv800)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv801 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1152 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1156 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv802)) : 'freshtv804)

and _menhir_goto_parameters_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_parameters_IDENTIFIER_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv789 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1169 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_parameters_IDENTIFIER_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv787 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1177 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_parameters_IDENTIFIER_) = _v in
        ((let (((_menhir_stack, _menhir_s, tag), _), _, xs0) = _menhir_stack in
        let _v : 'tv_pattern = let ts =
          let xs = xs0 in
          
# 135 "standard.mly"
    ( xs )
# 1187 "parser.ml"
          
        in
        
# 151 "parser.mly"
    ( PConApp (tag, ts, xs) )
# 1193 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v) : 'freshtv788)) : 'freshtv790)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1201 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_parameters_IDENTIFIER_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv791 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1209 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_parameters_IDENTIFIER_) = _v in
        ((let (_menhir_stack, _menhir_s, tag) = _menhir_stack in
        let _v : 'tv_pattern = 
# 148 "parser.mly"
    ( PConApp (tag, [], xs) )
# 1217 "parser.ml"
         in
        _menhir_goto_pattern _menhir_env _menhir_stack _menhir_s _v) : 'freshtv792)) : 'freshtv794)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expression -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv645 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv643 * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv639 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv637 * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BAR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | CONSTRUCTOR _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv638)) : 'freshtv640)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv641 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv642)) : 'freshtv644)) : 'freshtv646)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv669 * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv667 * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, p), _, e) = _menhir_stack in
        let _v : 'tv_branch = 
# 237 "parser.mly"
    ( Branch (p, e) )
# 1268 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv665) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_branch) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState112 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv651 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv649 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state) * _menhir_state * 'tv_branch) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BAR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | END ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv647 * _menhir_state) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
                let _v : 'tv_nonempty_list_preceded_BAR_branch__ = let x =
                  let x = x0 in
                  
# 86 "standard.mly"
    ( x )
# 1296 "parser.ml"
                  
                in
                
# 124 "standard.mly"
    ( [ x ] )
# 1302 "parser.ml"
                 in
                _menhir_goto_nonempty_list_preceded_BAR_branch__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv648)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv650)) : 'freshtv652)
        | MenhirState121 | MenhirState95 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv663 * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv661 * _menhir_state * 'tv_branch) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv655 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv653 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_branch) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | CONSTRUCTOR _v ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv654)) : 'freshtv656)
            | END ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv657 * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_BAR_branch_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1339 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_BAR_branch_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv658)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv659 * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)) : 'freshtv662)) : 'freshtv664)
        | _ ->
            _menhir_fail ()) : 'freshtv666)) : 'freshtv668)) : 'freshtv670)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv679 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1356 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv677 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1364 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv673 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1373 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv671 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1380 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv672)) : 'freshtv674)
        | RPAR ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv675 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1411 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv676)) : 'freshtv678)) : 'freshtv680)
    | MenhirState138 | MenhirState131 | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv691 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv689 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv683 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1429 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv681 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1436 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState131
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv682)) : 'freshtv684)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv685 * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expression_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1465 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv686)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv687 * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv688)) : 'freshtv690)) : 'freshtv692)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv701 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1480 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv699 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1488 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv695 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1497 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv693 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1504 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv694)) : 'freshtv696)
        | RPAR ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv697 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1535 "parser.ml"
            )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv698)) : 'freshtv700)) : 'freshtv702)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv705 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 1544 "parser.ml"
        )) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv703 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 1550 "parser.ml"
        )) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s), _), _, q), x), _, t), _, e) = _menhir_stack in
        let _v : 'tv_expression = 
# 227 "parser.mly"
    ( EFunI (q, x, t, e) )
# 1556 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv704)) : 'freshtv706)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv709 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1564 "parser.ml"
        )) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv707 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1570 "parser.ml"
        )) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, x), _, t), _, e) = _menhir_stack in
        let _v : 'tv_expression = 
# 215 "parser.mly"
    ( EFun (x, t, e) )
# 1576 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv708)) : 'freshtv710)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv713 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv711 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, vs), _, e) = _menhir_stack in
        let _v : 'tv_expression = 
# 229 "parser.mly"
    ( ELam (vs, e) )
# 1588 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv712)) : 'freshtv714)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv717 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv715 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _), _, ds), _, e2) = _menhir_stack in
        let _v : 'tv_expression = 
# 224 "parser.mly"
    ( ELetRec (ds, e2) )
# 1600 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)) : 'freshtv718)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv735 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1608 "parser.ml"
        )) * _menhir_state * 'tv_sch) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv733 * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1614 "parser.ml"
        )) * _menhir_state * 'tv_sch) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, u), x), _, s), _, e) = _menhir_stack in
        let _v : 'tv_let_rec_definition = 
# 233 "parser.mly"
    ( (u, x, s, e) )
# 1620 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv731) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_rec_definition) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv729 * _menhir_state * 'tv_let_rec_definition) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv727 * _menhir_state * 'tv_let_rec_definition) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv721 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_let_rec_definition) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv719 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_let_rec_definition) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | QUESTIONMARK ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | IDENTIFIER _ ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv720)) : 'freshtv722)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv723 * _menhir_state * 'tv_let_rec_definition) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_AND_let_rec_definition_ = 
# 144 "standard.mly"
    ( [ x ] )
# 1658 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_AND_let_rec_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv725 * _menhir_state * 'tv_let_rec_definition) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv726)) : 'freshtv728)) : 'freshtv730)) : 'freshtv732)) : 'freshtv734)) : 'freshtv736)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv745 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv743 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv739 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv737 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171) : 'freshtv738)) : 'freshtv740)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv741 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv742)) : 'freshtv744)) : 'freshtv746)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv749 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv747 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, p), _, e1), _, e2) = _menhir_stack in
        let _v : 'tv_expression = 
# 221 "parser.mly"
    ( EMatch (e1, [ Branch (p, e2) ]) )
# 1719 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)) : 'freshtv750)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv759 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1727 "parser.ml"
        )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv757 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1735 "parser.ml"
        )) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv753 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1744 "parser.ml"
            )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv751 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1751 "parser.ml"
            )) * _menhir_state * 'tv_expression) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177) : 'freshtv752)) : 'freshtv754)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv755 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1780 "parser.ml"
            )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv756)) : 'freshtv758)) : 'freshtv760)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv763 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1789 "parser.ml"
        )) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv761 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 1795 "parser.ml"
        )) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, u), x), _, e1), _, e2) = _menhir_stack in
        let _v : 'tv_expression = 
# 219 "parser.mly"
    ( ELet (u, x, e1, e2) )
# 1801 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv762)) : 'freshtv764)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv769 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv767 * _menhir_state) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv766)) : 'freshtv768)) : 'freshtv770)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv783 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state * 'tv_expression) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv779 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv777 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, defs), _, e) = _menhir_stack in
            let _v : (
# 38 "parser.mly"
       (Ast.program)
# 1840 "parser.ml"
            ) = 
# 265 "parser.mly"
    ( Program (defs, e) )
# 1844 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv775) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
       (Ast.program)
# 1852 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv773) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
       (Ast.program)
# 1860 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv771) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 38 "parser.mly"
       (Ast.program)
# 1868 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv772)) : 'freshtv774)) : 'freshtv776)) : 'freshtv778)) : 'freshtv780)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv782)) : 'freshtv784)) : 'freshtv786)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bar_data_constructor_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bar_data_constructor_definition_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv635 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1887 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_bar_data_constructor_definition_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv633 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1895 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let (cs : 'tv_bar_data_constructor_definition_) = _v in
    ((let (((_menhir_stack, _menhir_s), _, xs), x) = _menhir_stack in
    let _v : 'tv_data_type_definition = 
# 256 "parser.mly"
    ( DefDataType (xs, x, cs) )
# 1903 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv631) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_data_type_definition) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv629 * _menhir_state * 'tv_data_type_definition) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv627 * _menhir_state * 'tv_data_type_definition) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | SEMISEMI ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182) : 'freshtv628)) : 'freshtv630)) : 'freshtv632)) : 'freshtv634)) : 'freshtv636)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv537 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv535 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
        let _v : 'tv_typ = 
# 124 "parser.mly"
    ( TArrow (t1, t2) )
# 1940 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv536)) : 'freshtv538)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv547 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state) * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv541 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv539 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : 'tv_typ0 = 
# 118 "parser.mly"
    ( t )
# 1962 "parser.ml"
             in
            _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv543 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)) : 'freshtv548)
    | MenhirState35 | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv559 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | STAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv551 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1986 "parser.ml"
            )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 1990 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv549 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 1997 "parser.ml"
            )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2001 "parser.ml"
            )) * _menhir_state * 'tv_typ) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | IDENTIFIER _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv550)) : 'freshtv552)
        | BAR | SEMISEMI | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_STAR_typ_ = 
# 144 "standard.mly"
    ( [ x ] )
# 2022 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_STAR_typ_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv554)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv556)) : 'freshtv558)) : 'freshtv560)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv569 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv567 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv563 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv561 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _), _, t) = _menhir_stack in
            let _v : 'tv_expression0 = 
# 182 "parser.mly"
    ( EImplicit (t) )
# 2051 "parser.ml"
             in
            _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)) : 'freshtv564)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv565 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv566)) : 'freshtv568)) : 'freshtv570)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv585 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2066 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv583 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2074 "parser.ml"
        )) * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv579 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2083 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv577 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2090 "parser.ml"
            )) * _menhir_state * 'tv_typ) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DOUBLEARROW ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv573 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2099 "parser.ml"
                )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv571 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2106 "parser.ml"
                )) * _menhir_state * 'tv_typ) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | BEGIN ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | CONSTRUCTOR _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
                | FUN ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | IDENTIFIER _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
                | LAM ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | LET ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | LPAR ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv572)) : 'freshtv574)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv575 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2135 "parser.ml"
                )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv576)) : 'freshtv578)) : 'freshtv580)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv581 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2146 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)) : 'freshtv584)) : 'freshtv586)
    | MenhirState133 | MenhirState105 | MenhirState88 | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv597 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv589 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv587 * _menhir_state) * _menhir_state * 'tv_typ) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IDENTIFIER _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv588)) : 'freshtv590)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv591 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_typ_ = 
# 144 "standard.mly"
    ( [ x ] )
# 2184 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_typ_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv592)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv593 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv594)) : 'freshtv596)) : 'freshtv598)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2199 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv611 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2207 "parser.ml"
        )) * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2216 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv605 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2223 "parser.ml"
            )) * _menhir_state * 'tv_typ) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | ARROW ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv601 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2232 "parser.ml"
                )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv599 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2239 "parser.ml"
                )) * _menhir_state * 'tv_typ) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | BEGIN ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | CONSTRUCTOR _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
                | FUN ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | IDENTIFIER _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
                | LAM ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | LET ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | LPAR ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv600)) : 'freshtv602)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv603 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2268 "parser.ml"
                )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv604)) : 'freshtv606)) : 'freshtv608)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv609 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2279 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv610)) : 'freshtv612)) : 'freshtv614)
    | MenhirState157 | MenhirState161 | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOUBLEARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv617 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2297 "parser.ml"
            )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv615 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2304 "parser.ml"
            )) * _menhir_state * 'tv_typ) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | IDENTIFIER _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161) : 'freshtv616)) : 'freshtv618)
        | DEFEQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv619 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, t) = _menhir_stack in
            let _v : 'tv_row = 
# 132 "parser.mly"
    ( [], t )
# 2325 "parser.ml"
             in
            _menhir_goto_row _menhir_env _menhir_stack _menhir_s _v) : 'freshtv620)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv621 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv622)) : 'freshtv624)) : 'freshtv626)
    | _ ->
        _menhir_fail ()

and _menhir_goto_implicit : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_implicit -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv517 * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv515 * _menhir_state * 'tv_implicit) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv511 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
            let (_v : (
# 32 "parser.mly"
       (string)
# 2357 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv509 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2365 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv505 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2374 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv503 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2381 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState157
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState157
                | IDENTIFIER _ ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState157
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157) : 'freshtv504)) : 'freshtv506)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv507 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2402 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)) : 'freshtv512)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv513 * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)) : 'freshtv516)) : 'freshtv518)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv533 * _menhir_state) * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * _menhir_state) * _menhir_state * 'tv_implicit) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv527 * _menhir_state) * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
            let (_v : (
# 32 "parser.mly"
       (string)
# 2428 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv525 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2436 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DEFEQ ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv521 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2445 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv519 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2452 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | BEGIN ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | CONSTRUCTOR _v ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                | FUN ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | IDENTIFIER _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
                | LAM ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | LET ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | LPAR ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState175
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175) : 'freshtv520)) : 'freshtv522)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv523 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 2481 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)) : 'freshtv528)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv529 * _menhir_state) * _menhir_state * 'tv_implicit) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)) : 'freshtv534)
    | _ ->
        _menhir_fail ()

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_parameters_IDENTIFIER_ = 
# 98 "parser.mly"
    ( [] )
# 2500 "parser.ml"
     in
    _menhir_goto_parameters_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv501 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv499 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2517 "parser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState97 in
        let (_v : (
# 32 "parser.mly"
       (string)
# 2523 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv497 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2531 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2535 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv493 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2544 "parser.ml"
            )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2548 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv491 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2555 "parser.ml"
            )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2559 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | IDENTIFIER _v ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv492)) : 'freshtv494)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv495 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2576 "parser.ml"
            )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2580 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)) : 'freshtv500)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv502)

and _menhir_goto_loption_separated_nonempty_list_COMMA_typ__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_typ__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv489 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2596 "parser.ml"
    )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv487 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2604 "parser.ml"
    )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv483 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2613 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv481 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2620 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | LPAR ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ARROW | DEFEQ ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv482)) : 'freshtv484)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv485 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 2641 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)) : 'freshtv490)

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 2649 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (x : (
# 32 "parser.mly"
       (string)
# 2659 "parser.ml"
    )) = _v in
    ((let _v : 'tv_parameters_IDENTIFIER_ = 
# 100 "parser.mly"
    ( [ x ] )
# 2664 "parser.ml"
     in
    _menhir_goto_parameters_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)

and _menhir_goto_expression1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expression1 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv477 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv475 * _menhir_state * 'tv_expression1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IDENTIFIER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState83 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * _menhir_state * 'tv_expression1) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | IDENTIFIER _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv470)) : 'freshtv472)
    | LPAR ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | AND | BAR | COMMA | END | EOF | IN | RPAR | WITH ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : 'tv_expression = 
# 213 "parser.mly"
    ( e )
# 2712 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v) : 'freshtv474)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv476)) : 'freshtv478)

and _menhir_goto_policy : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_policy -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * _menhir_state) * _menhir_state * 'tv_policy) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv449 * _menhir_state) * _menhir_state * 'tv_policy) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, q) = _menhir_stack in
        let _v : 'tv_implicit = 
# 169 "parser.mly"
    ( Some q  )
# 2733 "parser.ml"
         in
        _menhir_goto_implicit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)) : 'freshtv452)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv467 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv465 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv461 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) = Obj.magic _menhir_stack in
            let (_v : (
# 32 "parser.mly"
       (string)
# 2751 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv459 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2759 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv455 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2768 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv453 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2775 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | IDENTIFIER _ ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv454)) : 'freshtv456)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv457 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 2796 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)) : 'freshtv460)) : 'freshtv462)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv463 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)) : 'freshtv468)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_BAR_data_constructor_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_BAR_data_constructor_definition_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv443 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 2818 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_data_constructor_definition_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_BAR_data_constructor_definition_) = _v in
        ((let _v : 'tv_bar_data_constructor_definition_ = 
# 86 "parser.mly"
    ( xs )
# 2829 "parser.ml"
         in
        _menhir_goto_bar_data_constructor_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv447 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 2837 "parser.ml"
        )) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_BAR_data_constructor_definition_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv445 * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_BAR_data_constructor_definition_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_BAR_data_constructor_definition_ = 
# 146 "standard.mly"
    ( x :: xs )
# 2849 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_BAR_data_constructor_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_preceded_BAR_data_constructor_definition__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_preceded_BAR_data_constructor_definition__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv435 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 2863 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__) = _v in
        ((let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
        let _v : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__ = let x =
          let x = x0 in
          
# 86 "standard.mly"
    ( x )
# 2877 "parser.ml"
          
        in
        
# 126 "standard.mly"
    ( x :: xs )
# 2883 "parser.ml"
         in
        _menhir_goto_nonempty_list_preceded_BAR_data_constructor_definition__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)) : 'freshtv436)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv439 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 2891 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__) = _v in
        ((let _v : 'tv_bar_data_constructor_definition_ = 
# 86 "parser.mly"
    ( xs )
# 2902 "parser.ml"
         in
        _menhir_goto_bar_data_constructor_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_typ0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_typ0_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv421 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv419 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_typ0_ = 
# 146 "standard.mly"
    ( x :: xs )
# 2921 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typ0_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv420)) : 'freshtv422)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv431 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, head0), _, tail0) = _menhir_stack in
            let _v : 'tv_parameters_typ0_ = let xs =
              let tail = tail0 in
              let head = head0 in
              
# 70 "parser.mly"
    ( head :: tail )
# 2946 "parser.ml"
              
            in
            
# 102 "parser.mly"
    ( xs )
# 2952 "parser.ml"
             in
            _menhir_goto_parameters_typ0_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv424)) : 'freshtv426)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv427 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_separated_nonempty_list_COMMA_typ0_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)) : 'freshtv430)) : 'freshtv432)
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_typ0 -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_parameters_typ0_ = 
# 100 "parser.mly"
    ( [ x ] )
# 2971 "parser.ml"
     in
    _menhir_goto_parameters_typ0_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_typ0 -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, t) = _menhir_stack in
    let _v : 'tv_typ = 
# 122 "parser.mly"
    ( t )
# 2981 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_typ0 -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv417 * _menhir_state * 'tv_typ0) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENTIFIER _ ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv418)

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_implicit = 
# 167 "parser.mly"
    ( None )
# 3008 "parser.ml"
     in
    _menhir_goto_implicit _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv415 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LANGLE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | RANGLE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IDENTIFIER _ ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv416)

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 3034 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3043 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv411 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3054 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState96 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3063 "parser.ml"
        )) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | IDENTIFIER _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState105 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_typ__ = 
# 57 "standard.mly"
    ( [] )
# 3080 "parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_typ__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv410)) : 'freshtv412)
    | LPAR ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | ARROW | DEFEQ ->
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv414)

and _menhir_goto_expression0 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expression0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv393 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expression0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (e2 : 'tv_expression0) = _v in
        ((let (_menhir_stack, _menhir_s, e1) = _menhir_stack in
        let _v : 'tv_expression1 = 
# 190 "parser.mly"
    ( EApp (e1, e2) )
# 3112 "parser.ml"
         in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)) : 'freshtv394)
    | MenhirState50 | MenhirState51 | MenhirState177 | MenhirState175 | MenhirState171 | MenhirState169 | MenhirState166 | MenhirState63 | MenhirState68 | MenhirState148 | MenhirState78 | MenhirState138 | MenhirState136 | MenhirState131 | MenhirState127 | MenhirState80 | MenhirState114 | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expression0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (e : 'tv_expression0) = _v in
        ((let _v : 'tv_expression1 = 
# 188 "parser.mly"
    ( e )
# 3127 "parser.ml"
         in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)) : 'freshtv398)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3135 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expression0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3143 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (e : 'tv_expression0) = _v in
        ((let (((_menhir_stack, _menhir_s, tag), _), _, ts) = _menhir_stack in
        let _v : 'tv_expression1 = 
# 205 "parser.mly"
    ( EConApp (tag, ts, [e]) )
# 3151 "parser.ml"
         in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)) : 'freshtv402)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv405 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3159 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_expression0) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3167 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (e : 'tv_expression0) = _v in
        ((let (_menhir_stack, _menhir_s, tag) = _menhir_stack in
        let _v : 'tv_expression1 = 
# 194 "parser.mly"
    ( EConApp (tag, [], [e]) )
# 3175 "parser.ml"
         in
        _menhir_goto_expression1 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
    | _ ->
        _menhir_fail ()

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_policy = 
# 159 "parser.mly"
    ( Normal )
# 3186 "parser.ml"
     in
    _menhir_goto_policy _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv389) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_policy = 
# 163 "parser.mly"
    ( High )
# 3199 "parser.ml"
     in
    _menhir_goto_policy _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_policy = 
# 161 "parser.mly"
    ( Low )
# 3212 "parser.ml"
     in
    _menhir_goto_policy _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)

and _menhir_reduce19 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3219 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, tag) = _menhir_stack in
    let _v : 'tv_expression0 = 
# 180 "parser.mly"
    ( EConApp (tag, [], [])  )
# 3226 "parser.ml"
     in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv385 * _menhir_state) * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv381 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IDENTIFIER _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv380)) : 'freshtv382)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv383 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 3267 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_constructor_parameters : _menhir_env -> 'ttv_tail -> 'tv_constructor_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv377 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3280 "parser.ml"
    )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3284 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : 'tv_constructor_parameters) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv375 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3291 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (ts : 'tv_constructor_parameters) = _v in
    ((let (_menhir_stack, _menhir_s, tag) = _menhir_stack in
    let _v : 'tv_data_constructor_definition = 
# 251 "parser.mly"
    ( DefCon (tag, ts) )
# 3298 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv373) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_data_constructor_definition) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv359 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3311 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv357 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3319 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SEMISEMI | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv355 * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x0) = _menhir_stack in
            let _v : 'tv_nonempty_list_preceded_BAR_data_constructor_definition__ = let x =
              let x = x0 in
              
# 86 "standard.mly"
    ( x )
# 3334 "parser.ml"
              
            in
            
# 124 "standard.mly"
    ( [ x ] )
# 3340 "parser.ml"
             in
            _menhir_goto_nonempty_list_preceded_BAR_data_constructor_definition__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv356)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv358)) : 'freshtv360)
    | MenhirState45 | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_data_constructor_definition) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv363 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3361 "parser.ml"
            )) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv361 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3368 "parser.ml"
            )) * _menhir_state * 'tv_data_constructor_definition) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | CONSTRUCTOR _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv362)) : 'freshtv364)
        | SEMISEMI | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365 * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_BAR_data_constructor_definition_ = 
# 144 "standard.mly"
    ( [ x ] )
# 3385 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_BAR_data_constructor_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)) : 'freshtv370)) : 'freshtv372)
    | _ ->
        _menhir_fail ()) : 'freshtv374)) : 'freshtv376)) : 'freshtv378)

and _menhir_goto_parameters_typ0_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_parameters_typ0_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv353 * _menhir_state * 'tv_parameters_typ0_) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv351 * _menhir_state * 'tv_parameters_typ0_) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_parameters_typ0_) = Obj.magic _menhir_stack in
        let (_v : (
# 32 "parser.mly"
       (string)
# 3415 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_parameters_typ0_) = Obj.magic _menhir_stack in
        let (t : (
# 32 "parser.mly"
       (string)
# 3423 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, ts) = _menhir_stack in
        let _v : 'tv_typ0 = 
# 116 "parser.mly"
    ( TConApp (t, ts) )
# 3429 "parser.ml"
         in
        _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_parameters_typ0_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)) : 'freshtv354)

and _menhir_goto_list_type_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_type_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv333 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_type_variable_ = 
# 116 "standard.mly"
    ( x :: xs )
# 3453 "parser.ml"
         in
        _menhir_goto_list_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv332)) : 'freshtv334)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv337 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv336)) : 'freshtv338)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typ0 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ0 -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * _menhir_state) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state) * _menhir_state * 'tv_typ0) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * _menhir_state) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state) * _menhir_state * 'tv_typ0) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IDENTIFIER _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv304)) : 'freshtv306)
        | RPAR ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
        | IDENTIFIER _ ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv307 * _menhir_state) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)) : 'freshtv312)
    | MenhirState23 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAR ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | IDENTIFIER _ ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv314)) : 'freshtv316)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317 * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_typ0_ = 
# 144 "standard.mly"
    ( [ x ] )
# 3579 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_typ0_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | IDENTIFIER _ ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
    | MenhirState157 | MenhirState161 | MenhirState159 | MenhirState145 | MenhirState133 | MenhirState105 | MenhirState88 | MenhirState84 | MenhirState75 | MenhirState53 | MenhirState35 | MenhirState17 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv329 * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327 * _menhir_state * 'tv_typ0) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | DEFEQ | DOUBLEARROW | RBRACKET | RPAR | SEMISEMI | STAR | TYPE ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack)
        | IDENTIFIER _ ->
            _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv325 * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)) : 'freshtv328)) : 'freshtv330)
    | _ ->
        _menhir_fail ()

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BEGIN ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | CONSTRUCTOR _v ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FUN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENTIFIER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LAM ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAR ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | QUESTIONMARK ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv302)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv299 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | QUESTIONMARK ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv297 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState56 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | QUESTIONMARK ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IDENTIFIER _ ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv296)) : 'freshtv298)
    | IDENTIFIER _ ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv300)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | DOT ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv294)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 3702 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (x : (
# 32 "parser.mly"
       (string)
# 3712 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expression0 = 
# 178 "parser.mly"
    ( EVar x )
# 3717 "parser.ml"
     in
    _menhir_goto_expression0 _menhir_env _menhir_stack _menhir_s _v) : 'freshtv292)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENTIFIER _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            let (_v : (
# 32 "parser.mly"
       (string)
# 3744 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3752 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv271 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3761 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3768 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | IDENTIFIER _ ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv270)) : 'freshtv272)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv273 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3789 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)) : 'freshtv278)
        | QUESTIONMARK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState71 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * _menhir_state) * _menhir_state) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LANGLE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | RANGLE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | IDENTIFIER _ ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv280)) : 'freshtv282)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv284)) : 'freshtv286)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 3828 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv267 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3837 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | IDENTIFIER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3850 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState79 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3859 "parser.ml"
        )) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | IDENTIFIER _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv260)) : 'freshtv262)
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3878 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState79 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3887 "parser.ml"
        )) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BEGIN ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CONSTRUCTOR _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | FUN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IDENTIFIER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LAM ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LET ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | QUESTIONMARK ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv264)) : 'freshtv266)
    | AND | BAR | COMMA | END | EOF | IN | RPAR | WITH ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv268)

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv257 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | MATCH ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BEGIN ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CONSTRUCTOR _v ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | FUN ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IDENTIFIER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LAM ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LET ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAR ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv252)) : 'freshtv254)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 3963 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3972 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | OF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv243 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3981 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3985 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv241 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 3992 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 3996 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | IDENTIFIER _ ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv242)) : 'freshtv244)
    | BAR | SEMISEMI | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
        ((let _v : 'tv_constructor_parameters = 
# 245 "parser.mly"
    ( [] )
# 4016 "parser.ml"
         in
        _menhir_goto_constructor_parameters _menhir_env _menhir_stack _v) : 'freshtv246)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4026 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv239 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CONSTRUCTOR _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39) : 'freshtv240)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_parameters_typ0_ = 
# 98 "parser.mly"
    ( [] )
# 4056 "parser.ml"
     in
    _menhir_goto_parameters_typ0_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENTIFIER _ ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv238)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_type_variable_ = 
# 114 "standard.mly"
    ( [] )
# 4084 "parser.ml"
     in
    _menhir_goto_list_type_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce82 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_type_variable -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, tv) = _menhir_stack in
    let _v : 'tv_typ0 = 
# 114 "parser.mly"
    ( TFvar tv )
# 4094 "parser.ml"
     in
    _menhir_goto_typ0 _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_type_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_type_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv225 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_type_variable_ = 
# 146 "standard.mly"
    ( x :: xs )
# 4111 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)) : 'freshtv226)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv235 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv233 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv229 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv227 * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, head0), _, tail0) = _menhir_stack in
            let _v : 'tv_parameters_type_variable_ = let xs =
              let tail = tail0 in
              let head = head0 in
              
# 70 "parser.mly"
    ( head :: tail )
# 4136 "parser.ml"
              
            in
            
# 102 "parser.mly"
    ( xs )
# 4142 "parser.ml"
             in
            _menhir_goto_parameters_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv228)) : 'freshtv230)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv231 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_separated_nonempty_list_COMMA_type_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_data_type_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_data_type_definition_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state * 'tv_list_data_type_definition_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMISEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_list_data_type_definition_) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BEGIN ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | CONSTRUCTOR _v ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | FUN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | IDENTIFIER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LAM ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LPAR ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv210)) : 'freshtv212)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv213 * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * _menhir_state * 'tv_data_type_definition) * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state * 'tv_data_type_definition) * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_data_type_definition_ = 
# 116 "standard.mly"
    ( x :: xs )
# 4210 "parser.ml"
         in
        _menhir_goto_list_data_type_definition_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | _ ->
        _menhir_fail ()

and _menhir_goto_parameters_type_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_parameters_type_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv207 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv205 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) = Obj.magic _menhir_stack in
        let (_v : (
# 32 "parser.mly"
       (string)
# 4233 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv199 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4241 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DEFEQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4250 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv193 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4257 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | CONSTRUCTOR _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv194)) : 'freshtv196)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv197 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4276 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)) : 'freshtv206)) : 'freshtv208)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv191 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENTIFIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 32 "parser.mly"
       (string)
# 4302 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state) = Obj.magic _menhir_stack in
        let (x : (
# 32 "parser.mly"
       (string)
# 4310 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_type_variable = 
# 110 "parser.mly"
    ( x )
# 4316 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_type_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState4 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv149 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv147 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv143 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv141 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv142)) : 'freshtv144)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv145 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
        | MenhirState8 | MenhirState6 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv161 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv153 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv151 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv152)) : 'freshtv154)
            | RPAR ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_COMMA_type_variable_ = 
# 144 "standard.mly"
    ( [ x ] )
# 4384 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)) : 'freshtv162)
        | MenhirState1 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_parameters_type_variable_ = 
# 100 "parser.mly"
    ( [ x ] )
# 4403 "parser.ml"
             in
            _menhir_goto_parameters_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)) : 'freshtv166)
        | MenhirState161 | MenhirState145 | MenhirState133 | MenhirState105 | MenhirState88 | MenhirState84 | MenhirState75 | MenhirState53 | MenhirState35 | MenhirState17 | MenhirState29 | MenhirState23 | MenhirState21 | MenhirState18 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            (_menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) : 'freshtv168)
        | MenhirState65 | MenhirState64 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_type_variable) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | QUOTE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DOT ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv170)) : 'freshtv172)
        | MenhirState159 | MenhirState157 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv181 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
            ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_type_variable) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv175 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4441 "parser.ml"
                )) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv173 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4448 "parser.ml"
                )) * _menhir_state * 'tv_type_variable) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | QUOTE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | IDENTIFIER _ ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159) : 'freshtv174)) : 'freshtv176)
            | ARROW | DEFEQ | DOUBLEARROW | IDENTIFIER _ ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv177 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)) : 'freshtv180)) : 'freshtv182)
        | _ ->
            _menhir_fail ()) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)) : 'freshtv192)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState182 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * 'tv_data_type_definition) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv15 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4506 "parser.ml"
        )) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState175 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17 * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4515 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv19 * _menhir_state) * _menhir_state * 'tv_pattern) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state) * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv23 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4534 "parser.ml"
        )) * _menhir_state * 'tv_sch) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv25 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4543 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv27 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4552 "parser.ml"
        )) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv29 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_implicit) * (
# 32 "parser.mly"
       (string)
# 4561 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_let_rec_definition) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv33 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4575 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv35 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4584 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv37 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4593 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv39 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4602 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4611 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_typs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4620 "parser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv45 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4629 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv47 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4638 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv51 * _menhir_state) * _menhir_state * 'tv_expression) * _menhir_state) * _menhir_state * 'tv_branch) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_pattern) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv57 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4667 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_typ__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4676 "parser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv61 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4685 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4689 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4693 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4702 "parser.ml"
        )) * _menhir_state) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4706 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4720 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state * 'tv_expression) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expression1) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_expression1) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4754 "parser.ml"
        )) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4763 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv83 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 4772 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv85 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_policy) * (
# 32 "parser.mly"
       (string)
# 4781 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state) * _menhir_state * 'tv_list_type_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_separated_nonempty_list_AND_let_rec_definition_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state * 'tv_list_data_type_definition_) * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_list_data_type_definition_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4850 "parser.ml"
        )) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv113 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4859 "parser.ml"
        )) * _menhir_state) * _menhir_state * 'tv_data_constructor_definition) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv117 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4873 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4877 "parser.ml"
        )) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv121 * _menhir_state) * _menhir_state * 'tv_typ0) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_typ0) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv127 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4906 "parser.ml"
        )) * _menhir_state * (
# 32 "parser.mly"
       (string)
# 4910 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv129 * _menhir_state) * _menhir_state * 'tv_parameters_type_variable_) * (
# 32 "parser.mly"
       (string)
# 4919 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv131 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv133 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_type_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv140)

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_data_type_definition_ = 
# 114 "standard.mly"
    ( [] )
# 4953 "parser.ml"
     in
    _menhir_goto_list_data_type_definition_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState1 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv5 * _menhir_state) * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | QUOTE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv6)) : 'freshtv8)
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENTIFIER _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState1 in
        ((let _v : 'tv_parameters_type_variable_ = 
# 98 "parser.mly"
    ( [] )
# 4990 "parser.ml"
         in
        _menhir_goto_parameters_type_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv12)

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 38 "parser.mly"
       (Ast.program)
# 5001 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TYPE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SEMISEMI ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2)) : 'freshtv4))



