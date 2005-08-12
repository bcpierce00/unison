(* TODO:
   - get rid of the axiom extensionality

   - existence of a maximal triple for ok
     (for ok and for a "more synchronized" relation)
   - this triple is the one that synchronize the most the filesystems
*)

Implicit Arguments On.

Inductive Option [A : Set] : Set :=
    Some : (x : A) (Option A)
  | None : (Option A).

Inductive Unit : Set := unit : Unit.

(****)

Variable Kind : Set.
Hypothesis eq_dec_kind : (k,k':Kind){k = k'} + {~k = k'}.
Variable Name : Set.
Hypothesis eq_dec_name : (k,k':Name){k = k'} + {~k = k'}.

Inductive FS : Set :=
    Node : Kind -> (Name -> FS) -> FS
  | Bot :  FS.

Definition child : Kind -> FS -> Name -> FS :=
  [k;a;n]
  Cases a of
    (Node k' c) =>
       Cases (eq_dec_kind k k') of
         (left equality) => (c n)
       | (right _)       => Bot
       end
  | _ =>
       Bot
  end.

(****)

Inductive Path : Set :=
    Here : Path
  | Child : Kind -> Name -> Path -> Path.

Fixpoint follows [p : Path] : FS -> FS :=
  [a : FS]
  Cases p of
    Here           => a
  | (Child k n p') => (child k (follows p' a) n)
  end.

Fixpoint overwrite [v : FS; fs : FS; path : Path] : (Option FS) :=
  Cases path of
    Here =>
      (Some v)
  | (Child k nm p) =>
      Cases fs of
        Bot =>
          (None FS)
      | (Node k' c) =>
          Cases (eq_dec_kind k k') of
            (left equality) =>
               Cases (overwrite v (c nm) p) of
                 None =>
                   (None FS)
               | (Some v') =>
                   (Some
                     (Node k'
                        [n : Name]
                        Cases (eq_dec_name n nm) of
                          (left _)  => v'
                        | (right _) => (c n)
                        end))
               end
          | (right _)       =>
               (None FS)
          end
      end
  end.    

(****)

Variable Action : Set -> Set.

Inductive Result [A : Set] : Set :=
    Success : (x : A) (Result A)
  | Failure : (Result A).

Inductive Volume : Set := A : Volume | B : Volume | Arch : Volume.

Variable eval :
  (A : Set) (a : (Action A))
  (Volume -> FS) -> ((Result A) * (Volume -> FS)).

Variable bind : (A, B : Set)(Action A) -> (A -> (Action B)) -> (Action B).

Axiom bind_spec :
  (A, B : Set) (a : (Action A)) (f : A -> (Action B))
  (eval (bind a f)) =
  [st : Volume -> FS]
  Cases (eval a st) of
    (Failure, st')       => ((Failure B), st')
  | ((Success res), st') => (eval (f res) st')
  end.

Variable return : (A : Set) A -> (Action A).

Axiom return_spec :
  (A : Set) (x : A)
  (eval (return x)) =
  [st : Volume -> FS] ((Success x), st).

(****)

Variable write : Volume -> Path -> FS -> (Action Unit).
Variable read : Volume -> Path -> (Action (Option Kind)).

Require EqDecide.

Lemma eq_dec_volume : (k,k':Volume){k = k'} + {~k = k'}.
Decide Equality.
Qed.

Definition replace [st : Volume -> FS; v : Volume; fs' : FS] :=
  [v' : Volume]
  Cases (eq_dec_volume v v') of
    left  => fs'
  | right => (st v')
  end.

Axiom write_spec :
  (v : Volume) (p : Path) (fs : FS)
  (eval (write v p fs)) =
  [st : Volume -> FS]
  Cases (overwrite fs (st v) p) of
    None =>
      ((Failure Unit), st)
  | (Some fs') =>
      ((Success unit), (replace st v fs'))
  end.

Axiom read_spec :
  (v : Volume) (p : Path) (fs : FS)
  (eval (read v p)) =
  [st : Volume -> FS]
  Cases (follows p (st v)) of
    Bot =>
      ((Success (None Kind)), st)
  | (Node k _) =>
      ((Success (Some k)), st)
  end.

(*

findUpdates_A : (path->bool) Action
  such that...        

unison : unit Action

unison =
  bind findUpdates_A (fun uA ->
  bind findUpdates_B (fun uB ->
  reconcile uA uB))

*)
