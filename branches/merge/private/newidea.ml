open Util
open Os
open Common

(**********************************************************************)
(* GENERIC TREE CODE                                                  *)
(**********************************************************************)

(* An 'a tree has edges labeled with names and node values of
   type 'a.  Usually we will require that no two edges of a Node have
   the same name, but this is not enforced in the datatype.  At times
   we may also assume that the children of a node are sorted by name. *)
type 'a tree =
  Node of 'a * (name * 'a tree) list

let leaf x = Node(x,[])

(* mapU: ('a -> 'a -> 'b) -> 'a -> 'a tree -> 'a tree -> 'b tree
   Map over two trees. The resulting tree has the union of the paths
   of the argument trees. The second argument is a default value to
   be used if one tree is missing a path that appears in the other tree. *)
let rec mapU f default t1 t2 =
  let mappit = mapU f default in
  let defaultTree = Node(default,[]) in
  let leftMissing t = mappit defaultTree t in
  let rightMissing t = mappit t defaultTree in
  match t1,t2 with
  | Node(x1,children1),Node(x2,children2) ->
      (* We don't assume the children are sorted, we sort them ourself *)
      let children1 = Sort.list
          (fun (n1,_) (n2,_) -> compareName n1 n2 < 0) children1 in
      let children2 = Sort.list
          (fun (n1,_) (n2,_) -> compareName n1 n2 < 0) children2 in

      let rec mapChildren c1 c2 =
        match c1,c2 with
          [],[] -> []
        | (n1,t1)::c1',[] -> 
            (n1,rightMissing t1)::(mapChildren c1' [])
        | [],(n2,t2)::c2' ->
            (n2,leftMissing t2)::(mapChildren [] c2')
        | (n1,t1)::c1',(n2,t2)::c2' ->
            if compareName n1 n2 < 0
            then (n1,rightMissing t1)::(mapChildren c1' c2)
            else if compareName n1 n2 > 0
            then (n2,leftMissing t2)::(mapChildren c1 c2')
            else (n1,mappit t1 t2)::(mapChildren c1' c2') in
      Node(f x1 x2,
           mapChildren children1 children2)


(**********************************************************************)
(* SYNCHRONIZER-RELEVANT CODE                                         *)
(**********************************************************************)

(*****************************************************************)
(* UPDATE DETECTION = recursive ls + compare to old recursive ls *)
(*****************************************************************)

type content =
   Ino of float                 (* last synchronization time *)
        * int                   (* inode number *)
 | Dig of string                (* fingerprint of file contents *)

(* FIX: maybe we should have an 'a filetype, where 'a could be
   inode/modtime or digest. Then we would be sure that a directory
   listing does not mix the two. *)
type filetype =
    File of desc * content
  | Dir of desc
  | Absent

let filetype2string = function
  | Absent -> "<absent>"
  | File(d,c) ->
      Printf.sprintf "<file> %s" (desc2string d)
  | Dir d ->
      Printf.sprintf "<dir> %s" (desc2string d)

(* listDir: fspath -> path -> filetype tree option *)
let listDir fspath thePath =
  let mkContent fspath thePath =
    match Sys.os_type with
      "Win32" ->
        Dig(md5Digest fspath thePath)
    | "Unix" ->
        let newinfo = fileInfo fspath thePath in
        Ino (newinfo.lastmod, newinfo.inode)
    | os -> raise(Can'tHappen("listDir",
                              Printf.sprintf "unsupported os %s" os)) in
  let rec loop thePath =
    let theDesc = describe fspath thePath [Mod;Size] in
    let theType = typeOf fspath thePath in
    match theType with
    | SREG ->
        let content = mkContent fspath thePath in
        Node(File(theDesc,content),[])
    | SDIR ->
        let dirChildren =
          List.map
            string2name
            (StringSet.elements (childrenOf fspath thePath)) in
        Node(Dir theDesc,
             List.map
               (fun name -> (name, loop (childPath thePath name)))
               dirChildren)
    | _ -> raise(Can'tHappen("listDir",
                             Printf.sprintf "unimplemented file type %s"
                             (filetype2string theType))) in
  match typeOf fspath thePath with
  | NOTHING -> None
  | SREG | SDIR -> Some(loop thePath)
  | theType -> raise(Can'tHappen("listDir",
                                 Printf.sprintf "unimplemented file type %s"
                                   (filetype2string theType)))
  
type change =
  | Created
  | Modified
  | Deleted
  | NoChange

let change2string = function
  | Created -> "Created"
  | Modified -> "Modified"
  | Deleted -> "Deleted"
  | NoChange -> "NoChange"

(* whatHappened : filetype -> filetype -> change *)
let whatHappened f1 f2 =
  match f1,f2 with
  | Absent,Absent -> NoChange
  | Absent,_ -> Created
  | _,Absent -> Deleted
  | File _,Dir _ -> Modified
  | Dir _,File _ -> Modified
  | File(d1,f1),File(d2,f2) ->
      (match f1,f2 with
        Ino(m1,i1),Ino(m2,i2) ->
          (* FIX: we should optimize this to check digests *)
          if (m2>m1 || i1<>i2) then Modified else NoChange
      | Dig d1,Dig d2 ->
          if (d1<>d2) then Modified else NoChange
      | _ -> raise(Can'tHappen("checkDirty","comparing Digest and Inode")))
  | Dir d1,Dir d2 -> NoChange

(* whatHappenedTree : filetype tree -> filetype tree -> (filetype*change) tree *)
let whatHappenedTree t1 t2 =
  mapU 
    (fun f1 f2 -> (f2,whatHappened f1 f2))
    Absent
    t1
    t2

(*******************************************************************************)
(* RECONCILE = compare changes to 2 recursive listings and make recommendation *)
(*******************************************************************************)

type direction =
    DoNothing (* Added, corresponds to NoUpdates *)
  | Conflict
  | Replica1ToReplica2
  | Replica2ToReplica1

let direction2string = function
    DoNothing -> "DoNothing"
  | Conflict -> "Conflict"
  | Replica1ToReplica2 -> "Replica1ToReplica2"
  | Replica2ToReplica1 -> "Replica2ToReplica1"

(* recommend : change -> change -> direction *)
let recommend c1 c2 =
  match c1,c2 with
    NoChange,NoChange -> DoNothing
  | NoChange,_ -> Replica2ToReplica1
  | _,NoChange -> Replica1ToReplica2
  | _,_ -> Conflict

(* reconcile :
   (filetype*change) tree
   -> (filetype*change) tree
   -> (filetype*filetype*direction) tree *)
let reconcile t1 t2 =
  let t = mapU 
      (fun (f1,c1) (f2,c2) -> (f1,f2,recommend c1 c2))
      (Absent,NoChange)
      t1
      t2 in
  let rec prefixClose t =
    match t with
      Node(_,[]) -> t (* This is an optimization *)
    | Node((f1,f2,direction),children) ->
        let children1 =
          List.map
            (fun (n,t') -> (n,prefixClose t'))
            children in
        let directions = List.map (fun (_,Node((_,_,d),_)) -> d) children1 in
        let combine d1 d2 =
          if d1=DoNothing then d2
          else if d2=DoNothing then d1
          else if (d1=d2) then d1
          else Conflict in
        let direction = List.fold_left combine direction directions in
        Node((f1,f2,direction), children1) in
  prefixClose t











(* FOR TESTING *)

(* iterPath : (path -> 'a -> 'b) -> 'a tree -> unit *)
let iterPath f t =
  let rec iP p t =
    match t with
    | Node(x,l) ->
        begin
          ignore (f p x);
          List.iter
            (fun (n,t) ->
              iP (childPath p n) t)
            l
        end
  in iP [] t


let printTree f t =
  let rec formatLast p =
    match p with
      [] -> ""
    | [last] -> name2string last^" "
    | hd::tl -> " "^formatLast tl in
  iterPath
    (fun p a ->
      Printf.printf "%s" (formatLast p);
      f a;
      Printf.printf "\n")
    t;
  flush stdout

let ls s =
  match listDir (localString2fspath s) [] with
    None -> ()
  | Some t ->
      printTree
        (fun f -> Printf.printf "%s" (filetype2string f))
        t

let pWhatHappened t =
  printTree
    (fun (f,c) ->
      Printf.printf "%s,%s"
        (filetype2string f)
        (change2string c))
    t

let pReconcile t =
  printTree
    (fun (f1,f2,d) ->
      Printf.printf "%s,%s,%s"
        (filetype2string f1)
        (filetype2string f2)
        (direction2string d))
    t

let wtest s1 s2 =
  match listDir (localString2fspath s1) [],listDir (localString2fspath s2) [] with
    None,_ -> ()
  | _,None -> ()
  | Some t1,Some t2 ->
      pWhatHappened (whatHappenedTree t1 t2)







(**********************************************************************)
(* CURRENTLY UNUSED STUFF                                             *)
(**********************************************************************)


(*


(* checkDirty : filetype -> filetype -> bool *)
let checkDirty f1 f2 =
  match f1,f2 with
    Absent,Absent -> false
  | Absent,_ -> true
  | _,Absent -> true
  | File _,Dir _ -> true
  | Dir _,File _ -> true
  | File(d1,f1),File(d2,f2) ->
      (match f1,f2 with
        Ino(m1,i1),Ino(m2,i2) -> (m2>m1 || i1<>i2)
      | Dig d1,Dig d2 -> (d1<>d2)
      | _ -> raise(Can'tHappen("checkDirty","comparing Digest and Inode")))
  | Dir d1,Dir d2 -> false

type updatetree = (filetype * filetype * bool) tree

(* checkDirtyTree : filetype tree -> filetype tree -> updatetree *)
let checkDirtyTree t1 t2 =
  let m = mapU 
      (fun f1 f2 -> (f1,f2,checkDirty f1 f2))
      Absent
      t1
      t2 in
  let rec prefixClose t =
    match t with
      Node((f1,f2,dirty),children) ->
        if dirty then t
        else
          let children1 =
            List.map
              (fun (n,t') -> (n,prefixClose t'))
              children in
          if List.exists
              (fun (n,Node((_,_,dirtyChild),_)) -> dirtyChild)
              children1
          then Node((f1,f2,true), children1)
          else t in
  let m = prefixClose m in
  m



type updateInfo =
  | Deleted of desc        (* A deleted file or directory *)
  | Absent                 (* Path is not defined *)
  | Error of string        (* Error when looking up the path *)
  | FileModified of desc   (* A modified file *)
  | FileUnchanged of desc  (* Unchanged file *)
  | DirNew of desc         (* A new directory *)
  | DirOld of desc         (* A directory with some dirty child *)
  | DirUnchanged of desc   (* A directory with no dirty children *)

let isDirty = function
  | Deleted _ -> true
  | Absent -> false
  | Error _ -> true
  | FileModified _ -> true
  | FileUnchanged _ -> false
  | DirNew _ -> true
  | DirOld _ -> true
  | DirUnchanged _ -> false

let isDir = function
  | Deleted _ -> false
  | Absent -> false
  | Error _ -> false (* CAREFUL *)
  | FileModified _ -> false
  | FileUnchanged _ -> false
  | DirNew _ -> true
  | DirOld  _-> true
  | DirUnchanged _ -> true

type direction =
    DoNothing (* Added, corresponds to NoUpdates *)
  | Conflict
  | Replica1ToReplica2
  | Replica2ToReplica1

let defaultDirection ui1 ui2 =
  if not(isDirty ui1) & not(isDirty ui2) then
    Some(DoNothing)
  else if isDir ui1 & isDir ui2 then
    None (* Default direction depends on children *)
  else if not(isDirty ui1) then
    Some(Replica2ToReplica1)
  else if not(isDirty ui2) then
    Some(Replica1ToReplica2)
  else
    Some(Conflict)

(* Invariants:
   Leaf(ui) => ui is Deleted, FileModified, or Error
   Node(ui,t) => ui is DirNew or DirOld
   Node(DirNew,t) => every Leaf in t is FileModified, every Node is DirNew
 *)
type updateItem = updateInfo tree

(* Invariants:
   Node((ui1,ui2,d),t) => ui1 or ui2 is DirNew or DirOld
   ... *)
type updateActions =
    (updateInfo*updateInfo*direction) tree



(* The idea: update detectors produce updateItems. The reconciler
   combines two updateItems into an updateAction. The direction in
   each node of an updateAction can be changed by the gui. An
   updateAction has all the info needed to produce the instructions
   for the transport agent. *)

(* Combine two updateItems into an updateAction *)
let rec combine u1 u2 =
  match u1,u2 with
  | Node(ui1,x1),Node(ui2,x2) ->
      let x1 = Sort.list
          (fun (n1,_) (n2,_) -> n1<=n2) x1 in
      let x2 = Sort.list
          (fun (n1,_) (n2,_) -> n1<=n2) x2 in
      let rec f x1 x2 =
        match x1,x2 with
        | [],[] -> []
        | [],(n2,t2)::z2 ->
            (n2,combine (leaf Absent) t2)::(f x1 z2)
        | (n1,t1)::z1,[] ->
            (n1,combine t1 (leaf Absent))::(f z1 [])
        | (n1,t1)::z1,(n2,t2)::z2 ->
            if n1=n2 then (n1,combine t1 t2)::(f z1 z2)
            else if n1<=n2
            then (n1,combine t1 (leaf Absent))::(f z1 x2)
            else (n2,combine (leaf Absent) t2)::(f x1 z2) in
      Node( (ui1,ui2,defaultDirection ui1 ui2),
           f x1 x2)

(* We can use a tree to compactly encode a prefix-closed set of
   paths.  Save a log factor!! *)
type paths = unit tree

(* Given an 'a tree, return its paths. *)
let paths_of t =
  map (fun x -> ()) t

*)


(* merge : 'a tree -> 'a tree -> 'a tree
   Merge two 'a trees.  The node values of the first tree have
   precedence over the values of the second tree. *)
let rec merge t1 t2 =
  match t1,t2 with
  | Node(x1,children1),Node(_,children2) ->
      (* We don't assume the children are sorted, we sort them ourself *)
      let children1 = Sort.list
          (fun (n1,_) (n2,_) -> compareName n1 n2 < 0) children1 in
      let children2 = Sort.list
          (fun (n1,_) (n2,_) -> compareName n1 n2 < 0) children2 in
      let rec merge0 c1 c2 =
        match c1,c2 with
        | _,[] -> c1
        | [],_ -> c2
        | (n1,t1)::c1',(n2,t2)::c2' ->
            if compareName n1 n2 < 0 then (n1,t1)::(merge0 c1' c2)
            else if compareName n1 n2 > 0 then (n2,t2)::(merge0 c1 c2')
            else (n1,merge t1 t2)::(merge0 c1' c2') in
      Node(x1,merge0 children1 children2)

(* map : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map f t =
  match t with
  | Node(x,l) ->
      Node(f x,
           List.map (fun (n,t) -> (n,map f t)) l)

(* mapPath : (path -> 'a -> 'b) -> 'a tree -> 'b tree *)
let mapPath f t =
  let rec mP p t =
    match t with
    | Node(x,l) ->
        Node(f p x,
             List.map
               (fun (n,t) ->
                 (n,mP (childPath p n) t))
               l)
  in mP [] t

(* map2 : ('a -> 'b -> 'c) -> 'a tree -> 'b tree -> 'c tree
   Like List.map2, we require that the tree arguments have the same
   tree structure, else Invalid_argument "Tree.map2" is raised. *)
let rec map2 f t1 t2 =
  match t1,t2 with
  | Node(x1,l1),Node(x2,l2) ->
      Node(f x1 x2,
           try
             List.map2
               (fun (n1,t1) (n2,t2) ->
                 if compareName n1 n2 <> 0 then
                   raise(Invalid_argument "Tree.map2")
                 else (n1,map2 f t1 t2))
               l1 l2
           with Invalid_argument "List.map2" ->
             raise(Invalid_argument "Tree.map2"))

(* map2Path : (path -> 'a -> 'b -> 'c) -> 'a tree -> 'b tree -> 'c tree
   Like List.map2, we require that the tree arguments have the same
   tree structure, else Invalid_argument "Tree.map2Path" is raised. *)
let rec map2 f t1 t2 =
  let rec m2P p t1 t2 =
    match t1,t2 with
    | Node(x1,l1),Node(x2,l2) ->
        Node(f p x1 x2,
             try
               List.map2
                 (fun (n1,t1) (n2,t2) ->
                   if compareName n1 n2 <> 0 then
                     raise(Invalid_argument "Tree.map2Path")
                   else (n1,m2P (childPath p n1) t1 t2))
                 l1 l2
             with Invalid_argument "List.map2" ->
               raise(Invalid_argument "Tree.map2Path"))
  in m2P [] t1 t2

(* iter : ('a -> 'b) -> 'a tree -> unit
   Iteration on a tree in prefix order *)
let rec iter f t =
  match t with
  | Node(x,l) ->
      begin
        ignore (f x);
        List.iter (fun (n,t) -> iter f t) l
      end

(* post : ('a -> 'b) -> 'a tree -> unit
   Iteration on a tree in postfix order *)
let rec post f t =
  match t with
  | Node(x,l) ->
      begin
        List.iter (fun (n,t) -> post f t) l;
        ignore (f x)
      end

(* postPath : (path -> 'a -> 'b) -> 'a tree -> unit *)
let postPath f t =
  let rec pP p t =
    match t with
    | Node(x,l) ->
        begin
          List.iter
            (fun (n,t) ->
              pP (childPath p n) t)
            l;
          ignore (f p x)
        end
  in pP [] t

(* sort : 'a tree -> 'a tree *)
let rec sort t =
  let compareNameBool pair1 pair2 =
    compareName (fst pair1) (fst pair2) <= 0 in
  match t with
  | Node(x,l) ->
      let sorted = Sort.list compareNameBool l in
      Node(x,
           List.map (fun (n,t) -> (n,sort t)) sorted)

