open Tk;;          

let top = openTk ();; 

let theFrame = Frame.create top;;
pack ~side:`Top ~fill:`X ~pady:4 [theFrame];;

let rec grabtext t =
  try
    let l = input_line stdin in
    grabtext(t ^ l ^ "\n")
  with 
    End_of_file -> t;;

let theLabel = 
  Label.create ~text:(grabtext "") ~justify:`Left theFrame;; 
pack ~side:`Left [theLabel];

let buttonFrame = Frame.create top in
pack ~side:`Top ~fill:`X ~pady:4 [buttonFrame];
let ok = Button.create 
    ~relief:`Raised ~text:"OK" 
    ~background:`Green ~activebackground:`Green 
    ~command:(fun () -> exit 0) buttonFrame in
pack ~side:`Left ~padx:4 [ok];
let cancel = Button.create 
    ~relief:`Raised 
    ~background:`Red ~activebackground:`Red
    ~text:"Cancel"
  ~command:(fun () -> exit 1) buttonFrame in
pack ~side:`Right ~padx:4 [cancel];

mainLoop() ;;   


