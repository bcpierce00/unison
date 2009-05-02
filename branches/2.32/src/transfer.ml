(* Unison file synchronizer: src/transfer.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


(* rsync compression algorithm

     To compress, we use a compression buffer with a size a lot
   greater than the size of a block, typically half a megabyte. This
   buffer is loaded with the file contents. Its valid part is
   represented by its limit 'length'.
     We scan the file contents by sliding a window with the size of a
   block over the compression buffer. This window is represented by
   its 'offset' and its size 'blockSize'.
     We transmit STRING tokens, containing the differences between the
   files, and BLOCK tokens, containing the number of a block from the
   old file found in the new one. The data not transmitted yet are
   pointed by 'toBeSent'.
     For each position of the window, we compute the checksum of the
   block it contains and try to find a matching entry in the hashed
   block information data. If there is a match, we compute the
   fingerprint of our block to match it with the candidates'
   fingerprints :
   - if there is a match, we've just hit, we can transmit the data not
   sent yet as a STRING token and emit a BLOCK token representing our
   match, then we slide the window one block ahead and try again;
  - in any other case, we've missed, we just slide the window one
   character ahead and try again.
     If the file size is greater than the compression buffer size,
   then we have to update the compression buffer when the window
   reaches its limit. We do so by sending any data not sent yet, then
   copying the end of the buffer at its beginning and filling it up
   with the file contents coming next. We now place our window at the
   beginning of the buffer and we continue the process.
     The compression is over when we reach the end of the file. We
   just have to send the data not sent yet together with the last
   characters that could not fill a block.  *)

let debug  = Trace.debug "transfer"
let debugV = Trace.debug "transfer+"
let debugToken = Trace.debug "rsynctoken"
let debugLog =   Trace.debug "rsynclog"

open Lwt

type transfer_instruction = string * int * int

type transmitter = transfer_instruction -> unit Lwt.t

(*************************************************************************)
(*                          BUFFERED DISK I/O                            *)
(*************************************************************************)

let reallyRead infd buffer pos length =
  let rec read pos length =
    let n = input infd buffer pos length in
    if n = length || n = 0 then pos + n else
    read (pos + n) (length - n)
  in
  read pos length - pos

let rec reallyWrite outfd buffer pos length =
  output outfd buffer pos length

(*************************************************************************)
(*                            TOKEN QUEUE                                *)
(*************************************************************************)

(* There are two goals:
   1) to merge consecutive compatible tokens (catenating STRING tokens
      and combining BLOCK tokens when the referenced blocks are
      consecutive)
   2) to delay the transmission of the tokens across the network until
      their total size is greater than a limit, not to make a costly
      RPC for each token (therefore, the rsync module uses memory up to
      (2 * comprBufSize + tokenQueueLimit) bytes at a time) *)

type token =
  | STRING   of string * int * int
  | BLOCK of int
  | EOF

(* Size of a block *)
let blockSize = 700
let blockSize64 = Int64.of_int blockSize

let maxQueueSize = 65500
let maxQueueSizeFS = Uutil.Filesize.ofInt maxQueueSize
type tokenQueue =
  { mutable data : string;       (* the queued tokens *)
    mutable previous : [`Str of int | `Block of int | `None];
                                 (* some informations about the
                                    previous token *)
    mutable pos : int;           (* head of the queue *)
    mutable prog : int }         (* the size of the data they represent *)

(* Size of the data a token represents for the destination host,
   to keep track of the propagation progress *)
let tokenProg t =
  match t with
    STRING (s, pos, len) -> String.length s
  | BLOCK n              -> blockSize
  | EOF                  -> 0

let encodeInt3 s pos i =
  assert (i >= 0 && i < 256 * 256 * 256);
  s.[pos + 0] <- Char.chr ((i lsr 0) land 0xff);
  s.[pos + 1] <- Char.chr ((i lsr 8) land 0xff);
  s.[pos + 2] <- Char.chr ((i lsr 16) land 0xff)

let decodeInt3 s pos =
  (Char.code s.[pos + 0] lsl 0) lor
  (Char.code s.[pos + 1] lsl 8) lor
  (Char.code s.[pos + 2] lsl 16)

let encodeInt2 s pos i =
  assert (i >= 0 && i < 65536);
  s.[pos + 0] <- Char.chr ((i lsr 0) land 0xff);
  s.[pos + 1] <- Char.chr ((i lsr 8) land 0xff)

let decodeInt2 s pos =
  (Char.code s.[pos + 0] lsl 0) lor (Char.code s.[pos + 1] lsl 8)

let encodeInt1 s pos i =
  assert (i >= 0 && i < 256);
  s.[pos + 0] <- Char.chr i

let decodeInt1 s pos =
  Char.code s.[pos + 0]

(* Transmit the contents of the tokenQueue *)
let flushQueue q showProgress transmit cond =
  if cond && q.pos > 0 then begin
    debugToken (fun() -> Util.msg "flushing the token queue\n");
    transmit (q.data, 0, q.pos) >>= (fun () ->
    showProgress q.prog;
    q.pos <- 0; q.prog <- 0; q.previous <- `None;
    return ())
  end else
    return ()

let pushEOF q showProgress transmit =
  flushQueue q showProgress transmit
    (q.pos + 1 > String.length q.data) >>= (fun () ->
  q.data.[q.pos] <- 'E';
  q.pos <- q.pos + 1;
  q.previous <- `None;
  return ())

let pushString q id transmit s pos len =
  flushQueue q id transmit (q.pos + len + 3 > String.length q.data)
    >>= (fun () ->
  if q.pos + 3 + len > String.length q.data then begin
    (* The file is longer than expected, so the string does not fit in
       the buffer *)
    assert (q.pos = 0);
    q.data <- String.create maxQueueSize
  end;
  q.data.[q.pos] <- 'S';
  encodeInt2 q.data (q.pos + 1) len;
  assert (q.pos + 3 + len <= String.length q.data);
  String.blit s pos q.data (q.pos + 3) len;
  q.pos <- q.pos + len + 3;
  q.prog <- q.prog + len;
  q.previous <- `Str len;
  return ())

let rec growString q id transmit len' s pos len =
  let l = min (String.length q.data - q.pos) len in
  String.blit s pos q.data q.pos l;
  assert (q.data.[q.pos - len' - 3] = 'S');
  assert (decodeInt2 q.data (q.pos - len' - 2) = len');
  let len'' = len' + l in
  encodeInt2 q.data (q.pos - len' - 2) len'';
  q.pos <- q.pos + l;
  q.prog <- q.prog + l;
  q.previous <- `Str len'';
  if l < len then
    pushString q id transmit s (pos + l) (len - l)
  else
    return ()

let pushBlock q id transmit pos =
  flushQueue q id transmit (q.pos + 5 > String.length q.data) >>= (fun () ->
  q.data.[q.pos] <- 'B';
  encodeInt3 q.data (q.pos + 1) pos;
  encodeInt1 q.data (q.pos + 4) 1;
  q.pos <- q.pos + 5;
  q.prog <- q.prog + blockSize;
  q.previous <- `Block (pos + 1);
  return ())

let growBlock q id transmit pos =
  let count = decodeInt1 q.data (q.pos - 1) in
  assert (q.data.[q.pos - 5] = 'B');
  assert (decodeInt3 q.data (q.pos - 4) + count = pos);
  assert (count < 255);
  encodeInt1 q.data (q.pos - 1) (count + 1);
  q.prog <- q.prog + blockSize;
  q.previous <- if count = 254 then `None else `Block (pos + 1);
  return ()

(* Queue a new token, possibly merging it with a previous compatible
   token and flushing the queue if its size becomes greater than the
   limit *)
let queueToken q id transmit token =
  match token, q.previous with
    EOF, _ ->
      pushEOF q id transmit
  | STRING (s, pos, len), `Str len' ->
      growString q id transmit len' s pos len
  | STRING (s, pos, len), _ ->
      pushString q id transmit s pos len
  | BLOCK pos, `Block pos' when pos = pos' ->
      growBlock q id transmit pos
  | BLOCK pos, _ ->
      pushBlock q id transmit pos

let makeQueue length =
  { data =
      (* We need to make sure here that the size of the queue is not
         larger than 65538
         (1 byte: header, 2 bytes: string size, 65535 bytes: string) *)
      String.create
        (if length > maxQueueSizeFS then maxQueueSize else
         Uutil.Filesize.toInt length + 10);
    pos = 0; previous = `None; prog = 0 }

(*************************************************************************)
(* GENERIC TRANSMISSION                                                  *)
(*************************************************************************)

let debug = Trace.debug "generic"

(* Slice the file into STRING tokens that are transmitted incrementally *)
let send infd length showProgress transmit =
  debug (fun() -> Util.msg "sending file\n");
  let timer = Trace.startTimer "Sending file using generic transmission" in
  let bufSz = 8192 in
  let bufSzFS = Uutil.Filesize.ofInt 8192 in
  let buf = String.create bufSz in
  let q = makeQueue length in
  let rec sendSlice length =
    let count =
      reallyRead infd buf 0
        (if length > bufSzFS then bufSz else Uutil.Filesize.toInt length) in
    queueToken q showProgress transmit (STRING (buf, 0, count)) >>= (fun () ->
    let length = Uutil.Filesize.sub length (Uutil.Filesize.ofInt count) in
    if count = bufSz && length > Uutil.Filesize.zero then
      sendSlice length
    else
      return ())
  in
  sendSlice length >>= (fun () ->
  queueToken q showProgress transmit EOF >>= (fun () ->
  flushQueue q showProgress transmit true >>= (fun () ->
  Trace.showTimer timer;
  return ())))

let rec receiveRec outfd showProgress data pos maxPos =
  if pos = maxPos then false else
  match data.[pos] with
    'S' ->
      let length = decodeInt2 data (pos + 1) in
      if Trace.enabled "generic" then debug (fun() -> Util.msg
          "receiving %d bytes\n" length);
      reallyWrite outfd data (pos + 3) length;
      showProgress length;
      receiveRec outfd showProgress data (pos + length + 3) maxPos
  | 'E' ->
      true
  | _   ->
      assert false

let receive outfd showProgress (data, pos, len) =
  receiveRec outfd showProgress data pos (pos + len)

(*************************************************************************)
(* RSYNC TRANSMISSION                                                    *)
(*************************************************************************)

module Rsync =
struct

  (* Debug messages *)
  let debug =      Trace.debug "rsync"


(**************************** DESTINATION HOST ***************************)

  (* It is impossible to use rsync when the file size is smaller than
     the size of a block *)
  let blockSizeFs = Uutil.Filesize.ofInt blockSize
  let aboveRsyncThreshold sz = sz >= blockSizeFs

  (* The type of the info that will be sent to the source host *)
  type rsync_block_info = (Checksum.t * Digest.t) list


  (*** PREPROCESS ***)

  (* Preprocess buffer size *)
  let preproBufSize = 8192

  (* Incrementally build arg by executing f on successive blocks (of size
     'blockSize') of the input stream (pointed by 'infd').
     The procedure uses a buffer of size 'bufferSize' to load the input,
     and eventually handles the buffer update. *)
  let blockIter infd f arg maxCount =
    let bufferSize = 8192 + blockSize in
    let buffer = String.create bufferSize in
    let rec iter count arg offset length =
      if count = maxCount then arg else begin
        let newOffset = offset + blockSize in
        if newOffset <= length then
          iter (count + 1) (f buffer offset arg) newOffset length
        else if offset > 0 then begin
          let chunkSize = length - offset in
          String.blit buffer offset buffer 0 chunkSize;
          iter count arg 0 chunkSize
        end else begin
          let l = input infd buffer length (bufferSize - length) in
          if l = 0 then
            arg
          else
            iter count arg 0 (length + l)
        end
      end
    in
    iter 0 arg 0 0

  let rec rev_split_rec accu1 accu2 n l =
    if n = 100000 then
      rev_split_rec (accu2 :: accu1) [] 0 l
    else
      match l with
        []     -> accu2 :: accu1
      | x :: r -> rev_split_rec accu1 (x :: accu2) (n + 1) r

  let rev_split l = rev_split_rec [] [] 0 l

  (* Given a block size, get blocks from the old file and compute a
     checksum and a fingerprint for each one. *)
  let rsyncPreprocess infd =
    debug (fun() -> Util.msg "preprocessing\n");
    debugLog (fun() -> Util.msg "block size = %d bytes\n" blockSize);
    let timer = Trace.startTimer "Preprocessing old file" in
    let addBlock buf offset rev_bi =
      let cs = Checksum.substring buf offset blockSize in
      let fp =   Digest.substring buf offset blockSize in
      (cs, fp) :: rev_bi
    in
    (* Make sure we are at the beginning of the file
       (important for AppleDouble files *)
    LargeFile.seek_in infd 0L;
    (* Limit the number of block so that there is no overflow in
       encodeInt3 *)
    let rev_bi = blockIter infd addBlock [] (256*256*256) in
    let bi = rev_split rev_bi in
    debugLog (fun() -> Util.msg "%d blocks\n" (Safelist.length bi));
    Trace.showTimer timer;
    bi


  (*** DECOMPRESSION ***)

  (* Decompression buffer size *)
  let decomprBufSize = 8192

  (* For each transfer instruction, either output a string or copy one or
     several blocks from the old file. *)
  let rsyncDecompress infd outfd showProgress (data, pos, len) =
    let decomprBuf = String.create decomprBufSize in
    let progress = ref 0 in
    let rec copy length =
      if length > decomprBufSize then begin
        let _ = reallyRead infd decomprBuf 0 decomprBufSize in
        reallyWrite outfd decomprBuf 0 decomprBufSize;
        copy (length - decomprBufSize)
      end else
        let _ = reallyRead infd decomprBuf 0 length in
        reallyWrite outfd decomprBuf 0 length
    in
    let copyBlocks n k =
      LargeFile.seek_in infd (Int64.mul n blockSize64);
      let length = k * blockSize in
      copy length;
      progress := !progress + length
    in
    let maxPos = pos + len in
    let rec decode pos =
      if pos = maxPos then false else
      match data.[pos] with
        'S' ->
          let length = decodeInt2 data (pos + 1) in
          if Trace.enabled "rsynctoken" then
            debugToken (fun() ->
              Util.msg "decompressing string (%d bytes)\n" length);
          reallyWrite outfd data (pos + 3) length;
          progress := !progress + length;
          decode (pos + length + 3)
      | 'B' ->
          let n = decodeInt3 data (pos + 1) in
          let k = decodeInt1 data (pos + 4) in
          if Trace.enabled "rsynctoken" then
            debugToken (fun() -> Util.msg
                "decompressing %d block(s) (sequence %d->%d)\n"
                k n (n + k - 1));
          copyBlocks (Int64.of_int n) k;
          decode (pos + 5)
      | 'E' ->
          true
      | _ ->
          assert false
    in
    let finished = decode pos in
    showProgress !progress;
    finished

(***************************** SOURCE HOST *******************************)

  (*** CUSTOM HASH TABLE ***)

  (* Maximum number of entries in the hash table.
     MUST be a power of 2 !
     Typical values are around an average 2 * fileSize / blockSize. *)
  let hashTableMaxLength = 64 * 1024

  let hash checksum = checksum

  let rec sigLength sigs =
    match sigs with
      []     -> 0
    | x :: r -> Safelist.length x + sigLength r

  (* Compute the hash table length as a function of the number of blocks *)
  let hashTableLength signatures =
    let rec upperPowerOfTwo n n2 =
      if (n2 >= n) || (n2 = hashTableMaxLength) then
        n2
      else
        upperPowerOfTwo n (2 * n2)
    in
    2 * (upperPowerOfTwo (sigLength signatures) 32)

  (* Hash the block signatures into the hash table *)
  let hashSig hashTableLength signatures =
    let hashTable = Array.make hashTableLength [] in
    let rec addList k l l' =
      match l, l' with
        [], [] ->
          ()
      | [], r :: r' ->
          addList k r r'
      | ((cs, fp) :: r), _ ->
          let h = (hash cs) land (hashTableLength - 1) in
          hashTable.(h) <- (k, cs, fp)::(hashTable.(h));
          addList (k + 1) r l'
    in
    addList 0 [] signatures;
    hashTable

  (* Given a key, retrieve the corresponding entry in the table *)
  let findEntry hashTable hashTableLength checksum :
      (int * Checksum.t * Digest.t) list =
    hashTable.((hash checksum) land (hashTableLength - 1))

  (* Log the values of the parameters associated with the hash table *)
  let logHash hashTable hashTableLength =
    let rec probe empty collision i =
      if i = hashTableLength then (empty, collision)
      else begin
        let length = Safelist.length hashTable.(i) in
        let next =
          if length = 0 then probe (empty + 1) collision
          else if length > 1 then probe empty (collision + 1)
          else probe empty collision
        in
        next (i + 1)
      end
    in
    let (empty, collision) = probe 0 0 0 in
    debugLog (fun() -> Util.msg "%d hash table entries\n" hashTableLength);
    debugLog (fun() -> Util.msg
        "%d empty, %d used, %d collided\n"
        empty (hashTableLength - empty) collision)

  (*** MEASURES ***)

  type probes = {
      mutable hitHit : int;
      mutable hitMiss : int;
      mutable nbBlock : int;
      mutable nbString : int;
      mutable stringSize : int
    }

  let logMeasures pb =
((*
    debugLog (fun() -> Util.msg
        "hit-hit = %d, hit-miss = %d, hit rate = %d%%\n"
        pb.hitHit pb.hitMiss
        (if pb.hitHit <> 0 then
           pb.hitHit * 100 / (pb.hitHit + pb.hitMiss)
         else 0));
    debugLog (fun() -> Util.msg
        "%d strings (%d bytes), %d blocks\n"
        pb.nbString pb.stringSize pb.nbBlock);
    let generic = pb.stringSize + pb.nbBlock * blockSize in
    debugLog (fun() -> Util.msg
        "file size = %d bytes\n"
        generic);
    debug (fun() -> Util.msg
        "compression rate = %d%%\n" ((pb.stringSize * 100) / generic))
*))


  (*** COMPRESSION ***)

  (* Compression buffer size *)
  (* MUST be >= 2 * blockSize *)
  let comprBufSize = 8192
  let comprBufSizeFS = Uutil.Filesize.ofInt 8192

  (* Compress the file using the algorithm described in the header *)
  let rsyncCompress sigs infd srcLength showProgress transmit =
    debug (fun() -> Util.msg "compressing\n");
    debugLog (fun() -> Util.msg
        "compression buffer size = %d bytes\n" comprBufSize);
    debugLog (fun() -> Util.msg "block size = %d bytes\n" blockSize);
    assert (comprBufSize >= 2 * blockSize);
    let timer = Trace.startTimer "Compressing the new file" in

    (* Measures *)
    let pb =
      { hitHit = 0; hitMiss = 0; nbBlock = 0; nbString = 0; stringSize = 0 } in
(*
    let transmit tokenList =
      Safelist.iter
        (fun token ->
           match token with
           | STRING s ->
               let length = String.length s in
               if Trace.enabled "rsynctoken" then debugToken (fun() ->
                 Util.msg "transmitting string (%d bytes)\n" length);
               pb.nbString <- pb.nbString + 1;
               pb.stringSize <- pb.stringSize + length
           | BLOCK n ->
               if Trace.enabled "rsynctoken" then debugToken (fun() -> Util.msg
                   "transmitting %d block(s) (sequence %d->%d)\n"
                   1 n (n));
               pb.nbBlock <- pb.nbBlock + k)
        tokenList;
      transmit tokenList
    in
*)

    (* Enable token buffering *)
    let tokenQueue = makeQueue srcLength in
    let flushTokenQueue () =
      flushQueue tokenQueue showProgress transmit true in
    let transmit token = queueToken tokenQueue showProgress transmit token in

    (* Set up the hash table for fast checksum look-up *)
    let hashTableLength = ref (hashTableLength sigs) in
    let blockTable = hashSig !hashTableLength sigs in
    logHash blockTable !hashTableLength;

    (* Create the compression buffer *)
    let comprBuf = String.create comprBufSize in

    (* If there is data waiting to be sent, transmit it as a STRING token *)
    let transmitString toBeSent offset =
      if offset > toBeSent then
        transmit (STRING (comprBuf, toBeSent, offset - toBeSent))
      else
        return ()
    in

    (* Set up the rolling checksum data *)
    let checksum = ref 0 in
    let cksumOutgoing = ref ' ' in
    let cksumTable = ref (Checksum.init blockSize) in

    let absolutePos = ref Uutil.Filesize.zero in

    (* Check the new window position and update the compression buffer
       if its end has been reached *)
    let rec slideWindow newOffset toBeSent length miss : unit Lwt.t =
      if newOffset + blockSize <= length then
        computeChecksum newOffset toBeSent length miss
      else if length = comprBufSize then begin
        transmitString toBeSent newOffset >>= (fun () ->
        let chunkSize = length - newOffset in
        if chunkSize > 0 then begin
          assert(comprBufSize >= blockSize);
          String.blit comprBuf newOffset comprBuf 0 chunkSize
        end;
        let rem = Uutil.Filesize.sub srcLength !absolutePos in
        let avail = comprBufSize - chunkSize in
        let l =
          reallyRead infd comprBuf chunkSize
            (if rem > comprBufSizeFS then avail else
             min (Uutil.Filesize.toInt rem) avail)
        in
        absolutePos :=
          Uutil.Filesize.add !absolutePos (Uutil.Filesize.ofInt l);
        let length = chunkSize + l in
        debugToken (fun() -> Util.msg "updating the compression buffer\n");
        debugToken (fun() -> Util.msg "new length = %d bytes\n" length);
        slideWindow 0 0 length miss)
      end else
        transmitString toBeSent length >>= (fun () ->
        transmit EOF)

    (* Compute the window contents checksum, in a rolling fashion if there
       was a miss *)
    and computeChecksum newOffset toBeSent length miss =
      let cksum =
        if miss then
          Checksum.roll !cksumTable !checksum !cksumOutgoing
            (String.unsafe_get comprBuf (newOffset + blockSize - 1))
        else
          Checksum.substring comprBuf newOffset blockSize
      in
      checksum := cksum;
      cksumOutgoing := String.unsafe_get comprBuf newOffset;
      processBlock newOffset toBeSent length cksum

    (* Try to match the current block with one existing in the old file *)
    and processBlock offset toBeSent length checksum =
      if Trace.enabled "transfer+" then
        debugV (fun() -> Util.msg
            "processBlock offset=%d toBeSent=%d length=%d blockSize = %d\n"
            offset toBeSent length blockSize);
      if Trace.enabled "rsynctoken" then assert
         (0 <= toBeSent && toBeSent <= offset && offset + blockSize <= length);
      match findEntry blockTable !hashTableLength checksum with
      | [] -> miss offset toBeSent length
      | entry ->
          let blockNum = findBlock offset checksum entry None in
          if blockNum = -1 then begin
              pb.hitMiss <- pb.hitMiss + 1;
              miss offset toBeSent length
          end else begin
              pb.hitHit <- pb.hitHit + 1;
              hit offset toBeSent length blockNum
          end

    (* In the hash table entry, find nodes with the right checksum and
       match fingerprints *)
    and findBlock offset checksum entry fingerprint =
      match entry, fingerprint with
      | [], _ ->
          -1
      | (k, cs, fp) :: tl, None
        when cs = checksum ->
          let fingerprint = Digest.substring comprBuf offset blockSize in
          findBlock offset checksum entry (Some fingerprint)
      | (k, cs, fp) :: tl, Some fingerprint
        when (cs = checksum) && (fp = fingerprint) ->
          k
      | _ :: tl, _ ->
          findBlock offset checksum tl fingerprint

    (* Miss : slide the window one character ahead *)
    and miss offset toBeSent length =
      slideWindow (offset + 1) toBeSent length true

    (* Hit : send the data waiting and a BLOCK token, then slide the window
       one block ahead *)
    and hit offset toBeSent length blockNum =
      transmitString toBeSent offset >>= (fun () ->
      let sent = offset in
      let toBeSent = sent + blockSize in
      transmit (BLOCK blockNum) >>= (fun () ->
      slideWindow (offset + blockSize) toBeSent length false))
    in

    (* Initialization and termination *)
    slideWindow comprBufSize comprBufSize comprBufSize false >>= (fun () ->
    flushTokenQueue () >>= (fun () ->
    logMeasures pb;
    Trace.showTimer timer;
    return ()))

end
